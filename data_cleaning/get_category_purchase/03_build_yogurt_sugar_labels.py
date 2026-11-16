"""Step 07 | Build a product-level sugar label for every yogurt UPC-version.

One label per (upc, upc_ver_uc), constant across years, derived only from
NielsenIQ text:
  - upc_descr                      (master products.tsv; one file, not annual)
  - flavor / type / style / product (annual products_extra_YYYY.tsv)

Why product-level: the annual attribute files have holes (2010 has no flavor
text at all; 2014 and 2017 omit CTL BR products). Sweetening is a property of
the product, so each attribute is pooled across all years in which it is
observed before the rule is applied.

Definition
  high_sugar = 1  sweetened with sugar, with no non-nutritive sweetener and no
                  Light marker. Products that cannot be classified are also 1
                  (project decision: unknown counts as high sugar).
  high_sugar = 0  plain unsweetened, non-nutritive sweetener only,
                  sugar + non-nutritive (reduced-sugar lines), or an uncoded
                  Light / Lite line.

The sweetener code is the token placed before 'Y' / 'Y/A' / 'NDY' / 'SKYR' in
upc_descr (e.g. 'YP LTNF FT N&S Y ST' -> N&S). Code meanings below are
INFERRED from how the codes line up with brands and Type; they do not come
from a NielsenIQ dictionary.

Input  (local, never the GitHub repository)
  healthy_purchase_data/intermediate/yogurt_purchases/yogurt_purchases_YYYY.tsv
  HMS/Master_Files/Latest/products.tsv
  HMS/YYYY/Annual_Files/products_extra_YYYY.tsv
Output
  healthy_purchase_data/intermediate/yogurt_sugar_label/
    yogurt_sugar_label_upc.tsv          one row per UPC-version (the lookup)
    yogurt_sugar_label_year_audit.tsv   spending shares by year and brand status

Dependencies: pandas.   Run: python 03_build_yogurt_sugar_labels.py
"""

from __future__ import annotations

import csv
import os
import re
from pathlib import Path

import pandas as pd

# ---------------------------------------------------------------------------
# Configuration (same environment overrides as the other pipeline scripts)
# ---------------------------------------------------------------------------
HMS_ROOT = Path(os.environ.get(
    "HPC_HMS_ROOT", "C:/Users/rwen13/Documents/2004-2020_Panel_Data/nielsen_extracts/HMS"))
DATA_ROOT = Path(os.environ.get(
    "HPC_DATA_ROOT", "C:/Users/rwen13/Documents/healthy_purchase_data"))
PURCHASE_DIR = DATA_ROOT / "intermediate" / "yogurt_purchases"
MASTER_FILE = HMS_ROOT / "Master_Files" / "Latest" / "products.tsv"
OUTPUT_DIR = DATA_ROOT / "intermediate" / "yogurt_sugar_label"
LABEL_FILE = OUTPUT_DIR / "yogurt_sugar_label_upc.tsv"
AUDIT_FILE = OUTPUT_DIR / "yogurt_sugar_label_year_audit.tsv"

SOURCE_YEARS = range(2007, 2019)   # adjacent years included, as in the trip aggregator
CHUNK_ROWS = 500_000
KEY = ["upc", "upc_ver_uc"]
ATTRIBUTES = ["flavor_descr", "type_descr", "style_descr", "product_descr"]

# ---------------------------------------------------------------------------
# Rule vocabulary
# ---------------------------------------------------------------------------
# Non-nutritive sweetener only (sucralose, aspartame/NutraSweet, stevia, ace-K ...).
NNS_CODES = {"S", "NS", "STV", "APS", "ANS", "AP&N", "AK&N", "AK", "RBN", "NS&PYD", "AP"}
# Sugar together with a non-nutritive sweetener (reduced-sugar lines).
MIX_CODES = {"S&S", "N&S", "NS&S", "NS&C", "APS&SGR", "APS&S", "C&S", "ECJS", "ECS",
             "SV&S", "RBN&S", "AK&S", "STV&S", "RA&S", "NTM&S", "ANS&S", "A&S"}
# Caloric sweeteners only (cane juice, honey, maple, agave, fruit juice ...).
SUGAR_CODES = {"ECJ", "EJS", "ECJ&S", "ECJ&SGR", "ECJH", "EC&W", "CJC", "HY&S", "HYP&S",
               "NHYP", "HNY", "HY", "HNY&MS", "MS", "AG", "RCC", "FJ", "FJC"}
# Very short codes are accepted only as the LAST token before 'Y', because the
# same letters also occur as ordinary abbreviations elsewhere in a description.
LAST_POSITION_ONLY = {"S", "AP"}
# Tokens that end the "attribute" part of a description and start the flavor part.
STOP_TOKENS = {"Y", "Y/A", "NDY", "SKYR", "D-S", "Y-S"}
# Light markers: Type text, description tokens, or the brand name itself.
LIGHT_TOKENS = {"LT", "LTNF", "LLF", "L&F"}
LIGHT_BRAND_WORDS = ("LIGHT", "LITE", "WEIGHT WATCHERS", "SIMPLY 100", "CARB")
# Flavor values that mean "plain" (Lifeway kefir uses PLN / PLN SMOOTHIE).
PLAIN_FLAVOR = re.compile(r"^(PLAIN|PLN|UNFLAVORED|UNSWEETENED)( SMOOTHIE)?$")
PACK_TOKEN = re.compile(r"^\d+P$")          # multipack marker such as 4P, 12P


# ---------------------------------------------------------------------------
# File helpers
# ---------------------------------------------------------------------------
def read_tsv(path: Path, usecols: list[str], **kwargs):
    """Read selected TSV columns as text.

    NielsenIQ descriptions contain literal quote characters, so quote handling
    is disabled. Identifiers are never converted to numbers, and empty fields
    stay empty strings (not NaN).
    """
    return pd.read_csv(path, sep="\t", usecols=usecols, dtype=str, quoting=csv.QUOTE_NONE,
                       encoding="utf-8-sig", encoding_errors="replace",
                       keep_default_na=False, **kwargs)


def filter_by_upc(path: Path, usecols: list[str], wanted_upcs: set[str]) -> pd.DataFrame:
    """Stream a large TSV and keep only rows whose UPC is in the yogurt set."""
    parts = [chunk[chunk["upc"].isin(wanted_upcs)]
             for chunk in read_tsv(path, usecols, chunksize=CHUNK_ROWS)]
    return pd.concat(parts, ignore_index=True)


def is_control_brand(brand: pd.Series) -> pd.Series:
    """Same CTL BR test as the top-90 audit: compare letters and digits only."""
    return brand.str.upper().str.replace(r"[^A-Z0-9]", "", regex=True).str.match(r"^(CTLBR|CONTROLBR)")


# ---------------------------------------------------------------------------
# Step 1. Which UPC-versions are purchased, and how much is spent on them
# ---------------------------------------------------------------------------
def load_purchase_pairs() -> pd.DataFrame:
    """Return one row per (source_year, upc, upc_ver_uc) with spending and brand."""
    parts = []
    for year in SOURCE_YEARS:
        path = PURCHASE_DIR / f"yogurt_purchases_{year}.tsv"
        if not path.is_file():
            print(f"[SKIP] no purchase extract for {year}")
            continue
        purchases = read_tsv(path, KEY + ["brand_descr", "final_price_paid"])
        # Coupon-adjusted price, missing treated as zero and floored at zero,
        # exactly as in the trip-level aggregation.
        purchases["spend"] = pd.to_numeric(purchases["final_price_paid"], errors="coerce").fillna(0).clip(lower=0)
        pairs = purchases.groupby(KEY, as_index=False).agg(
            spend=("spend", "sum"), brand_descr=("brand_descr", "first"))
        pairs.insert(0, "source_year", year)
        parts.append(pairs)
        print(f"[READ] purchases {year}: {len(pairs):,} UPC-versions")
    if not parts:
        raise FileNotFoundError(f"No yogurt purchase extracts found in {PURCHASE_DIR}")
    return pd.concat(parts, ignore_index=True)


# ---------------------------------------------------------------------------
# Step 2. Pool the annual attributes across years
# ---------------------------------------------------------------------------
def load_annual_attributes(wanted_upcs: set[str], wanted_keys: set[tuple]) -> pd.DataFrame:
    """Stack the yogurt rows of every available products_extra file."""
    parts = []
    for year in SOURCE_YEARS:
        path = HMS_ROOT / str(year) / "Annual_Files" / f"products_extra_{year}.tsv"
        if not path.is_file():
            continue
        extra = filter_by_upc(path, KEY + ["flavor_code"] + ATTRIBUTES, wanted_upcs)
        extra = extra[[key in wanted_keys for key in zip(extra["upc"], extra["upc_ver_uc"])]].copy()
        for column in ATTRIBUTES + ["flavor_code"]:
            extra[column] = extra[column].str.upper().str.strip()
        extra.insert(0, "source_year", year)
        parts.append(extra)
        print(f"[READ] products_extra {year}: {len(extra):,} yogurt rows")
    return pd.concat(parts, ignore_index=True)


def fill_flavor_from_code(attributes: pd.DataFrame) -> pd.DataFrame:
    """Recover blank flavor text from flavor_code where the code is present.

    products_extra_2010 has an empty flavor_descr column for every product.
    The code -> text mapping is learned from the years in which both exist.
    """
    known = attributes[(attributes["flavor_code"] != "") & (attributes["flavor_descr"] != "")]
    code_map = (known.groupby(["flavor_code", "flavor_descr"]).size().reset_index(name="n")
                     .sort_values("n", ascending=False).drop_duplicates("flavor_code")
                     .set_index("flavor_code")["flavor_descr"])
    blank = (attributes["flavor_descr"] == "") & attributes["flavor_code"].isin(code_map.index)
    attributes.loc[blank, "flavor_descr"] = attributes.loc[blank, "flavor_code"].map(code_map)
    print(f"[FILL] flavor text recovered from flavor_code: {int(blank.sum()):,} rows")
    return attributes


def pool_attribute(attributes: pd.DataFrame, column: str) -> pd.DataFrame:
    """Choose one value per UPC-version: the most frequent non-blank value
    across years; ties go to the most recent year. Also count distinct values
    so that products whose attribute changed over time can be audited."""
    observed = attributes[attributes[column] != ""]
    counts = observed.groupby(KEY + [column]).agg(
        n_years=("source_year", "size"), last_year=("source_year", "max")).reset_index()
    distinct = counts.groupby(KEY).size().rename(f"{column}_n_values").reset_index()
    best = counts.sort_values(["n_years", "last_year"], ascending=False).drop_duplicates(KEY)
    return best[KEY + [column]].merge(distinct, on=KEY)


# ---------------------------------------------------------------------------
# Step 3. The rule
# ---------------------------------------------------------------------------
def split_description(description: str) -> tuple[list[str], list[str]]:
    """Split upc_descr into the tokens before and after the first stop token.

    'YP LTNF FT N&S Y ST 4P' -> (['YP','LTNF','FT','N&S'], ['ST','4P'])
    """
    tokens = description.split()
    for position, token in enumerate(tokens):
        if token in STOP_TOKENS:
            return tokens[:position], tokens[position + 1:]
    return tokens, []


def sweetener_code(description: str) -> tuple[str, str]:
    """Return (kind, code). Kind is MIX, NNS, SUG, UNS or '' (no code found).

    Mixed codes are checked first: a product that carries both a sugar code
    and a non-nutritive code is a reduced-sugar product.
    """
    before, _ = split_description(description)
    for codes, kind in ((MIX_CODES, "MIX"), (NNS_CODES, "NNS"), (SUGAR_CODES, "SUG")):
        for token in before:
            if token in codes and not (token in LAST_POSITION_ONLY and before[-1] != token):
                return kind, token
    return ("UNS", "UNS") if "UNS" in before else ("", "")


def description_flavor(description: str) -> str:
    """Flavor part of upc_descr with drink and multipack markers removed.

    Used only when the Flavor attribute is missing: 'CTL BR NF Y PLN' -> 'PLN'.
    """
    _, after = split_description(description)
    return " ".join(t for t in after if t != "DR" and not PACK_TOKEN.match(t))


def classify(row) -> tuple[int, str]:
    """Apply the rules in order; the first match decides. Returns (high_sugar, rule)."""
    flavor, kind = row.flavor_descr, row.sw_kind
    fallback_flavor = description_flavor(row.upc_descr) if flavor == "" else ""
    is_light = ("LIGHT" in row.type_descr or "LITE" in row.type_descr
                or any(word in row.brand_descr for word in LIGHT_BRAND_WORDS)
                or bool(LIGHT_TOKENS & set(split_description(row.upc_descr)[0])))

    # R8: nothing to read at all. Project decision: unknown counts as high sugar.
    if row.upc_descr == "" and flavor == "" and row.type_descr == "":
        return 1, "R8 no text (unknown -> high)"
    # R2: plain yogurt sold with a sweetened side cup (fruit, honey, granola).
    if (flavor.startswith(("PLAIN", "PLN")) and "/" in flavor) or fallback_flavor.startswith(("PLN/", "PLAIN/")):
        return 1, "R2 plain + sweetened side cup"
    # R1: plain, by the Flavor attribute, or by the description when Flavor is missing.
    if PLAIN_FLAVOR.match(flavor) or (flavor == "" and (kind == "UNS" or PLAIN_FLAVOR.match(fallback_flavor))):
        return 0, "R1 plain unsweetened"
    if kind == "NNS":
        return 0, "R3 non-nutritive only"
    if kind == "MIX":
        return 0, "R4 sugar + non-nutritive"
    # An explicit sugar code outranks a Light marker.
    if kind == "SUG":
        return 1, "R6 explicit sugar code"
    if is_light:
        return 0, "R5 Light line, no code"
    # R7: flavored and nothing says otherwise.
    return 1, "R7 flavored, no marker (default)"


def text_source(row) -> str:
    """How much text the label rests on; use it to build robustness samples."""
    has_description = row.upc_descr != ""
    has_attributes = row.flavor_descr != "" or row.type_descr != ""
    if has_description and has_attributes:
        return "full"
    if has_description:
        return "description only"
    if has_attributes:
        return "attributes only (no sweetener code readable)"
    return "none"


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    # 1. Purchased UPC-versions.
    pairs_by_year = load_purchase_pairs()
    products = pairs_by_year.groupby(KEY, as_index=False).agg(
        total_spend=("spend", "sum"), brand_descr=("brand_descr", "first"))
    wanted_upcs = set(products["upc"])
    wanted_keys = set(zip(products["upc"], products["upc_ver_uc"]))
    print(f"[INFO] distinct yogurt UPC-versions purchased: {len(products):,}")

    # 2. Description from the master (single file; cannot be pooled over years).
    master = filter_by_upc(MASTER_FILE, KEY + ["upc_descr"], wanted_upcs).drop_duplicates(KEY)
    products = products.merge(master, on=KEY, how="left")

    # 3. Attributes pooled across years.
    attributes = fill_flavor_from_code(load_annual_attributes(wanted_upcs, wanted_keys))
    for column in ATTRIBUTES:
        products = products.merge(pool_attribute(attributes, column), on=KEY, how="left")
    if len(products) != len(wanted_keys):
        raise AssertionError("A join multiplied or dropped UPC-version rows.")

    text_columns = ["upc_descr", "brand_descr"] + ATTRIBUTES
    for column in text_columns:
        products[column] = products[column].fillna("").str.upper().str.strip()
    products["brand_status"] = is_control_brand(products["brand_descr"]).map({True: "CTLBR", False: "National"})

    # 4. Label.
    code = products["upc_descr"].map(sweetener_code)
    products["sw_kind"], products["sw_code"] = code.str[0], code.str[1]
    result = products.apply(classify, axis=1)
    products["high_sugar"], products["rule"] = result.str[0], result.str[1]
    products["text_source"] = products.apply(text_source, axis=1)
    changed = [f"{c}_n_values" for c in ("flavor_descr", "type_descr")]
    products["attribute_changed_over_years"] = (products[changed].fillna(1) > 1).any(axis=1).astype(int)

    # 5. Save the lookup. Identifiers stay text.
    columns = KEY + ["high_sugar", "rule", "sw_code", "text_source", "brand_status",
                     "attribute_changed_over_years", "brand_descr", "upc_descr"] + ATTRIBUTES + ["total_spend"]
    products.sort_values("total_spend", ascending=False)[columns].to_csv(
        LABEL_FILE, sep="\t", index=False, quoting=csv.QUOTE_NONE, escapechar="\\")
    print(f"[SAVED] {LABEL_FILE}  ({len(products):,} UPC-versions)")

    # 6. Audit by year and brand status (spending-weighted).
    yearly = pairs_by_year.merge(products[KEY + ["high_sugar", "rule", "text_source", "brand_status"]], on=KEY)
    yearly["high_spend"] = yearly["spend"] * yearly["high_sugar"]
    for source in ("full", "description only", "attributes only (no sweetener code readable)", "none"):
        yearly[f"spend_{source.split(' (')[0].replace(' ', '_')}"] = yearly["spend"] * (yearly["text_source"] == source)
    audit = yearly.groupby(["source_year", "brand_status"]).sum(numeric_only=True).drop(columns="high_sugar")
    shares = audit.drop(columns="spend").div(audit["spend"], axis=0).mul(100).round(2).add_suffix("_pct")
    audit = audit[["spend"]].join(shares).reset_index()
    audit.to_csv(AUDIT_FILE, sep="\t", index=False)

    pd.set_option("display.width", 250); pd.set_option("display.max_rows", 200)
    print("\n=== Share of spending by year and brand status (%) ===")
    print(audit.to_string(index=False))
    total = products["total_spend"].sum()
    print("\n=== Rule mix, all years (% of spending) ===")
    print((100 * products.groupby("rule")["total_spend"].sum() / total).round(2).to_string())
    print("\n=== Text source, all years (% of spending) ===")
    print((100 * products.groupby(["brand_status", "text_source"])["total_spend"].sum() / total).round(2).to_string())
    print(f"\nUPC-versions whose flavor or type differs across years: "
          f"{int(products['attribute_changed_over_years'].sum()):,} "
          f"({100 * products.loc[products['attribute_changed_over_years'] == 1, 'total_spend'].sum() / total:.2f}% of spending)")


if __name__ == "__main__":
    main()
