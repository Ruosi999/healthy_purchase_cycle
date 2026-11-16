"""Step 08 | Aggregate yogurt purchases to trip level by sugar label (2008-2017).

Mirrors the fat-based yogurt trip aggregation (Step 06) so the output can be
merged into the panel the same way.

Input
  healthy_purchase_data/intermediate/yogurt_purchases/yogurt_purchases_YYYY.tsv
  healthy_purchase_data/intermediate/yogurt_sugar_label/yogurt_sugar_label_upc.tsv
Output
  healthy_purchase_data/processed/yogurt_sugar_trip_level/
    yogurt_sugar_trip_level_2008_2017.tsv
    yogurt_sugar_trip_source_audit_2008_2017.tsv
    yogurt_sugar_trip_year_audit_2008_2017.tsv

Conventions (identical to Step 06)
  hhid = household_code, trip_id = trip_code_uc, both kept as text.
  panel_year = year(purchase_date), NOT the source-file year.
  Purchases dated 2008-01-01 .. 2017-12-31 are kept; adjacent source years
  (2007, 2018) are read when available to capture cross-year trips.
  Spending = final_price_paid, missing treated as zero and floored at zero.

Sugar categories (mutually exclusive and exhaustive for every purchase row)
  high_sugar == 1 -> high_sugar_spending
  high_sugar == 0 -> regular_sugar_spending
  A purchase whose UPC-version is not in the label table counts as high sugar
  (project decision: unknown = high sugar). Such rows are counted in the audit.

yogurt_sugar_y
  1 if the trip contains ANY purchase that is not high sugar (even if its net
    spending is zero), 2 for every other yogurt trip.
  Trips without yogurt are absent and should be NA after the panel merge.
  This follows yogurt_y, where 1 marks the healthier option. Set
  Y_EQUALS_1_WHEN to "high" below to flip the convention.

Dependencies: pandas.   Run: python 04_aggregate_yogurt_sugar_trip_level.py
"""

from __future__ import annotations

import csv
import os
from pathlib import Path

import pandas as pd

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
DATA_ROOT = Path(os.environ.get(
    "HPC_DATA_ROOT", "C:/Users/rwen13/Documents/healthy_purchase_data"))
INPUT_DIR = DATA_ROOT / "intermediate" / "yogurt_purchases"
LABEL_FILE = DATA_ROOT / "intermediate" / "yogurt_sugar_label" / "yogurt_sugar_label_upc.tsv"
OUTPUT_DIR = DATA_ROOT / "processed" / "yogurt_sugar_trip_level"
OUTPUT_FILE = OUTPUT_DIR / "yogurt_sugar_trip_level_2008_2017.tsv"
SOURCE_AUDIT_FILE = OUTPUT_DIR / "yogurt_sugar_trip_source_audit_2008_2017.tsv"
YEAR_AUDIT_FILE = OUTPUT_DIR / "yogurt_sugar_trip_year_audit_2008_2017.tsv"

SOURCE_YEARS = range(2007, 2019)
WINDOW_START, WINDOW_END = pd.Timestamp("2008-01-01"), pd.Timestamp("2017-12-31")
REQUIRE_ALL_SOURCE_FILES = False     # True: stop if any 2007-2018 extract is missing
RUN_OVERWRITE = True
Y_EQUALS_1_WHEN = "regular"          # "regular": 1 = any non-high-sugar purchase; "high": 1 = any high-sugar purchase

KEY = ["upc", "upc_ver_uc"]
TRIP_KEYS = ["hhid", "panel_year", "trip_id", "purchase_date"]
OUTPUT_COLS = TRIP_KEYS + ["yogurt_sugar_y", "high_sugar_spending", "regular_sugar_spending"]
PURCHASE_COLS = ["household_code", "trip_code_uc", "purchase_date", "final_price_paid"] + KEY


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def read_tsv(path: Path, usecols: list[str]) -> pd.DataFrame:
    """Read selected columns as text; quotes are literal; blanks stay blank."""
    return pd.read_csv(path, sep="\t", usecols=usecols, dtype=str, quoting=csv.QUOTE_NONE,
                       encoding="utf-8-sig", encoding_errors="replace", keep_default_na=False)


def parse_dates(raw: pd.Series, path: Path) -> pd.Series:
    """Parse the date families accepted by Step 06; fail on anything else.

    ISO dates and timestamps (YYYY-MM-DD..., YYYY/MM/DD...) use their first ten
    characters. Then YYYYMMDD, MM/DD/YYYY and MM/DD/YY are tried in that order.
    """
    text = raw.str.strip()
    text = text.mask(text.isin(["", "NA", "N/A", "NULL"]))
    is_iso = text.str.match(r"^\d{4}[-/]\d{1,2}[-/]\d{1,2}", na=False)
    normalized = text.where(~is_iso, text.str.slice(0, 10).str.replace("/", "-", regex=False))
    dates = pd.to_datetime(normalized.where(is_iso), format="%Y-%m-%d", errors="coerce")
    for date_format in ("%Y%m%d", "%m/%d/%Y", "%m/%d/%y"):
        todo = text.notna() & dates.isna()
        if not todo.any():
            break
        dates = dates.fillna(pd.to_datetime(text.where(todo), format=date_format, errors="coerce"))
    bad = text.notna() & dates.isna()
    if bad.any():
        raise ValueError(f"Unrecognized purchase_date in {path}; example: {text[bad].iloc[0]}")
    return dates


def load_labels() -> pd.DataFrame:
    """Load the UPC-version lookup and check that its key is unique."""
    if not LABEL_FILE.is_file():
        raise FileNotFoundError(f"Run 03_build_yogurt_sugar_labels.py first: {LABEL_FILE}")
    labels = read_tsv(LABEL_FILE, KEY + ["high_sugar"])
    if labels.duplicated(KEY).any():
        raise ValueError("Duplicate UPC-version keys in the sugar label table.")
    if not labels["high_sugar"].isin(["0", "1"]).all():
        raise ValueError("high_sugar must be 0 or 1 in the sugar label table.")
    labels["high_sugar"] = labels["high_sugar"].astype(int)
    return labels


# ---------------------------------------------------------------------------
# One annual extract -> trips
# ---------------------------------------------------------------------------
def aggregate_file(source_year: int, path: Path, labels: pd.DataFrame):
    purchases = read_tsv(path, PURCHASE_COLS)
    input_rows = len(purchases)

    dates = parse_dates(purchases["purchase_date"], path)
    missing_date_rows = int(dates.isna().sum())
    in_window = dates.between(WINDOW_START, WINDOW_END)
    purchases, dates = purchases[in_window].copy(), dates[in_window]
    audit = {"source_year": source_year, "input_rows": input_rows,
             "missing_date_rows": missing_date_rows, "retained_rows": len(purchases),
             "trip_rows": 0, "regular_sugar_trips": 0, "unlabeled_rows": 0,
             "unlabeled_spending": 0.0, "missing_price_rows": 0, "negative_price_rows": 0}
    if purchases.empty:
        return None, audit

    if (purchases["household_code"] == "").any() or (purchases["trip_code_uc"] == "").any():
        raise ValueError(f"Missing household_code/trip_code_uc in: {path}")

    # Identifiers and the year derived from the actual purchase date.
    purchases["hhid"] = purchases["household_code"]
    purchases["trip_id"] = purchases["trip_code_uc"]
    purchases["purchase_date"] = dates.dt.strftime("%Y-%m-%d")
    purchases["panel_year"] = dates.dt.year.astype(int)

    # Net spending: non-numeric text is an error; blank is zero; negatives are floored.
    price_text = purchases["final_price_paid"].str.strip()
    spend = pd.to_numeric(price_text.mask(price_text == ""), errors="coerce")
    if (price_text.ne("") & spend.isna()).any():
        raise ValueError(f"Non-numeric final_price_paid value in: {path}")
    audit["missing_price_rows"] = int(spend.isna().sum())
    audit["negative_price_rows"] = int((spend < 0).sum())
    purchases["spend"] = spend.fillna(0).clip(lower=0)

    # Attach the product label. The lookup key is unique, so rows cannot multiply.
    rows_before = len(purchases)
    purchases = purchases.merge(labels, on=KEY, how="left")
    if len(purchases) != rows_before:
        raise AssertionError("The label join changed the number of purchase rows.")
    unlabeled = purchases["high_sugar"].isna()
    audit["unlabeled_rows"] = int(unlabeled.sum())
    audit["unlabeled_spending"] = float(purchases.loc[unlabeled, "spend"].sum())
    purchases["high_sugar"] = purchases["high_sugar"].fillna(1).astype(int)   # unknown = high sugar

    # Each purchase row contributes to exactly one category.
    purchases["high_piece"] = purchases["spend"].where(purchases["high_sugar"] == 1, 0.0)
    purchases["regular_piece"] = purchases["spend"].where(purchases["high_sugar"] == 0, 0.0)
    if abs(purchases["spend"].sum() - purchases["high_piece"].sum() - purchases["regular_piece"].sum()) > 1e-6:
        raise AssertionError(f"Spending reconciliation failed for source year {source_year}.")
    flag = purchases["high_sugar"] if Y_EQUALS_1_WHEN == "high" else 1 - purchases["high_sugar"]
    purchases["y_flag"] = flag

    # Group by the complete trip key, not by trip_id or source year alone.
    trips = purchases.groupby(TRIP_KEYS, as_index=False).agg(
        any_flag=("y_flag", "max"),
        high_sugar_spending=("high_piece", "sum"),
        regular_sugar_spending=("regular_piece", "sum"))
    trips["yogurt_sugar_y"] = trips.pop("any_flag").map({1: 1, 0: 2}).astype(int)

    audit["trip_rows"] = len(trips)
    audit["regular_sugar_trips"] = int((purchases.groupby(TRIP_KEYS)["high_sugar"].min() == 0).sum())
    return trips, audit


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main() -> None:
    if Y_EQUALS_1_WHEN not in ("regular", "high"):
        raise ValueError("Y_EQUALS_1_WHEN must be 'regular' or 'high'.")
    if OUTPUT_FILE.exists() and not RUN_OVERWRITE:
        raise FileExistsError(f"Output already exists. Set RUN_OVERWRITE = True: {OUTPUT_FILE}")

    paths = {year: INPUT_DIR / f"yogurt_purchases_{year}.tsv" for year in SOURCE_YEARS}
    missing = [p.name for p in paths.values() if not p.is_file()]
    if missing:
        message = ("Missing source extract(s): " + ", ".join(missing) +
                   ". An incomplete source range could omit cross-year purchases.")
        if REQUIRE_ALL_SOURCE_FILES:
            raise FileNotFoundError(message)
        print("[WARNING]", message)
    available = {year: p for year, p in paths.items() if p.is_file()}
    if not available:
        raise FileNotFoundError(f"No yogurt purchase files found in {INPUT_DIR}")

    labels = load_labels()
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    partials, audits = [], []
    for year, path in available.items():
        print(f"Aggregating yogurt source year: {year}")
        trips, audit = aggregate_file(year, path, labels)
        audits.append(audit)
        if trips is not None:
            partials.append(trips)
    if not partials:
        raise ValueError("No yogurt purchases dated 2008-2017 were found.")
    all_trips = pd.concat(partials, ignore_index=True)

    # A trip key that appears in two source files may be a repeated record.
    # Summing would risk double counting, so stop and write the keys for review.
    duplicated = all_trips[all_trips.duplicated(TRIP_KEYS, keep=False)]
    if not duplicated.empty:
        review = OUTPUT_DIR / "yogurt_sugar_duplicate_trip_keys_review.tsv"
        duplicated[TRIP_KEYS].drop_duplicates().to_csv(review, sep="\t", index=False)
        raise ValueError(f"A trip appears in multiple source-year files. Review {review}.")

    if not all_trips["yogurt_sugar_y"].isin([1, 2]).all():
        raise AssertionError("yogurt_sugar_y must be 1 or 2 for every yogurt trip.")
    all_trips = all_trips[OUTPUT_COLS].sort_values(["panel_year", "hhid", "purchase_date", "trip_id"])

    # Write to a temporary file first so a failed run never leaves a partial output.
    temporary = OUTPUT_FILE.with_suffix(".tsv.partial")
    all_trips.to_csv(temporary, sep="\t", index=False, na_rep="")
    temporary.replace(OUTPUT_FILE)

    year_audit = all_trips.groupby("panel_year").agg(
        n_trips=("trip_id", "size"),
        n_y1_trips=("yogurt_sugar_y", lambda s: int((s == 1).sum())),
        n_y2_trips=("yogurt_sugar_y", lambda s: int((s == 2).sum())),
        high_sugar_spending=("high_sugar_spending", "sum"),
        regular_sugar_spending=("regular_sugar_spending", "sum")).reset_index()
    year_audit["high_sugar_share_pct"] = (100 * year_audit["high_sugar_spending"] /
        (year_audit["high_sugar_spending"] + year_audit["regular_sugar_spending"])).round(2)
    pd.DataFrame(audits).to_csv(SOURCE_AUDIT_FILE, sep="\t", index=False)
    year_audit.to_csv(YEAR_AUDIT_FILE, sep="\t", index=False)

    print(f"Finished: {len(all_trips):,} yogurt trips. Output: {OUTPUT_FILE}")
    print(f"yogurt_sugar_y = 1 means: any {'high' if Y_EQUALS_1_WHEN == 'high' else 'non-high'}-sugar yogurt on the trip")
    print(year_audit.to_string(index=False))
    unlabeled = sum(a["unlabeled_rows"] for a in audits)
    print(f"Purchase rows without a product label (counted as high sugar): {unlabeled:,}")


if __name__ == "__main__":
    main()
