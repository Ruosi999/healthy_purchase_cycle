# Matched DID event study

Outcome: annual healthy spending in four focal categories (fresh fruits and vegetables, low-sugar cereal, healthy snacks, and healthy beverages) / annual sum of trip food_spend. Yogurt and milk are excluded from the numerator; the denominator remains total food spending. See analysis_config.json for category inclusion and matching settings.

The six event analyses use first observed events, no focal-event recurrence in the window, never-observed-event controls, complete outcome windows, and pre-event matching only. The analysis estimates dynamic matched changes relative to year -1, in percentage points. The orange vertical line marks year 0. These are adjusted associations; causal interpretation requires additional assumptions.

Pointwise 95% CIs use household-clustered fixed-match sandwich SEs. Control reuse across cohorts is included; propensity estimation and matching uncertainty are not. Pretrend tests are partly constrained by matching on pre-event outcomes. Full-window restrictions select on observation/attrition.

Inspect tables/matching_sample_flow.csv and tables/matching_balance.csv before interpreting figures. Counts are unweighted household counts; no Nielsen projection weights are used. Annual mean shares are unweighted household means, not aggregate dollar market shares.

This folder contains aggregate outputs only. Annual microdata and matched household IDs are saved separately under the private data directory.
