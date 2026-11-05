# V33 results
Both models use the same household-filtered sample and fixed initial prior. Proposed: 52 parameters; nested: 38.
Eligible households make >=1 observed purchase in FV, classified cereal, snacks, or beverages over 2008--2017.
All trips of eligible households are retained, including trips and household-years with no focal purchases.
The household filter changes the population for incidence estimation relative to all-household V33;
compare nested and proposed fit only within this filtered sample.
Coefficient tables contain all free parameters, block-Hessian SEs, 95% CIs, z, p and stars.
Stars: * p<.10, ** p<.05, *** p<.01. The fixed initial prior and nested transitions have no SEs.
Latent-block Hessians omit missing-state information; cross-block covariance is NA, not zero.
Posterior profiles are posterior-weighted empirical incidence, conditional composition and category wallet shares.
Profile SEs cluster by household and treat estimated posterior weights as fixed; stars test State 2 minus State 1.
Cereal incidence means classified cereal purchase. Milk and yogurt are excluded from estimation and profiles.
Wallet shares sum all category spending columns, including purchases overridden by outcome priority coding.
BIC uses independent household sequences. The no-switch restriction is on a boundary; no standard chi-square LR p-value is reported.
Read sample_audit.csv for filtering and year gaps. There is no train/test split.
Read estimation logs and state-label diagnostic before interpreting results. Convergence is not proof of a global optimum.
