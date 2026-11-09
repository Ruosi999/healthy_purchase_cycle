# V38 results
Both models use the same retained sample and fixed initial prior. Proposed: 60 parameters; nested: 46.
Coefficient tables contain all free parameters, block-Hessian SEs, 95% CIs, z, p and stars.
Stars: * p<.10, ** p<.05, *** p<.01. The fixed initial prior and nested transitions have no SEs.
Latent-block Hessians omit missing-state information; cross-block covariance is NA, not zero.
Posterior profiles are posterior-weighted empirical incidence, conditional composition and category wallet shares.
Profile SEs cluster by household and treat estimated posterior weights as fixed; stars test State 2 minus State 1.
Cereal incidence means classified cereal purchase. Yogurt code 1 means fat-free. Beverage code 1 includes any original milk purchase. No independent milk emission.
Wallet shares are inherited descriptive outputs only; they are NOT HMM outcomes. Beverage spending includes milk exactly once.
11_posterior_healthy_choices.csv is the focused five-category conditional probability table.
beverage_merge_audit.csv counts original and merged codes on the retained sample.
Compare AIC/BIC only between the V38 nested and proposed models, not directly against V30 with different outcomes.
BIC uses independent household sequences. The no-switch restriction is on a boundary; no standard chi-square LR p-value is reported.
Read sample_audit.csv for filtering and year gaps. There is no train/test split.
Read estimation logs and state-label diagnostic before interpreting results. Convergence is not proof of a global optimum.
