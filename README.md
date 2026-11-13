# Reliability of hand-coded FoR2020-to-FoR2008 crosswalk decisions (ARC projects)

Part of this project whether Australia's research-security measures (UFIT Guidelines, 2019) reduced research collaboration with China. Field sensitivity scores are defined on FoR2008 fields, so 4,658 projects coded in FoR2020 must be mapped back. For 487 of them the mapping is not unique, and two coders assigned them independently. This repository measures how reliable those decisions are.

**Run:** Open `arc_coding_agreement.Rproj`, then `source()` the scripts in `R/` in order (00 → 03).
Requires R ≥ 4.1, tidyverse and readxl.

**Result (n = 485):** agreement 76.9%; Cohen's kappa 0.76 (95% CI 0.72–0.80); adjusted kappa, which allows for each project having only 2–6 candidate groups, 0.60 (0.53–0.66).
Agreement is 88% when both coders were highly confident and 61% otherwise.
See `output/` for tables and the figure.
