---
output: github_document
---

# caseselectR

Algorithmic case selection for intensive within-case analysis in R.

```r
# install from source
# remotes::install_github("your-org/caseselectR")
library(caseselectR)
data(toy_cross, package = "caseselectR")

typ <- typical_case(toy_cross, Y ~ X | Z1 + Z2)
print(typ)
