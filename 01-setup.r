## ----PackageLoading-----------------------------------------------------
req.pkgs <- c("tidyverse", "simrel", "pls", "envlp",
              "BayesPLS", "parallel", "grid", "gridExtra")
for (pkg in req.pkgs)
{
  require(pkg, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)
}
rm(pkg, req.pkgs)

## ----FunctionsAndScripts------------------------------------------------
source('function.r')
