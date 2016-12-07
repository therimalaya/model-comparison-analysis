## ----PackageLoading-----------------------------------------------------

req.pkgs <- c("data.table", "simrel", "pls", "envlp",
              "BayesPLS", "parallel", "purrr", "ggplot2",
              "grid", "gridExtra")
for (pkg in req.pkgs) {
  require(pkg, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)
}

rm(pkg, req.pkgs)

## ----FunctionsAndScripts------------------------------------------------
source('function.r')
