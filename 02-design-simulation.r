## ----initialSetup---------------------------------------------------------
source("01-setup.r")

## ----options----------------------------------------------------------------
opt <- list(
  mdls = c("ols", "pcr", "pls", "cppls", "envelope", "bayes"),
  ncomp = 10,
  nsim = 5,
  ntest = 5000,
  nsim_all = 5
)

## ----design----------------------------------------------------------
design_parm <- list(
  n = "50",
  p = "15, 40",
  R2 = "0.5, 0.9",
  relpos = "1, 2; 1, 3; 2, 3; 1, 2, 3",
  gamma = "0.5, 0.9"
)
design_parm <- lapply(design_parm, parseText)

## ----design------------------------------------------------------
design <- expand.grid(design_parm)
design$q <- design$p
design <- as_data_frame(design)

## ----complete-simulation------------------------------------------------------
load_if_not(
  obj_name = "sim_obj",
  obj_path = "robj",
  expression = expression({
    sim_obj <- apply(design, 1, sim_rep, nrep = opt$nsim)
    sim_obj <- bind_obj(sim_obj, name = "sim_obj")
  })
)

