source("01-setup.r")
source("02-design-simulation.r")
load_if_not("fit_obj", "robj", expression(source("03-model-fitting.r")))
load_if_not("fit_obj_all", "robj-bak", expression(source("03a-model-fitting-all.r")))

## ---- Estimation error for all the models --------------------------
mdls <- c("pls", "envelope", "ols")
beta_err <- rbindlist(lapply(mdls, function(mdl){
  fit_obj[, mapply(get_beta_error, .SD[["sim_obj"]], .SD[[mdl]], list(model = mdl)),
          by = .(design, rep)]}),
  idcol = TRUE)
setnames(beta_err, names(beta_err), c("Model", "design", "rep", "ncomp", "est_err"))
beta_err[, Model := mdls[Model]]
avg_beta_err <- beta_err[, .(est_err = mean(est_err)), by = .(Model, design, ncomp)]

saveRDS(avg_beta_err, file = "robj/avg_beta_err.rds")

