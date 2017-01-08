source("02-design-simulation.r")
 
load_if_not(
    obj_name = "ols_obj",
    obj_path = "robj",
    expression = expression({
        ols.fit <- lapply(unique(sim_obj$design), function(dgn){
            lapply(unique(sim_obj$rep), function(r){
                ols_fit(sim_obj[design == dgn & rep == r, obj][[1]])
            })
        })
    })
)

load_if_not(
    obj_name = "pls_obj",
    obj_path = "robj",
    expression = expression({
        pls.fit <- lapply(unique(sim_obj$design), function(dgn){
            lapply(unique(sim_obj$rep), function(r){
                mvr_fit(sim_obj[design == dgn & rep == r, obj][[1]], mvr_fun = "plsr")
            })
        })
    })
)

load_if_not(
    obj_name = "env_obj",
    obj_path = "robj",
    expression = expression({
        envelope.fit <- lapply(unique(sim_obj$design), function(dgn){
            mclapply(unique(sim_obj$rep), function(r){
                env_fit(sim_obj[design == dgn & rep == r, obj][[1]])
            }, mc.cores = 5)
        })
    })
)

## ---- Fitting Bayes Model --------------------------
## for (dgn in 1:1) {
##   cat("\nDesign:", dgn, "\t")
##   for (rp in 1:5) {
##     cat("Replication:", rp, "\n")
##     bobj <- mclapply(1:10, function(comp){
##       cat("\nComponent:", comp, "\n")
##       fit_bayes(dgn, rp, comp)
##     }, mc.cores = 10)
##     saveRDS(bobj, file = paste0("robj/bayes-obj-2/b_", dgn, "_", rp, ".rds"))
##   }
## }

## Combining all bayes objects
load_if_not(
  obj_name = "bayes_obj",
  obj_path = "robj",
  expression = expression({
    lapply(1:32, function(dgn) {
      lapply(1:5, function(rep) {
        readRDS(paste0("robj/bayes-obj/b_", dgn, "_", rep, ".rds"))
      })
    })
  })
)


## Binding Together
load_if_not(
  obj_name = "fit_obj",
  obj_path = "robj",
  expression = expression({
    fit_obj <- sim_obj[
      bind_obj(pls_obj)][
      bind_obj(env_obj)][
      bind_obj(ols_obj)][
      bind_obj(bayes_obj)]

    setnames(fit_obj, names(fit_obj),
             c('design', 'rep', 'sim_obj',
               'pls', 'envelope', 'ols', 'bayes'))
  })
)
