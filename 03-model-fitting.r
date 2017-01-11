source("02-design-simulation.r")
 
load_if_not(
  obj_name = "ols_obj",
  obj_path = "robj",
  expression = expression({
    sim_obj %>%
      rowwise() %>%
      do({
        data_frame(
          design = .[["design"]],
          rep = .[["rep"]],
          ols = list(ols_fit(.[["sim_obj"]]))
        )
      })
  })
)

load_if_not(
  obj_name = "pls_obj",
  obj_path = "robj",
  expression = expression({
    sim_obj %>%
      rowwise() %>%
      do({
        data_frame(
          design = .[["design"]],
          rep = .[["rep"]],
          pls = list(mvr_fit(.[["sim_obj"]], mvr_fun = "plsr"))
        )
      })
  })
)

load_if_not(
  obj_name = "env_obj",
  obj_path = "robj",
  expression = expression({
    sim_obj %>%
      rowwise() %>%
      do({
        data_frame(
          design = .[["design"]],
          rep = .[["rep"]],
          envelope = list(env_fit(.[["sim_obj"]]))
        )
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
    bobj <- lapply(1:32, function(dgn) {
      lapply(1:5, function(rep) {
        readRDS(paste0("robj/bayes-obj/b_", dgn, "_", rep, ".rds"))
      })
    })
    bobj <- bind_obj(bobj, "bayes")
  })
)


## Binding Together
load_if_not(
  obj_name = "fit_obj",
  obj_path = "robj",
  expression = expression({
    sim_obj %>%
      left_join(ols_obj) %>%
      left_join(pls_obj) %>%
      left_join(env_obj) %>%
      left_join(bayes_obj)
  })
)
