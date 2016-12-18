source("02-design-simulation.r")

load_if_not(
    obj_name = "ols_obj_all",
    obj_path = "robj-bak",
    expression = expression({
        ols.fit <- lapply(unique(sim_obj_all$design), function(dgn){
            lapply(unique(sim_obj_all$rep), function(rep){
                ols_fit(sim_obj_all[design == dgn & rep == rep, obj][[1]])
            })
        })
    })
)

load_if_not(
    obj_name = "pls_obj_all",
    obj_path = "robj-bak",
    expression = expression({
        pls.fit <- lapply(unique(sim_obj_all$design), function(dgn){
            lapply(unique(sim_obj_all$rep), function(rep){
                mvr_fit(sim_obj_all[design == dgn & rep == rep, obj][[1]], mvr_fun = "plsr")
            })
        })
    })
)

load_if_not(
    obj_name = "env_obj_all",
    obj_path = "robj-bak",
    expression = expression({
        envelope.fit <- lapply(unique(sim_obj_all$design), function(dgn){
            mclapply(unique(sim_obj_all$rep), function(rep){
                env_fit(sim_obj_all[design == dgn & rep == rep, obj][[1]])
            }, mc.cores = 5)
        })
    })
)

load_if_not(
  obj_name = "bayes_obj_all",
  obj_path = "robj-bak",
  expression = expression({
    bayes.fit <- lapply(unique(sim_obj_all$design), function(dgn){
      mclapply(unique(sim_obj_all$rep), function(rep){
        obj <- sim_obj_all[design == dgn & rep == rep, obj][[1]]
        bayes_fit(obj, ncomp = 10, scale = FALSE,
                  dotrace = FALSE, totiter = 50000, freeze = 0.01,
                  compreduce = FALSE, thin = 50, burn = 5000)
      }, mc.cores = 10)
    })    
  })
)
bayes_dt_all <- bind_obj(bayes_obj_all)
bayes_dt_all[, design := as.numeric(gsub("design", "", design))]

## Binding Together
fit_obj_all <- sim_obj_all[
  bind_obj(pls_obj_all)][
      bind_obj(env_obj_all)][
        bind_obj(ols_obj_all)][
          bayes_dt_all]
setnames(fit_obj_all, names(fit_obj_all),
         c('design', 'rep', 'sim_obj_all',
           'pls', 'envelope', 'ols',
           'bayes'
           ))

## ---- Saving Fit-All-Obj --------------------------
saveRDS(fit_obj_all, file = "robj-bak/fit-obj-all.rds")
