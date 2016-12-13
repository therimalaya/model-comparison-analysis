source("02-design-simulation.r")

load_if_not(
    obj_name = "ols_obj",
    obj_path = "robj",
    expression = expression({
        ols.fit <- lapply(unique(sim_obj$design), function(dgn){
            lapply(unique(sim_obj$rep), function(rep){
                ols_fit(sim_obj[design == dgn & rep == rep, obj][[1]])
            })
        })
    })
)

load_if_not(
    obj_name = "pls_obj",
    obj_path = "robj",
    expression = expression({
        pls.fit <- lapply(unique(sim_obj$design), function(dgn){
            lapply(unique(sim_obj$rep), function(rep){
                mvr_fit(sim_obj[design == dgn & rep == rep, obj][[1]], mvr_fun = "plsr")
            })
        })
    })
)

load_if_not(
    obj_name = "env_obj",
    obj_path = "robj",
    expression = expression({
        envelope.fit <- lapply(unique(sim_obj$design), function(dgn){
            mclapply(unique(sim_obj$rep), function(rep){
                env_fit(sim_obj[design == dgn & rep == rep, obj][[1]])
            }, mc.cores = 5)
        })
    })
)

load_if_not(
  obj_name = "bayes_obj",
  obj_path = "robj",
  expression = expression({
    bayes.fit <- lapply(unique(sim_obj$design), function(dgn){
      mclapply(unique(sim_obj$rep), function(rep){
        obj <- sim_obj[design == dgn & rep == rep, obj][[1]]
        bayes_fit(obj, ncomp = 10, scale = FALSE,
                  dotrace = FALSE, totiter = 50000, freeze = 0.01,
                  compreduce = FALSE, thin = 50, burn = 5000)
      })
    })    
  })
)
bayes_dt <- bind_obj(bayes_obj)
bayes_dt[, design := as.numeric(gsub("design", "", design))]

## Binding Together
fit_obj <- sim_obj[
  bind_obj(pls_obj)][
    bind_obj(env_obj)][
      bind_obj(ols_obj)][
        bayes_dt]
setnames(fit_obj, names(fit_obj), c('design', 'rep', 'sim_obj', 'pls', 'envelope', 'ols', 'bayes'))

## ---- Saving Fit-Obj --------------------------
saveRDS(fit_obj, file = "robj/fit-obj.rds")
