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

## PCR Skipped as per suggestion ----
## load_if_not(
##     obj_name = "pcr_obj_all",
##     obj_path = "robj-bak",
##     expression = expression({
##         pcr.fit <- lapply(unique(sim_obj_all$design), function(dgn){
##             lapply(unique(sim_obj_all$rep), function(rep){
##                 mvr_fit(sim_obj_all[design == dgn & rep == rep, obj][[1]], mvr_fun = "pcr")
##             })
##         })
##     })
## )

## CPPLS skipped as it is not very applicable for single response ----
## load_if_not(
##   obj_name = "cppls_obj_all",
##   obj_path = "robj-bak",
##   expression = expression({
##     cppls.fit <- lapply(unique(sim_obj_all$design), function(dgn){
##       lapply(unique(sim_obj_all$rep), function(rep){
##         mvr_fit(sim_obj_all[design == dgn & rep == rep, obj][[1]], mvr_fun = "cppls", trunc.pow = TRUE)
##       })
##     })
##   })
## )

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

## which_design <- c(16L, 17L, 18L, 22L, 25L, 26L, 28L, 29L, 30L)
## bayes_fit_2 <- lapply(which_design, function(dgn){
##   lapply(unique(sim_obj_all$rep), function(rep){
##     obj <- sim_obj_all[design == dgn & rep == rep, obj][[1]]
##     bayes_fit(obj, ncomp = 10, scale = FALSE,
##               dotrace = FALSE, totiter = 50000, freeze = 0.01,
##               compreduce = FALSE, thin = 50, burn = 5000)
##   })
## })

## which_design <- c(17L, 18L, 22L, 26L, 29L, 30L)
## bayes_fit_3 <- lapply(which_design, function(dgn){
##   lapply(unique(sim_obj_all$rep), function(rep){
##     obj <- sim_obj_all[design == dgn & rep == rep, obj][[1]]
##     bayes_fit(obj, ncomp = 10, scale = FALSE,
##               dotrace = FALSE, totiter = 50000, freeze = 0.01,
##               compreduce = FALSE, thin = 50, burn = 5000)
##   })
## })

## names(bayes_fit_3) <- paste0("design", which_design)
## for (dgn in seq_along(which_design)) names(bayes_fit_3[[dgn]]) <- paste0("rep", 1:5)

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
      })
    })    
  })
)
bayes_dt <- bind_obj(bayes_obj_all)
bayes_dt[, design := as.numeric(gsub("design", "", design))]

## Binding Together
fit_obj_all <- sim_obj_all[
  bind_obj(pls_obj_all)][
      bind_obj(env_obj_all)][
        bind_obj(ols_obj_all)][
          bayes_dt]
setnames(fit_obj_all, names(fit_obj_all),
         c('design', 'rep', 'sim_obj_all', 'pls', 'envelope', 'ols', 'bayes'))

