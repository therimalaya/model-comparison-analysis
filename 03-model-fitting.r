source("02-design-simulation.r")

## load_if_not(
##     obj_name = "ols_obj",
##     obj_path = "robj",
##     expression = expression({
##         ols.fit <- lapply(unique(sim_obj$design), function(dgn){
##             lapply(unique(sim_obj$rep), function(rep){
##                 ols_fit(sim_obj[design == dgn & rep == rep, obj][[1]])
##             })
##         })
##     })
## )

## load_if_not(
##     obj_name = "pcr_obj",
##     obj_path = "robj",
##     expression = expression({
##         pcr.fit <- lapply(unique(sim_obj$design), function(dgn){
##             lapply(unique(sim_obj$rep), function(rep){
##                 mvr_fit(sim_obj[design == dgn & rep == rep, obj][[1]], mvr_fun = "pcr")
##             })
##         })
##     })
## )


## load_if_not(
##     obj_name = "pls_obj",
##     obj_path = "robj",
##     expression = expression({
##         pls.fit <- lapply(unique(sim_obj$design), function(dgn){
##             lapply(unique(sim_obj$rep), function(rep){
##                 mvr_fit(sim_obj[design == dgn & rep == rep, obj][[1]], mvr_fun = "plsr")
##             })
##         })
##     })
## )

## load_if_not(
##     obj_name = "env_obj",
##     obj_path = "robj",
##     expression = expression({
##         envelope.fit <- lapply(unique(sim_obj$design), function(dgn){
##             mclapply(unique(sim_obj$rep), function(rep){
##                 env_fit(sim_obj[design == dgn & rep == rep, obj][[1]])
##             }, mc.cores = 5)
##         })
##     })
## )

which_design <- c(16L, 17L, 18L, 22L, 25L, 26L, 28L, 29L, 30L)
bayes_fit_2 <- lapply(which_design, function(dgn){
  lapply(unique(sim_obj$rep), function(rep){
    obj <- sim_obj[design == dgn & rep == rep, obj][[1]]
    bayes_fit(obj, ncomp = 10, scale = FALSE,
              dotrace = FALSE, totiter = 50000, freeze = 0.01,
              compreduce = FALSE, thin = 50, burn = 5000)
  })
})

## names(bayes_fit_2) <- paste0("design", which_design)
## for (dgn in seq_along(which_design)) names(bayes_fit_2[[dgn]]) <- paste0("rep", 1:5)
