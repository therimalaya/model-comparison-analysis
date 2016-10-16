source("02-design-simulation.r")
models <- c("linear", "pcr", "plsr", "envelope", "bayes")

fit_obj <- mclapply(unique(sim_obj$design), function(dgn){
    mclapply(unique(sim_obj$rep), function(r){
        obj <- sim_obj[rep == r & design == dgn, obj][[1]]
        get_fitted(obj, "bayes", ncomp = 10)
    })
}, mc.cores = 32)

saveRDS(fit_obj, file = "robj/fit_obj.rds")
