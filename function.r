parseText <- function(x) {
    evl <- function(x) as.numeric(unlist(strsplit(x, split = ",")))
    out <- lapply(strsplit(x, ";")[[1]], evl)
    if (length(out) > 1) return(out)
    return(out[[1]])
}

sim_rep <- function(design_parm, nrep = 5, min_lambda = 1e-4){
    design_parm <- append(
        design_parm,
        c(lambda.min = min_lambda)
    )
    sim_obj <- lapply(1:nrep, function(x){
        set.seed(7777)
        do.call(simrel, design_parm)
    })
    return(sim_obj)
}

load_if_not <- function(obj_name, obj_path = ".", expression){
    full_path <- paste(file.path(obj_path, obj_name), "rds", sep = ".")
    if (file.exists(full_path)) {
        assign(obj_name, readRDS(full_path), envir = .GlobalEnv)
    } else {
        assign(obj_name, eval(expression), envir = .GlobalEnv)
        saveRDS(get(obj_name), file = full_path)
    }
}

linear_fit <- function(sim_obj, ...) {
    if (class(sim_obj) != "simrel") stop("wrong sim_obj")
    train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
    fit <- lm(y ~ x, data = train, ...)
    return(fit)
}

mvr_fit <- function(sim_obj, mvr_fun = "pcr", ncomp = 10, ...) {
    if (class(sim_obj) != "simrel") stop("wrong sim_obj")
    train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
    fit <- match.fun(mvr_fun)(y ~ x, data = train, ncomp = ncomp, ...)
    return(fit)
}

env_fit <- function(sim_obj, ncomp = 10, ...){
    if (class(sim_obj) != "simrel") stop("wrong sim_obj")
    train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
    fit <- lapply(1:ncomp, function(nc){
        with(train, {
            xenv(x, y, u = nc, ...)
        })
    })
    return(fit)
}

bayes_fit <- function(sim_obj, ncomp = 2, ...){
    if (class(sim_obj) != "simrel") stop("wrong sim_obj")
    train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
    fit <- list()
    for(nc in 1:ncomp){
        fit[[nc]] <- with(train, {
            BayesPLS(y, x, ncomp = ncomp, ...)
        })
    }
    return(fit)
}

fit_model <- function(sim_obj, method = c(), ...){
    if (class(sim_obj) != "simrel") stop("wrong sim_obj")
    method <- match.arg(method, c("linear", "pcr", "plsr", "envelope", "bayes"))
    res <- switch(method,
           linear = linear_fit(sim_obj, ...),
           pcr = mvr_fit(sim_obj, mvr_fun = "pcr", ...),
           plsr = mvr_fit(sim_obj, mvr_fun = "plsr", ...),
           envelope = env_fit(sim_obj, ...),
           bayes = bayes_fit(sim_obj, ...))
    return(res)
}

get_fitted <- function(sim_obj, mdl, ncomp = 10, ...){
    if (class(sim_obj) != "simrel") stop("wrong sim_obj")
    if (mdl == "linear")
        fit_model(sim_obj, mdl, ...)
    else if (mdl != "bayes")
        fit_model(sim_obj, mdl, ncomp = ncomp, ...)
    else
        fit_model(sim_obj, mdl, ncomp = ncomp, scale = FALSE,
                      dotrace = FALSE, totiter = 50000, freeze = 0.01,
                      compreduce = FALSE, thin = 50, burn = 5000, ...)
}

get_beta <- function(model = c('pcr', 'pls', 'mvr', 'envelope', 'bayes', 'linear',
                              'try-error', "list", "lm", "lmm", "BayesPLS")){
  model <- match.arg(model)
  ## Start Making some Expression
  if (model %in% c('pcr', 'pls')) model <- 'mvr'
  if (model %in% c('BayesPLS')) model <- "bayes"
  if (model %in% c("lm", "lmm")) model <- "linear"
  if (model %in% c("list")) model <- "envelope"
  if (model %in% c("try-error")) return(model)
  switch(model,
         mvr = {
           coefs <- function(mdl, ncomp = 10){
             coef <- cbind(`0 comps` = 0, drop(mdl$coefficients)[, 1:ncomp])
             coef <- melt(coef)
             coef <- within(coef, Var2 <- as.numeric(gsub(' comps', '', Var2)))
             names(coef) <- c('var', 'ncomp', 'beta')
             return(coef)
           }
         },
         envelope = {
           coefs <- function(mdl, ncomp = ncomp){
             coef <- melt(cbind(0, sapply(mdl, function(obj) obj$beta)))
             names(coef) <- c('var', 'ncomp', 'beta')
             coef$ncomp <- coef$ncomp - 1
             return(coef)
           }
         },
         linear = {
           coefs <- function(mdl){
             coef <- cbind(0, coef(mdl)[-1])
             coef <- melt(coef)
             names(coef) <- c('var', 'ncomp', 'beta')
             coef$ncomp <- coef$ncomp - 1
             coef$var <- as.numeric(gsub("x", "", coef$var))
             return(coef)
           }
         },
         bayes = {
           coefs <- function(mdl, start = 500, stop = NULL){
             coef <- melt(cbind(0, sapply(mdl, function(obj) {
               estimate.BayesPLS(
                 obj, start = start, stop = stop
               )$coef
             })))
             names(coef) <- c('var', 'ncomp', 'beta')
             coef$ncomp <- coef$ncomp - 1
             # coef$ncomp[coef$ncomp == 1] <- dim(mdl$gamma$solu)[2]
             return(coef)
           }
         }
  )
  return(coefs)
}

