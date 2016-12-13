## ---- Convert character into list -----------------------------------------------
parseText <- function(x) {
  evl <- function(x) as.numeric(unlist(strsplit(x, split = ",")))
  out <- lapply(strsplit(x, ";")[[1]], evl)
  if (length(out) > 1) return(out)
  return(out[[1]])
}

## ---- Get Design Label for Plots --------------------------
design_label <- function(design_table, var.list = NULL){
  if (!is.null(var.list))
    design_table <- design_table[, var.list, with = F]
  rownames(design_table) <- NULL
  out <- data.table(t(design_table[, sapply(1:.N, function(x){
    c(design = x, label = paste(capture.output(design_table[x]), collapse = "\n"))
  })]))
  out[, design := as.numeric(design)][]
}

## ---- Simulation Function -----------------------------------------------
sim_rep <- function(design_parm, nrep = 5, min_lambda = 1e-4, ntest = 5000){
  design_parm <- append(
    design_parm,
    c(lambda.min = min_lambda, ntest = ntest)
  )
  sim_obj <- lapply(1:nrep, function(x){
    set.seed(7777)
    do.call(simrel, design_parm)
  })
  return(sim_obj)
}

## ---- Load Object if exist -----------------------------------------------
load_if_not <- function(obj_name, obj_path = ".", expression){
  full_path <- paste(file.path(obj_path, obj_name), "rds", sep = ".")
  if (file.exists(full_path)) {
    assign(obj_name, readRDS(full_path), envir = .GlobalEnv)
  } else {
    assign(obj_name, eval(expression), envir = .GlobalEnv)
    saveRDS(get(obj_name), file = full_path)
  }
}

## ---- Oridinary List Square Fit -----------------------------------------------
ols_fit <- function(sim_obj, ...) {
  if (class(sim_obj) != "simrel") stop("wrong sim_obj")
  train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
  fit <- lm(y ~ x, data = train, ...)
  return(fit)
}

## ---- PCR or PLS model Fit -----------------------------------------------
mvr_fit <- function(sim_obj, mvr_fun = "pcr", ncomp = 10, ...) {
  if (class(sim_obj) != "simrel") stop("wrong sim_obj")
  train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
  fit <- match.fun(mvr_fun)(y ~ x, data = train, ncomp = ncomp, ...)
  return(fit)
}

## ---- Envelope Model Fit -----------------------------------------------
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

## ---- BayesPLS model fit -----------------------------------------------
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

## ---- Bind list of object into data.table -----------------------------------------------
bind_obj <- function(obj){
  dt <- rbindlist(lapply(obj, data.table), idcol = TRUE)
  setnames(dt, c("V1", ".id"), c("obj", "design"))
  dt[, rep := 1:.N, by = design]
  setcolorder(dt, c("design", "rep", "obj"))
  setkeyv(dt, c("design", "rep"))
  return(dt[])
}

## ---- Beta Function Generator for different Models -----------------------------------------------
get_beta <- function(model = c('pcr', 'pls', 'cppls', 'mvr', 'envelope', 'bayes', 'linear',
                               'try-error', "list", "lm", "lmm", "BayesPLS", "ols")){
  model <- match.arg(model)

  if (model %in% c('pcr', 'pls', 'cppls')) model <- 'mvr'
  if (model %in% c('BayesPLS')) model <- "bayes"
  if (model %in% c("lm", "lmm", "ols")) model <- "ols"
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
         ols = {
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
             if("try-error" %in% class(mdl)) return(NA)
             coef <- melt(cbind(0, sapply(mdl, function(obj) {
               if(any(is.na(obj))) return(NA)
               estimate.BayesPLS(
                 obj, start = start, stop = stop
               )$coef
             })))
             names(coef) <- c('var', 'ncomp', 'beta')
             coef$ncomp <- coef$ncomp - 1
             return(coef)
           }
         }
         )
  return(coefs)
}

## ---- Mean Sum of square of Prediction ----------------------------
msep <- function(true, predicted) mean((true - predicted) ^ 2)

## ---- Prediction Error -----------------------------------------------
get_pred_error <- function(sim_obj, fit_obj, model_name){
  require(data.table)
  train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
  test <- data.frame(x = I(sim_obj$TESTX), y = I(sim_obj$TESTY))
  beta_fun <- get_beta(model_name)
  beta_df <- data.table(beta_fun(fit_obj))
  if (any(is.na(fit_obj))) return(data.table())
  pred_err <- beta_df[,.(
    train_err =  msep(c(beta %*% t(train$x)), c(train$y)),
    test_err =  msep(c(beta %*% t(test$x)), c(test$y))
  ), by = ncomp]
  return(pred_err)
}

## ---- Estimation Error -----------------------------------------------
get_beta_error <- function(sim_obj, fit_obj, model_name){
  require(data.table)
  true_beta <- sim_obj$beta
  est_beta <- data.table(get_beta(model_name)(fit_obj))
  beta_err <- est_beta[, msep(true_beta, beta), by = .(ncomp)]
  setnames(beta_err, "V1", "beta_error")
  return(beta_err)
}

## ---- ggplot common legend -----------------
share_legend <- function(..., ncol = length(list(...)), nrow = 1,
                         position = c("bottom", "right")) {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(
    position,
    "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                           legend,
                           ncol = 1,
                           heights = unit.c(unit(1, "npc") - lheight, lheight)),
    "right" = arrangeGrob(do.call(arrangeGrob, gl),
                          legend,
                          ncol = 2,
                          widths = unit.c(unit(1, "npc") - lwidth, lwidth))
  )
  grid.newpage()
  grid.draw(combined)
}
