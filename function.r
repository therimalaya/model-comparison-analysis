## ---- Convert character into list -----------------------------------------------
parseText <- function(x)
{
  evl <- function(x) as.numeric(unlist(strsplit(x, split = ",")))
  out <- lapply(strsplit(x, ";")[[1]], evl)
  if (length(out) > 1) return(out)
  return(out[[1]])
}

## ---- Get Design Label for Plots --------------------------
design_label <- function(design_table, var.list = NULL)
{
  require(data.table)
  design_table <- as.data.table(design_table)
  if (!is.null(var.list))
    design_table <- design_table[, var.list, with = F]
  rownames(design_table) <- NULL
  out <- data.table(t(design_table[, sapply(1:.N, function(x){
    c(design = x, label = paste(capture.output(design_table[x]), collapse = "\n"))
  })]))
  out[, design := as.numeric(design)]
  return(out[, Model := NA])
}

## ---- Simulation Function -----------------------------------------------
sim_rep <- function(design_parm, nrep = 5, min_lambda = 1e-4, ntest = 5000)
{
  design_parm <- append(
    design_parm,
    c(lambda.min = min_lambda, ntest = ntest)
  )
  set.seed(7777)
  sim_obj <- lapply(1:nrep, function(x){
    do.call(simrel, design_parm)
  })
  return(sim_obj)
}

## ---- Load Object if exist -----------------------------------------------
load_if_not <- function(obj_name, obj_path = ".", expression)
{
  full_path <- paste(file.path(obj_path, obj_name), "rds", sep = ".")
  if (file.exists(full_path)) {
    assign(obj_name, readRDS(full_path), envir = .GlobalEnv)
  } else {
    assign(obj_name, eval(expression), envir = .GlobalEnv)
    saveRDS(get(obj_name), file = full_path)
  }
}

## ---- Oridinary List Square Fit -----------------------------------------------
ols_fit <- function(sim_obj, ...)
{
  if (class(sim_obj) != "simrel") stop("wrong sim_obj")
  train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
  fit <- lm(y ~ x, data = train, ...)
  return(fit)
}

## ---- PCR or PLS model Fit -----------------------------------------------
mvr_fit <- function(sim_obj, mvr_fun = "pcr", ncomp = 10, ...)
{
  if (class(sim_obj) != "simrel") stop("wrong sim_obj")
  train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
  fit <- match.fun(mvr_fun)(y ~ x, data = train, ncomp = ncomp, ...)
  return(fit)
}

## ---- Envelope Model Fit -----------------------------------------------
env_fit <- function(sim_obj, ncomp = 10, ...)
{
  if (class(sim_obj) != "simrel") stop("wrong sim_obj")
  train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
  fit <- lapply(1:ncomp, function(nc){
    with(train, {
      xenv(x, y, u = nc, ...)
    })
  })
  return(fit)
}

## ---- BayesPLS model fit --------------------------
fit_bayes <- function(d, r, c, sobj = sim_obj[design == d & rep == r, obj][[1]]){
  bobj <- BayesPLS(sobj$Y, sobj$X, ncomp = c, scale = FALSE,
                   dotrace = FALSE, totiter = 50000, freeze = 0.01,
                   compreduce = FALSE, thin = 50, burn = 5000)
  return(bobj)
}

## ---- Bind list of object into data.table -----------------------------------------------
bind_obj <- function(obj, name = "obj")
{
  out <- map_df(obj, function(dgn){
    map_df(dgn, function(rep){
      data_frame(obj = list(rep))
    }, .id = "rep")
  }, .id = "design")
  names(out) <- c("design", "rep", name)
  return(out)
}

## ---- Beta Function Generator for different Models -----------------------------------------------
get_beta <- function(model = c('pcr', 'pls', 'cppls', 'mvr', 'envelope', 'bayes', 'linear',
                               'try-error', "list", "lm", "lmm", "BayesPLS", "ols"))
{
  model <- match.arg(model)
  if (model %in% c('pcr', 'pls', 'cppls')) model <- 'mvr'
  if (model %in% c('BayesPLS')) model <- "bayes"
  if (model %in% c("lm", "lmm", "ols")) model <- "ols"
  if (model %in% c("list")) model <- "envelope"
  if (model %in% c("try-error")) return(model)
  switch(model,
         mvr = {
           coefs <- function(mdl, ncomp = 10)
           {
             coef <- drop(coef(mdl, intercept = TRUE, ncomp = 1:ncomp))
             coef <- unname(cbind(0, coef))
             rownames(coef) <- c("Intercept", 1:(nrow(coef) - 1))
             colnames(coef) <- paste0("Comp", 0:ncomp)
             return(coef)
           }
         },
         envelope = {
           coefs <- function(mdl, ncomp = 10)
           {
             coef <- cbind(0, sapply(mdl, function(obj) c(obj$mu, obj$beta)))
             rownames(coef) <- c("Intercept", 1:(nrow(coef) - 1))
             colnames(coef) <- paste0("Comp", 0:ncomp)
             return(coef)
           }
         },
         ols = {
           coefs <- function(mdl)
           {
             coef <- unname(cbind(0, coef(mdl)))
             rownames(coef) <- c("Intercept", 1:(nrow(coef) - 1))
             colnames(coef) <- paste0("Comp", 0:(ncol(coef) - 1))
             return(coef)
           }
         },
         bayes = {
           coefs <- function(mdl, start = 500, stop = NULL)
           {
             coef <- lapply(mdl, function(obj) {
               if(class(obj) != "BayesPLS") return(NA)
               coef <- estimate.BayesPLS(obj, start = start, stop = stop)
               return(c(coef$intercept, coef$coefficients))
             })
             coef <- cbind(0, do.call(cbind, coef))
             rownames(coef) <- c("Intercept", 1:(nrow(coef) - 1))
             colnames(coef) <- paste0("Comp", 0:(ncol(coef) - 1))
             return(coef)
           }
         })
  return(coefs)
}

## ---- Mean Sum of square of Prediction ----------------------------
msep <- function(true, predicted) mean((true - predicted) ^ 2)

## ---- Prediction Error -----------------------------------------------
get_pred_error <- function(sim_obj, fit_obj, model_name)
{
  train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
  test <- data.frame(x = I(sim_obj$TESTX), y = I(sim_obj$TESTY))
  beta_fun <- get_beta(model_name)
  beta_df <- beta_fun(fit_obj)
  beta_df[1, 1] <- mean(train$y)
  truebeta <- c(sim_obj$beta)
  minerr <- c(sim_obj$minerror)
  sigma <- sim_obj$Rotation %*% sim_obj$Sigma[-1, -1] %*% t(sim_obj$Rotation)
  n <- sim_obj$n
  pe <- apply(beta_df, 2, function(b){
    b0 <- b[1]; beta <- b[-1]
    yhat_train <- b0 + beta %*% t(train$x)
    cbind(train = msep(c(yhat_train), train$y),
          test = (minerr + t(beta - truebeta) %*% sigma %*% (beta - truebeta)) * (n + 1)/n)
  })
  pred_err <- unname(cbind(0:(ncol(pe)-1), t(pe)))
  colnames(pred_err) <- c("ncomp", "train_err", "test_err")
  pred_err <- as.data.frame(pred_err)
  return(pred_err)
}

## ---- Estimation Error -----------------------------------------------
get_beta_error <- function(sim_obj, fit_obj, model_name)
{
  require(data.table)
  true_beta <- c(sim_obj$beta0, sim_obj$beta)
  est_beta <- get_beta(model_name)(fit_obj)
  est_beta[1, 1] <- mean(sim_obj$Y)
  beta_err <- data.table(stack(apply(est_beta, 2, msep, true_beta)))
  setnames(beta_err, names(beta_err), c("beta_error", "ncomp"))
  beta_err[, ncomp := as.numeric(gsub("Comp", "", ncomp))]
  setcolorder(beta_err, 2:1)
  return(beta_err[])
}

## ---- ggplot common legend -----------------
share_legend <- function(..., ncol = length(list(...)), nrow = 1,
                         position = c("bottom", "right"))
{
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
