source("01-setup.r")
source("02-design-simulation.r")
load_if_not("fit_obj", "robj", expression(source("03-model-fitting.r")))
load_if_not("fit_obj_all", "robj-bak", expression(source("03a-model-fitting-all.r")))

## ---- Code for Prediction Error for different model -----
mdls <- opt$mdls[c(1, 3, 5, 6)]

## ---- Prediction Error for all Design --------------------------
fit_obj_all <- fit_obj_all[lapply(bayes, class) == "try-error", bayes := list(NA)]
pred_error_all <- map_df(mdls, function(mdl){
  fit_obj_all[, map2_df(.SD[["sim_obj_all"]],
                    .SD[[mdl]],
                    ~get_pred_error(.x, .y, mdl)),
          by = c("design", "rep")]
}, .id = "Model")
pred_error_all[, Model := mdls[as.numeric(Model)]]
setkeyv(pred_error_all, c("Model", "design", "ncomp"))

## ---- Prediction Error for selected design  --------------------------
pred_error <- map_df(mdls, function(mdl){
  fit_obj[, map2_df(.SD[["sim_obj"]],
                                  .SD[[mdl]],
                                  ~get_pred_error(.x, .y, mdl)),
          by = c("design", "rep")]
}, .id = "Model")
pred_error[, Model := mdls[as.numeric(Model)]]
setkeyv(pred_error, c("Model", "design", "ncomp"))

## ---- Saving Objects --------------------------
saveRDS(pred_error, file = "robj/pred-error.rds")
saveRDS(pred_error_all, file = "robj-bak/pred-error-all.rds")
