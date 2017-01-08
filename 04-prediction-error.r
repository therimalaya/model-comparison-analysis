source("01-setup.r")
source("02-design-simulation.r")
load_if_not("fit_obj", "robj", expression(source("03-model-fitting.r")))

## ---- Code for Prediction Error for different model -----
mdls <- opt$mdls[c(1, 3, 5, 6)]

## ---- Prediction Error for all Design --------------------------
load_if_not(
  obj_name = "pred_error",
  obj_path = "robj",
  expression = expression({
    pred_error <- map_df(mdls, function(mdl){
      fit_obj[, map2_df(.SD[["sim_obj"]],
                        .SD[[mdl]],
                        ~get_pred_error(.x, .y, mdl)),
              by = c("design", "rep")]
    }, .id = "Model")
    pred_error[, Model := mdls[as.numeric(Model)]]
    setkeyv(pred_error, c("Model", "design", "ncomp"))
  })
)
