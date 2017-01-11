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
    pred_err <- fit_obj %>%
      group_by_(.dots = c("design", "rep")) %>%
      do({
        out <- map_df(mdls, function(mdl){
          map2_df(.[["sim_obj"]], .[[mdl]], function(x, y){
            get_pred_error(x, y, mdl)
          })
        }, .id = "Model")
      })
    pred_err %>%
      ungroup %>%
      mutate(
        design = as.numeric(design),
        rep = as.numeric(rep),
        Model = mdls[as.numeric(Model)]
      )
  })
)
