source("01-setup.r")
source("02-design-simulation.r")
load_if_not("fit_obj", "robj", expression(source("03-model-fitting.r")))

## ---- Estimation error for all the models --------------------------
mdls <- c("pls", "envelope", "ols", "bayes")
load_if_not(
  obj_name = "est_error",
  obj_path = "robj",
  expression = expression({
    est_err <- fit_obj %>%
      group_by_(.dots = c("design", "rep")) %>%
      do({
        out <- map_df(mdls, function(mdl){
          map2_df(.[["sim_obj"]], .[[mdl]], function(x, y){
            get_beta_error(x, y, mdl)
          })
        }, .id = "Model")
      })
    est_err %>%
      ungroup %>%
      mutate(
        design = as.numeric(design),
        rep = as.numeric(rep),
        Model = mdls[as.numeric(Model)]
      )
  })
)
