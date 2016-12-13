## ---- Prediction Plots --------------------------
if(all(file.exists(c("robj/pred-error.rds", "robj-bak/pred-error-all.rds")))) {
  source("02-design-simulation.r")
  pred_error <- readRDS("robj/pred-error.rds")
  pred_error_all <- readRDS("robj-bak/pred-error-all.rds")
} else {
  source("04-prediction-error.r")
}

## ---- Average Test Prediction Error --------------------------
avg_test_err <- pred_error[, .(MSEP = mean(test_err)),
                       by = c("Model", "design", "ncomp")]
avg_test_err_all <- pred_error_all[, .(MSEP = mean(test_err)),
                               by = c("Model", "design", "ncomp")]

## ---- Test Error with Label --------------------------
test_err <- merge(avg_test_err, design_label(design), by = "design")
test_err_all <- merge(avg_test_err_all,
                      design_label(design_all,
                                   var.list = c("p", "R2", "relpos", "gamma")),
                      by = "design")

## ---- Removing design that have ols prediction error larger than 1 --------------------------
## test_err_all <- test_err_all[!design %in% test_err_all[Model == "ols" & MSEP > 1, design]]

## ----  Prediction Plot -----
PE_plt <-
  ggplot(test_err[Model != "ols"],
         aes(ncomp, MSEP, color = Model)) +
  geom_point(shape = 4, size = 0.8) +
  geom_line() +
  facet_grid(. ~ design, label = label_both) +
  theme(legend.position = "top") +
  geom_hline(data = test_err[Model == "ols" & ncomp == 1],
             aes(yintercept = MSEP, color = Model),
             linetype = "dashed") +
  labs(x = "Number of Components",
       y = "Test Prediction Error (MSEP)") +
  scale_x_continuous(breaks = 0:10) +
  geom_text(aes(label = label), x = 10, y = 0.9, hjust = 1,
            show.legend = FALSE, family = "mono",
            color = "black") +
  scale_color_brewer(palette = "Set1")
ggsave(PE_plt, file = "pdf/prediction_error.pdf", width = 9, height = 4)

## ---- Prediction Error Plot for all designs --------------------------
PE_plt_all <-
  ggplot(test_err_all[Model != "ols"],
         aes(ncomp, MSEP, color = Model)) +
  geom_point(shape = 4, size = 0.8) +
  geom_line() +
  facet_wrap(~design, label = label_both,
             ncol = 4, scales = "free_y") +
  theme(legend.position = "top") +
  geom_hline(data = test_err_all[Model == "ols" & ncomp == 1],
             aes(yintercept = MSEP, color = Model),
             linetype = "dashed") +
  labs(x = "Number of Components",
       y = "Test Prediction Error (MSEP)") +
  scale_x_continuous(breaks = 0:10) +
  geom_text(aes(label = label), x = 10, y = 0.9,
            hjust = 1, size = rel(3),
            show.legend = FALSE, family = "mono",
            color = "black") +
  scale_color_brewer(palette = "Set1")
ggsave(PE_plt_all, file = "pdf/prediction-error-all.pdf", width = 10, height = 15)


## ---- Proportionate difference of Prediction Error from PLS --------------------------
test_err_pls_all <-
  dcast(test_err_all, label + design + ncomp ~ Model,
        value.var = "MSEP") %>%
  .[, (.SD - .SD[["pls"]])/.SD[["pls"]],
    .SDcols = 4:7,
    by = .(label, design, ncomp)] %>%
  melt(1:3, variable.name = "Model", value.name = "MSEP") %>%
  na.omit

## ---- Plotting the proportinate difference --------------------------
plt_pls <-
  ggplot(test_err_pls_all[Model != "ols"],
         aes(ncomp, MSEP, color = Model)) +
  geom_point(shape = 4, size = 0.8) + geom_line() +
  facet_wrap( ~ design, labeller = label_both, ncol = 4, scales = "free_y") +
  geom_hline(data = test_err_pls_all[Model == "ols" & ncomp == 1],
             aes(yintercept = MSEP, color = Model)) +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Number of Components",
       y = "Proportion difference of\nPrediction Error from PLS model") +
  geom_text(x = 10, y = 0.85, hjust = 1, size = rel(3),
            aes(label = label), family = "mono", color = "black") +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(-1, 1))
ggsave(filename = "pdf/prediction-error-pls.pdf", plot = plt_pls, width = 10, height = 15)
