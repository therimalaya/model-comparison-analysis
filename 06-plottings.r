## ---- Prediction Plots --------------------------
if(all(file.exists(c("robj/pred-error.rds", "robj-bak/pred-error-all.rds")))) {
  source("02-design-simulation.r")
  pred_error <- readRDS("robj/pred-error.rds")
  pred_error_all <- readRDS("robj-bak/pred-error-all.rds")
} else {
  source("04-prediction-error.r")
}

## ---- Average Test Prediction Error --------------------------
test_err <- pred_error[, .(MSEP = mean(test_err)),
                       by = c("Model", "design", "ncomp")]
test_err_all <- pred_error_all[, .(MSEP = mean(test_err)),
                               by = c("Model", "design", "ncomp")]

## ---- Test Error with Label --------------------------
## test_err <- merge(avg_test_err, design_label(design), by = "design")
## test_err_all <- merge(avg_test_err_all,
##                       design_label(design_all,
##                                    var.list = c("p", "R2", "relpos", "gamma")),
##                       by = "design")

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
  geom_text(data = design_label(design, c("p", "R2", "relpos", "gamma")),
            aes(label = label), x = 10, y = 0.9, hjust = 1,
            show.legend = FALSE, family = "mono", color = "black") +
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
  geom_text(data = design_label(design_all, c("p", "R2", "relpos", "gamma")), aes(label = label),
            x = 10, y = 0.9,
            hjust = 1, size = rel(3),
            show.legend = FALSE, family = "mono",
            color = "black") +
  scale_color_brewer(palette = "Set1")

ggsave(PE_plt_all, file = "pdf/prediction-error-all.pdf", width = 10, height = 15)


## ---- Proportionate difference of Prediction Error from PLS --------------------------
err_pls_all <- dcast(test_err_all, design + ncomp ~ Model, value.var = "MSEP")
err_pls_all <- err_pls_all[ncomp != 0, ols := ols[1], by = design]
pls_all <- err_pls_all[, (.SD - .SD[["pls"]])/.SD[["pls"]], .SDcols = 3:6,
    by = .(design, ncomp)] %>%
  melt(1:2, variable.name = "Model", value.name = "MSEP") %>%
  na.omit

## ---- Plotting the proportinate difference --------------------------
plt_pls <-
  ggplot(pls_all, aes(ncomp, MSEP, color = Model)) +
  geom_point(shape = 4, size = 0.8) + geom_line() +
  facet_wrap( ~ design, labeller = label_both, ncol = 4, scales = "free_y") +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Number of Components",
       y = "Proportion difference of\nPrediction Error from PLS model") +
  geom_text(data = design_label(design_all, c("p", "R2", "relpos", "gamma")),
            x = 10, y = 0.85, hjust = 1, size = rel(3),
            aes(label = label), family = "mono", color = "black") +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(-1, 1))
ggsave(filename = "pdf/prediction-error-pls.pdf", plot = plt_pls, width = 10, height = 15)


## ---- MSEP/simga^2 for each design -------------------------
minerror <- sim_obj[, .(minerror = unique(map_dbl(obj, ~.x[["minerror"]]))), by = design]
msep_sigma <- merge(test_err, minerror, by = "design")[, msep_sigma := MSEP/minerror]
label_y <- msep_sigma[, .(label_y = max(msep_sigma) - sd(msep_sigma) / 2), by = design]


## ---- MSEP/sigma^2 plot -------------------------------------
msep_sigma_plt <-
  ggplot(msep_sigma[Model != "ols"],
         aes(ncomp, msep_sigma, color = Model)) +
  geom_line() +
  geom_point(shape = 21, size = 0.8, fill = "black") +
  facet_grid(design ~ ., scales = "free_y", labeller = label_both) +
  labs(y = expression(MSEP/sigma^2), x = "Components") +
  scale_x_continuous(breaks = 0:10) +
  theme(legend.position = "top") +
  geom_text(data = merge(design_label(design, c("p", "R2", "relpos", "gamma")),
                         label_y, by = "design"),
            aes(label = label, y = label_y, x = 10),
            color = "black", family = "mono", size = rel(3), hjust = 1)
ggsave(msep_sigma_plt, filename = "pdf/msep-sigma-plot.pdf",
       width = 4, height = 5)

## ---- MSEP beta --------------------------
load_if_not("avg_beta_err", "robj", source("05-estimation-error.r"))

## ---- Estimation Error Plot --------------------------
lbl_tbl <-
  merge(design_label(design),
        avg_beta_err[, .(label_y = median(est_err) + sd(est_err)/2),
                     by = design], by = "design")
est_error_plt <-
  ggplot(avg_beta_err[Model != "ols"],
         aes(ncomp, est_err, color = Model)) +
  geom_line() + geom_point(shape = 21, fill = "black", size = 0.8) +
  facet_grid(design ~ ., scales = "free_y", labeller = label_both) +
  scale_x_continuous(breaks = 0:10) +
  theme(legend.position = "top") +
  geom_text(data = design_label(design, c("p", "R2", "relpos", "gamma")),
            aes(label = label, x = 0, y = 2.5), hjust = 0,
            family = "mono", color = "black", size = rel(3),
            nudge_y=-0.25) +
  labs(x = "Components", y = expression(MSE(hat(beta)))) +
  coord_cartesian(ylim = c(0, 2.5))
ggsave(est_error_plt, filename = "pdf/est-error-plot.pdf",
       width = 4, height = 5)
