## ---- Prediction Plots --------------------------
load_if_not("pred_error", "robj", expression(source("04-prediction-error.r")))

## ---- Test Prediction Error --------------------------
test_err <- pred_error %>%
  select(-train_err) %>%
  rename(msep = test_err)

## ---- Average Test Prediction Error --------------------------
avg_test_err <- test_err %>%
  group_by(design, Model, ncomp) %>%
  summarize(MSEP = mean(msep, na.rm = TRUE),
            STD_PE = sd(msep, na.rm = TRUE))

## ---- Prediction Error Plot for all designs --------------------------
SE_PE_plt <-
  ggplot(filter(avg_test_err, Model != "ols"),
         aes(ncomp, STD_PE, color = Model)) +
  geom_point(shape = 4, size = 0.8, na.rm = TRUE) +
  geom_line() +
  facet_wrap(~design, label = label_both,
             nrow = 4, scales = "free_y") +
  theme(legend.position = "top") +
  geom_hline(data = filter(avg_test_err, Model == "ols", ncomp != 0),
             aes(yintercept = STD_PE, color = Model),
             linetype = "dashed") +
  labs(x = "Number of Components",
       y = "Std.Deviation of Prediction Error between replicated calibration sets") +
  scale_x_continuous(breaks = 0:10) +
  geom_text(data = design_label(design, c("p", "R2", "relpos", "gamma")),
            aes(label = label),
            x = 10, y = 0.9,
            hjust = 1, size = rel(3),
            show.legend = FALSE, family = "mono",
            color = "black") +
  scale_color_brewer(palette = "Set1")
ggsave(SE_PE_plt, file = "pdf/prediction-error-sd.pdf", width = 15, height = 10)

PE_plt <-
  ggplot(filter(avg_test_err, Model != "ols"),
         aes(ncomp, MSEP, color = Model)) +
  geom_point(shape = 4, size = 0.8, na.rm = TRUE) +
  geom_line() +
  facet_wrap(~design, label = label_both,
             nrow = 4, scales = "free_y") +
  theme(legend.position = "top") +
  geom_hline(data = filter(avg_test_err, Model == "ols", ncomp != 0),
             aes(yintercept = MSEP, color = Model),
             linetype = "dashed") +
  labs(x = "Number of Components",
       y = "Test Prediction Error (MSEP)") +
  scale_x_continuous(breaks = 0:10) +
  geom_text(data = design_label(design, c("p", "R2", "relpos", "gamma")),
            aes(label = label),
            x = 10, y = 0.9,
            hjust = 1, size = rel(3),
            show.legend = FALSE, family = "mono",
            color = "black") +
  scale_color_brewer(palette = "Set1")
ggsave(PE_plt, file = "pdf/prediction-error.pdf", width = 15, height = 10)


## ---- Proportionate difference of Prediction Error from PLS --------------------------
err_pls <- test_err %>%
  spread(key = Model, value = msep) %>%
  mutate(ols = zoo::na.locf(ols)) %>%
  mutate(
    bayes = (bayes - pls)/pls,
    envelope = (envelope - pls)/pls,
    ols = (ols - pls)/pls,
    pls = (pls - pls)/pls
  ) %>%
  gather(Model, MSEP, -design:-ncomp)

## ---- Average proportionate difference of prediction error from PLS --------------
avg_err_pls <- err_pls %>%
  group_by(Model, design, ncomp) %>%
  summarize(MSEP = mean(MSEP, na.rm = TRUE))

## ---- Plotting the proportinate difference --------------------------
plt_pls <-
  ggplot(avg_err_pls, aes(ncomp, MSEP, color = Model)) +
  geom_point(shape = 4, size = 0.8, na.rm = TRUE) + geom_line() +
  facet_wrap( ~ design, labeller = label_both, ncol = 8, scales = "free_y") +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Number of Components",
       y = "Proportion difference of\nPrediction Error from PLS model") +
  geom_text(data = design_label(design, c("p", "R2", "relpos", "gamma")),
            x = 10, y = 0.85, hjust = 1, size = rel(3),
            aes(label = label), family = "mono", color = "black") +
  scale_color_brewer(palette = "Set1")
ggsave(filename = "pdf/prediction-error-pls.pdf", plot = plt_pls, width = 15, height = 10)

## ---- NEED TO BE CHANGED AFTER THIS POSITION --------------------------
## ---- MSEP/simga^2 for each design -------------------------
## minerror <- sim_obj[, .(minerror = unique(map_dbl(obj, ~.x[["minerror"]]))), by = design]
## msep_sigma <- merge(test_err, minerror, by = "design")[, msep_sigma := MSEP/minerror]
## label_y <- msep_sigma[, .(label_y = max(msep_sigma) - sd(msep_sigma) / 2), by = design]


## ## ---- MSEP/sigma^2 plot -------------------------------------
## msep_sigma_plt <-
##   ggplot(msep_sigma[Model != "ols"],
##          aes(ncomp, msep_sigma, color = Model)) +
##   geom_line() +
##   geom_point(shape = 21, size = 0.8, fill = "black") +
##   facet_grid(design ~ ., scales = "free_y", labeller = label_both) +
##   labs(y = expression(MSEP/sigma^2), x = "Components") +
##   scale_x_continuous(breaks = 0:10) +
##   theme(legend.position = "top") +
##   geom_text(data = merge(design_label(design, c("p", "R2", "relpos", "gamma")),
##                          label_y, by = "design"),
##             aes(label = label, y = label_y, x = 10),
##             color = "black", family = "mono", size = rel(3), hjust = 1)
## ggsave(msep_sigma_plt, filename = "pdf/msep-sigma-plot.pdf",
##        width = 4, height = 5)

## ## ---- MSEP beta --------------------------
## load_if_not("avg_beta_err", "robj", source("05-estimation-error.r"))

## ## ---- Estimation Error Plot --------------------------
## lbl_tbl <-
##   merge(design_label(design),
##         avg_beta_err[, .(label_y = median(est_err) + sd(est_err)/2),
##                      by = design], by = "design")
## est_error_plt <-
##   ggplot(avg_beta_err[Model != "ols"],
##          aes(ncomp, est_err, color = Model)) +
##   geom_line() + geom_point(shape = 21, fill = "black", size = 0.8) +
##   facet_grid(design ~ ., scales = "free_y", labeller = label_both) +
##   scale_x_continuous(breaks = 0:10) +
##   theme(legend.position = "top") +
##   geom_text(data = design_label(design, c("p", "R2", "relpos", "gamma")),
##             aes(label = label, x = 0, y = 2.5), hjust = 0,
##             family = "mono", color = "black", size = rel(3),
##             nudge_y=-0.25) +
##   labs(x = "Components", y = expression(MSE(hat(beta)))) +
##   coord_cartesian(ylim = c(0, 2.5))
## ggsave(est_error_plt, filename = "pdf/est-error-plot.pdf",
##        width = 4, height = 5)
