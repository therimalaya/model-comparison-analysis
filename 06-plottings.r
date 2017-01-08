## ---- Plotting Design --------------------------
dgn <- copy(design)[, lapply(.SD, as.character)]
dgn_plt <- ggplot(dgn, aes(relpos, p, fill = R2, alpha = gamma)) +
  geom_point(position = position_dodge(width = 1), size = 5, color = "black", shape = 22) +
  scale_alpha_discrete(range = c(0.5, 1)) +
  facet_wrap(~relpos, scales = "free_x", strip.position = "bottom", ncol = 4) +
  scale_x_discrete(breaks="none") + labs(x = "Relevant Position", y = "Number of Variables")
ggsave(dgn_plt, filename = "pdf/design-plot.pdf", width = 7, height = 2)

## ---- Prediction Plots --------------------------
load_if_not("pred_error", "robj", expression(source("04-prediction-error.r")))

## ---- Average Test Prediction Error --------------------------
test_err <- pred_error[, .(MSEP = mean(test_err, na.rm = TRUE)),
                       by = c("Model", "design", "ncomp")]

## ---- Prediction Error Plot for all designs --------------------------
PE_plt <-
  ggplot(test_err[Model != "ols"],
         aes(ncomp, MSEP, color = Model)) +
  geom_point(shape = 4, size = 0.8, na.rm = TRUE) +
  geom_line() +
  facet_wrap(~design, label = label_both,
             nrow = 4, scales = "free_y") +
  theme(legend.position = "top") +
  geom_hline(data = test_err[Model == "ols" & ncomp == 1],
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
err_pls <- dcast(test_err, design + ncomp ~ Model, value.var = "MSEP")
err_pls <- err_pls[ncomp != 0, ols := ols[1], by = design]

err.pls<- err_pls[, (.SD - .SD[["pls"]])/.SD[["pls"]], .SDcols = 3:6,
    by = .(design, ncomp)] %>%
  melt(1:2, variable.name = "Model", value.name = "MSEP") %>%
  na.omit

## ---- Plotting the proportinate difference --------------------------
plt_pls <-
  ggplot(err.pls, aes(ncomp, MSEP, color = Model)) +
  geom_point(shape = 4, size = 0.8) + geom_line() +
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
