## ---- Prediction Plots --------------------------
source("02-design-simulation.r")
load_if_not("pred_error", "robj", expression(source("04-prediction-error.r")))
load_if_not("est_error", "robj", expression(source("05-esimation-error.r")))

## ---- Renameing Some Variables ----------------------------------------
pred_error <- pred_error %>% rename(Method = Model)
est_error <- est_error %>% rename(Method = Model)
design <- design %>% rename(eta = gamma)
design <- design %>% mutate(design = row_number())

## ---- Test Prediction Error --------------------------
test_err <- pred_error %>%
  select(-train_err) %>%
  rename(msep = test_err)

## ---- Average Test Prediction Error --------------------------
avg_test_err <- test_err %>%
  group_by(design, Method, ncomp) %>%
  summarize(MSEP = mean(msep, na.rm = TRUE),
            STD_PE = sd(msep, na.rm = TRUE))

## ## ---- Standard Deviation of Prediction Error Plot for all designs ------------
## SE_PE_plt <-
##   ggplot(filter(avg_test_err, Method != "ols"),
##          aes(ncomp, STD_PE, color = Method)) +
##   geom_point(shape = 4, size = 0.8, na.rm = TRUE) +
##   geom_line() +
##   facet_wrap(~design, label = label_both,
##              nrow = 4, scales = "free_y") +
##   theme(legend.position = "top") +
##   geom_hline(data = filter(avg_test_err, Method == "ols", ncomp != 0),
##              aes(yintercept = STD_PE, color = Method),
##              linetype = "dashed") +
##   labs(x = "Number of Components",
##        y = "Std.Deviation of Prediction Error between replicated calibration sets") +
##   scale_x_continuous(breaks = 0:10) +
##   geom_text(data = design_label(design, c("p", "R2", "relpos", "eta")),
##             aes(label = label),
##             x = 10, y = 0.9,
##             hjust = 1, size = rel(3),
##             show.legend = FALSE, family = "mono",
##             color = "black") +
##   scale_color_brewer(palette = "Set1")
## ggsave(SE_PE_plt, file = "pdf/prediction-error-sd.pdf", width = 15, height = 10)

## ## ---- Prediction Error Plot for all designs --------------------------
## PE_plt <- avg_test_err %>% .pe_plot('TRUE')
## ggsave(PE_plt, file = "pdf/prediction-error.pdf", width = 15, height = 10)

## ---- Prediction Error plot for all designs in two separate page --------------------------
PE_plt_15_1 <- design %>% select(design, p, R2) %>%
  left_join(avg_test_err, by = c("design")) %>% 
  .pe_plot('p == 15 & R2 == 0.5', n.row = 2)
ggsave(PE_plt_15_1, file = "pdf/prediction-error-15-1.pdf", width = 7, height = 4.5)

PE_plt_15_2 <- design %>% select(design, p, R2) %>%
  left_join(avg_test_err, by = c("design")) %>% 
  .pe_plot('p == 15 & R2 == 0.9', n.row = 2)
ggsave(PE_plt_15_2, file = "pdf/prediction-error-15-2.pdf", width = 7, height = 4.5)

PE_plt_40_1 <- design %>% select(design, p, R2) %>%
  left_join(avg_test_err, by = c("design")) %>% 
  .pe_plot('p == 40 & R2 == 0.5', n.row = 2)
ggsave(PE_plt_40_1, file = "pdf/prediction-error-40-1.pdf", width = 7, height = 4.5)

PE_plt_40_2 <- design %>% select(design, p, R2) %>%
  left_join(avg_test_err, by = c("design")) %>% 
  .pe_plot('p == 40 & R2 == 0.9', n.row = 2)
ggsave(PE_plt_40_2, file = "pdf/prediction-error-40-2.pdf", width = 7, height = 4.5)

## ## ---- Proportionate difference of Prediction Error from PLS --------------------------
## err_pls <- test_err %>%
##   spread(key = Method, value = msep) %>%
##   mutate(ols = zoo::na.locf(ols)) %>%
##   mutate(
##     bayes = (bayes - pls)/pls,
##     envelope = (envelope - pls)/pls,
##     ols = (ols - pls)/pls,
##     pls = (pls - pls)/pls
##   ) %>%
##   gather(Method, MSEP, -design:-ncomp)

## ---- Average proportionate difference of prediction error from PLS --------------
avg_err_pls <- err_pls %>%
  group_by(Method, design, ncomp) %>%
  summarize(MSEP = mean(MSEP, na.rm = TRUE))

## ## ---- Plotting the proportinate difference --------------------------
## plt_pls <-
##   ggplot(avg_err_pls, aes(ncomp, MSEP, color = Method)) +
##   geom_point(shape = 4, size = 0.8, na.rm = TRUE) + geom_line() +
##   facet_wrap( ~ design, labeller = label_both, ncol = 8, scales = "free_y") +
##   theme(legend.position = "top") +
##   scale_x_continuous(breaks = 0:10) +
##   labs(x = "Number of Components",
##        y = "Proportion difference of\nPrediction Error from PLS model") +
##   geom_text(data = design_label(design, c("p", "R2", "relpos", "eta")),
##             x = Inf, y = Inf, vjust = 1, hjust = 1, size = rel(2.5),
##             aes(label = label), family = "mono", color = "black") +
##   scale_color_brewer(palette = "Set1")
## ggsave(filename = "pdf/prediction-error-pls.pdf", plot = plt_pls, width = 15, height = 10)

## ## ---- Some selected designs --------------------------
## avg_test_err_selected <- avg_test_err %>%
##   filter(design %in% c(5, 28))
## PE_plt_selected <-
##   ggplot(filter(avg_test_err_selected, Method != "ols"),
##          aes(ncomp, MSEP, color = Method)) +
##   geom_point(shape = 4, size = 1.8, na.rm = TRUE) +
##   geom_line() +
##   facet_wrap(~design, label = label_both,
##              nrow = 1, scales = "free_y") +
##   theme_grey(base_size = 13) +
##   theme(legend.position = "top") +
##   geom_hline(data = filter(avg_test_err_selected, Method == "ols", ncomp != 0),
##              aes(yintercept = MSEP, color = Method),
##              linetype = "dashed") +
##   labs(x = "Number of Components",
##        y = "Prediction Error (MSEP)") +
##   scale_x_continuous(breaks = 0:10) +
##   geom_text(data = design_label(design, c("p", "R2", "relpos", "eta")) %>%
##               filter(design %in% c(5, 28)),
##             aes(label = label),
##             x = Inf, y = Inf, vjust = 1,
##             hjust = 1, size = rel(3),
##             show.legend = FALSE, family = "mono",
##             color = "black") +
##   scale_color_brewer(palette = "Set1")
## ggsave(PE_plt_selected, file = "pdf/prediction-error-selected.pdf",
##        width = 8, height = 3, device = "pdf")

## ## ---- Average Prediction Error relative to PLS --------------------------
## avg_err_pls_selected <- avg_err_pls %>%
##   filter(design %in% c(5, 28))
## plt_pls_selected <-
##   ggplot(avg_err_pls_selected, aes(ncomp, MSEP, color = Method)) +
##   geom_point(shape = 4, size = 0.8, na.rm = TRUE) + geom_line() +
##   facet_wrap(~ design, labeller = label_both, nrow = 1, scales = "free_y") +
##   theme(legend.position = "none") +
##   scale_x_continuous(breaks = 0:10) +
##   labs(x = NULL, #"Number of Components",
##        y = "Prediction error w.r.t. PLS") +
##   ## geom_text(data = design_label(design, c("p", "R2", "relpos", "eta")) %>%
##   ##             filter(design %in% c(5, 28)),
##   ##           x = 10, y = 0, hjust = 1, size = rel(3),
##   ##           aes(label = label), family = "mono", color = "black") +
##   scale_color_brewer(palette = "Set1")
## ggsave(filename = "pdf/prediction-error-pls-selected.pdf",
##        plot = plt_pls_selected, width = 7, height = 2.5)

## ---- Beta Correlation --------------------------
load_if_not(
  obj_name = "beta_corr",
  obj_path = "robj",
  expression = expression({
    fit_obj <- readRDS("robj/fit_obj.rds")
    mdls <- c("ols", "pls", "envelope", "bayes")
    beta_corr <- fit_obj %>%
      group_by_(.dots = c("design", "rep")) %>%
      do({
        map_df(mdls, function(mdl){
          map2_df(.[["sim_obj"]], .[[mdl]], function(x, y){
            true_beta <- c(x$beta0, x$beta)
            est_beta <- get_beta(mdl)(y)
            est_beta[1, 1] <- mean(x$Y)
            out <- apply(est_beta, 2, cor, true_beta)
            data_frame(beta_corr = out) %>%
              rownames_to_column("ncomp") %>%
              mutate(ncomp = as.numeric(ncomp) - 1)
          })
        }, .id = "Model")
      })
    beta_corr <- beta_corr %>%
      ungroup %>%
      mutate(
        Model = mdls[as.numeric(Model)],
        ncomp = as.factor(ncomp),
        design = as.numeric(design),
        rep = as.numeric(rep)
      )
  })
)

## ---- Correlation between True and Estimated Beta Coefficients --------------------------
beta_corr_plot_selected <- beta_corr %>%
  filter(design %in% 1:4, ncomp != 0) %>%
  rename(Method = Model) %>%
  ggplot(aes(ncomp, beta_corr, fill = Method)) +
  geom_boxplot(na.rm = TRUE, alpha = 0.2,
               size = 0.25, outlier.size = 0.5) +
  facet_wrap(~design, labeller = label_both, scales = 'free_y', nrow = 1) +
  stat_summary(fun.y = mean, geom = "line", na.rm = TRUE,
               aes(group = Method, color = Method), size = 0.5) +
  stat_summary(fun.y = mean, geom = "point", na.rm = TRUE,
               aes(group = Method, color = Method), size = 0.5) +
  theme(legend.position = "top") +
  labs(x = NULL, #"Number of Components",
       y = expression(paste("cor(", beta, ",", widehat(beta), ")"))) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")
## ggsave(beta_corr_plot_selected, filename = "pdf/beta-corr-plot-selected.pdf",
##        width = 7, height = 3)

## ---- Beta Estimate Error --------------------------
beta_error_selected <- est_error %>%
  filter(design %in% 1:4, ncomp != 0) %>%
  group_by(Method, design, ncomp) %>%
  summarize(beta_err = mean(beta_err, na.rm = TRUE))

## ---- Beta Estimation Error Plot --------------------------
beta_err_plot_selected <-
  beta_error_selected %>%
  filter(Method != "ols") %>%
  ggplot(aes(ncomp, beta_err, color = Method)) +
  facet_wrap(~design, labeller = label_both, scales = "free_y", nrow = 1) +
  geom_line() + geom_point() +
  theme(legend.position = "none", strip.background = element_blank(),
        strip.text = element_blank()) +
  labs(x = NULL, #"Number of Components",
       y = expression(paste("Avg MSE", (beta)))) +
  geom_hline(
    data = beta_error_selected %>%
      filter(Method == "ols", ncomp != 0),
    aes(yintercept = beta_err, color = Method)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 1:10)
## ggsave(beta_err_plot_selected, filename = "pdf/beta-error.pdf",
##        width = 7, height = 2.5)

## ---- Combine Plot Together --------------------------
est_combined_plot <- 
  arrangeGrob(beta_corr_plot_selected, beta_err_plot_selected, 
              nrow = 2, ncol = 1, heights = c(1/4, 1/5)) %>% 
  grid.arrange() %>% invisible()
ggsave(est_combined_plot, filename = "pdf/est-combined-plot.pdf", width = 8, height = 5)
