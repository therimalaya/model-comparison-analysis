## ---- Make Plots and get average test prediction error data --------------------------
source("01-setup.r")
source("02-design-simulation.r")
source("06-plottings.r")

## ---- Join the test prediction error with design paramters --------------------------
model_data <- 
  test_err %>% 
  left_join(design, by = "design") %>%
  mutate(
    relpos = gsub("[c\\(\\)]", "", as.character(relpos)) %>% as.factor,
    p = as.character(p) %>% as.factor,
    R2 = as.character(R2) %>% as.factor,
    eta = as.character(eta) %>% as.factor
  ) %>%
  unite(DataSet, design, rep, sep = ":") %>%
  mutate(
    DataSet = DataSet %>% as.character %>% as.factor,
    Method = Method %>% as.factor
  )

## avg_msep <- model_data %>%
##   group_by(DataSet, Method, p, R2, relpos, eta) %>%
##   summarise(msep = mean(msep, na.rm = TRUE))

min_msep <- model_data %>%
  group_by(DataSet, Method, p, R2, relpos, eta) %>%
  summarise(
    ncomp = ncomp[which.min(msep)],
    msep = min(msep, na.rm = TRUE)
  )

## ---- ANOVA Model with two factor interaction --------------------------
mdl_min <- lmer(msep ~ ( 1 | DataSet ) + (p + R2 + relpos + eta + Method) ^ 3,
           data = min_msep)
## mdl_avg <- lmer(msep ~ ( 1 | DataSet ) + (p + R2 + relpos + eta + Method) ^ 3,
##            data = avg_msep)

# No need of Model reduction -- We are only looking at effects
# subModel_min <- step(mdl_min)
# subModel_avg <- step(mdl_avg)

## ---- Printing the results --------------------------
# We don't Need it anymore
# car::Anova(mdl_min)
# car::Anova(subModel_min$model)
# 
# car::Anova(mdl_avg)
# car::Anova(subModel_avg$model)

## ---- Some Boxplots --------------------------
# No need of box-plots too
# plt1 <-
  # ggplot(model_data, aes(Model, msep, fill = relpos)) +
  # geom_boxplot(size = 0.15, outlier.shape = 21, outlier.size = 0.25) +
  # facet_grid(R2 ~ p, scales="free", labeller = label_both) +
  # scale_fill_brewer(palette="Set1") +
  # labs(x = NULL, y = "Minimum test prediction error") +
  # ggtitle("Model Comparison",
          # subtitle="Based on minimum test prediction error") +
  # theme(legend.position = "bottom")
# ggsave(plt1, filename = "pdf/boxplots.pdf", width = 7, height = 5)


# plt2 <-
  # ggplot(model_data, aes(Model, msep, fill = p)) +
  # facet_wrap( ~ R2, labeller = label_both) +
  # scale_fill_brewer(palette="Set1") +
  # labs(x = NULL, y = "Minimum test prediction error") +
  # ggtitle("Model Comparison",
          # subtitle="Based on minimum test prediction error") +
  # theme(legend.position = "bottom") +
  # geom_boxplot(size = 0.15, outlier.shape = 21, outlier.size = 0.25)
# ggsave(plt2, filename = "pdf/boxplots-1.pdf", width = 7, height = 3)

## ---- Effect Plot :: p:R2:Method --------------------------
library(effects)
eff_plot_min <-
  allEffects(mdl_min) %>%
  .[["p:R2:Method"]] %>%
  as_data_frame %>%
  mutate(Method = gsub("envelope", "env", Method)) %>%
  ggplot(aes(Method, fit)) +
  geom_line(aes(group = 1)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.25, color = "red") +
  facet_grid(p~R2, labeller = label_both, switch = 'y') +
  labs(y = "Minimum MSEP",
       x = "Methods") +
  ggtitle('Effect Plot',
          expression(paste("Interaction:: p:", R^2, ":Method"))) +
  theme(plot.subtitle = element_text(family = "mono"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_point(size = 1) +
  scale_y_continuous(
    breaks = function(limits)seq(floor(limits[1]), ceiling(limits[2]), 0.25)
  )

## ---- Effect Plot :: relpos:gamma:Method --------------------------
eff_plot_min_rgm <-
  allEffects(mdl_min) %>%
  .[["relpos:eta:Method"]] %>%
  as_data_frame %>%
  mutate(Method = gsub("envelope", "env", Method)) %>%
  ggplot(aes(relpos, fit)) +
  geom_line(aes(group = 1)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.25, color = "red") +
  facet_grid(eta~Method, labeller = label_both, switch = 'y') +
  labs(x = "Position of relevant components",
       y = NULL) +
  ggtitle('', expression(paste("Interaction:: relpos:", eta, ":Method"))) +
  theme(plot.subtitle = element_text(family = "mono"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_point(size = 1) +
  scale_y_continuous(position = 'right')

## ---- Combine effect plot ----------------------------------------
eff_plot <- 
  gridExtra::grid.arrange(
    gridExtra::arrangeGrob(eff_plot_min, eff_plot_min_rgm, nrow = 1, widths = c(1/6, 1/4)))
ggsave(eff_plot, filename = "pdf/effect-plot.pdf", width = 7.5, height = 3.5)
## ggsave(eff_plot, filename = "pdf/effect-plot.svg", device = "svg", width = 8, height = 3)


## ---- ANOVA table xtable --------------------------
# We don't Need these ANOVA tables as well ---
# library(xtable)
# aov_min <- car::Anova(subModel_min$model, test.statistic = "F")
# aov_min_caption <- capture.output(aov_min)[1]
# aov_min %>%
#   xtable(caption = aov_min_caption,
#          label = "tab:anova-table",
#          align = "Xrcrr") %>%
#   print(width = "0.8\\textwidth",
#         tabular.environment = "tabularx")
