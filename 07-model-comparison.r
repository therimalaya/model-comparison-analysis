## ---- Make Plots and get average test prediction error data --------------------------
source("06-plottings.r")

## ---- Join the test prediction error with design paramters --------------------------
model_data <- design %>%
  rownames_to_column(var = "design") %>%
  mutate(design = as.numeric(design)) %>%
  left_join(test_err, by = "design") %>%
  mutate(
    relpos = gsub("[c\\(\\)]", "", as.character(relpos)),
    p = as.character(p),
    R2 = as.character(R2),
    gamma = as.character(gamma)
  )

## ---- ANOVA Model with two factor interaction --------------------------
anova_model <- lm(MSEP ~ (Model + p + R2 + relpos + gamma) ^ 2, data = model_data)
summary(aov(anova_model))

## ---- Some Boxplots --------------------------
boxplot_pfill <- ggplot(model_data, aes(Model, MSEP, fill = p)) +
  geom_boxplot() +
  facet_grid(R2 ~ gamma, labeller = label_both) +
  labs(x = NULL, y = "Mean Square error of prediction (MSEP)") +
  scale_fill_brewer(name = "N.Var", palette = "Set1") +
  theme(legend.position =  "top")

boxplot_relpos_fill <- ggplot(model_data, aes(Model, MSEP, fill = relpos)) +
  geom_boxplot() +
  facet_grid(R2 ~ gamma, labeller = label_both) +
  labs(x = NULL, y = "Mean Square error of prediction (MSEP)") +
  scale_fill_brewer(name = "RelPos", palette = "Set1") +
  theme(legend.position = "top" )

pdf(file = "pdf/boxplots.pdf", onefile = TRUE, height = 6, width = 8)
plot(boxplot_pfill)
plot(boxplot_relpos_fill)
dev.off()


