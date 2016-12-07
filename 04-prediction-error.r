source("03-model-fitting.r")

## ---- Code for Prediction Error for different model -----
mdls <- opt$mdls[c(1, 3, 5, 6)]
#fit_obj <- fit_obj[lapply(bayes, class) == "try-error", bayes := list(NA)]
pred_error <- map_df(mdls, function(mdl){
  fit_obj[, map2_df(.SD[["sim_obj"]],
                                  .SD[[mdl]],
                                  ~get_pred_error(.x, .y, mdl)),
          by = c("design", "rep")]
}, .id = "Model")
pred_error[, Model := mdls[as.numeric(Model)]]
setkeyv(pred_error, c("Model", "design", "ncomp"))

## ---- Average Predidtion Error different replicates -----
avg_pred_error <- pred_error[, lapply(.SD, mean), by = key(pred_error), .SDcols = c("train_err", "test_err")]

## ----  Plottings -----
avg_pred_error <- melt(avg_pred_error, 1:3, variable.name = "estimate", value.name = "MSEP")
avg_pred_error[, estimate := gsub("_err", "", estimate)]


## ----  Output plot to PDF --------
## library(RColorBrewer)
## makeColors <- function(){
##   maxColors <- 10
##   usedColors <- c()
##   possibleColors <- colorRampPalette( brewer.pal( 9 , "Set1" ) )(maxColors)
##   function(values){
##     newKeys <- setdiff(values, names(usedColors))
##     newColors <- possibleColors[1:length(newKeys)]
##     usedColors.new <-  c(usedColors, newColors)
##     names(usedColors.new) <- c(names(usedColors), newKeys)
##     usedColors <<- usedColors.new
##     possibleColors <<- possibleColors[length(newKeys)+1:maxColors]
##     usedColors
##   }
## }
## mkColor <- makeColors()

## plts <- lapply(as.character(unique(test_err$design)), function(which_design){
##   dta <- test_err[design == which_design] #avg_pred_error[design == which_design]
##   plt <- ggplot(dta[Model != "ols"],
##                 aes(ncomp, MSEP, color = Model)) +
##     geom_point(size = 0.75, shape = 23) +
##     geom_line() +
##     coord_cartesian(ylim = c(0, 1.5)) +
##     theme(legend.position = "top") +
##     labs(x = "Number of components", y = "Mean Prediction Error") +
##     ggtitle(paste0("Design:", which_design)) +
##     scale_x_continuous(breaks = 0:10) +
##     scale_color_manual(values = mkColor(dta$Model))
##   ## faceting if there is something
##   if(nrow(dta) != 0) plt <- plt + facet_grid(estimate ~ .)
##   ## Add linear line
##   plt <- plt + geom_hline(data = dta[Model == "ols" & ncomp == 1],
##                           aes(yintercept = MSEP, color = Model))
##   ## Annotation
##   plt <- plt + annotate("text", x = 0, y = 1.4, hjust = 0, size = rel(3),
##                         label = paste(capture.output(
##                           data.table(design)[as.numeric(which_design)]
##                         ), collapse = "\n"),
##                  family = "mono")
##   return(plt)
## })

## pdf(file = "prediction-error.pdf", onefile = TRUE, width = 8, height = 12)
## for (pg in seq(1, length(unique(test_err$design)), 4)) {
##   idx <- seq.int(pg, length.out = 4)
##   p <- plts[idx]
##   p <- append(p, list(ncol = 2, nrow = 2))
##   do.call(share_legend, p)
## }
## dev.off()

test_err <- avg_pred_error[!(design %in% avg_pred_error[Model == "ols" & MSEP > 1 & ncomp == 1, design]) &
                             estimate == "test"]
setnames(test_err, "design", "Design")

ann_text <- copy(design)[, Design := .I]
rownames(ann_text) <- NULL
ann_text[, label_text := lapply(1:.N, function(x) {
  paste(capture.output(ann_text[x, .(n, p, R2, relpos, gamma)]),
        collapse = "\n")
})]
test_err <- merge(test_err, ann_text[, .(Design, label_text)], by = "Design")
test_err[, label_text := unlist(label_text)]

plt <- ggplot(test_err[Model != "ols"],
              aes(ncomp, MSEP, color = Model)) +
  geom_point(shape = 4, size = 0.8) +
  geom_line() +
  facet_grid(. ~ Design, labeller = label_both) +
  geom_hline(data = test_err[Model == "ols" & ncomp == 1],
             aes(yintercept = MSEP, color = Model)) +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Number of Components", y = "Prediction Error") +
  geom_text(x = 10, y = 0.95, hjust = 1,
            aes(label = label_text), family = "mono",
            color = "black")
ggsave(filename = "prediction-error.pdf", plot = plt, width = 9, height = 4)


## Compare with PLS
test_err_pls <- dcast(test_err, label_text + Design + ncomp + estimate ~ Model,
                      value.var = "MSEP")[, (.SD - .SD[["pls"]])/.SD[["pls"]],
                                          .SDcols = 5:8,
                                          by = .(label_text, Design, ncomp, estimate)]
test_err_pls <- test_err_pls %>%
  melt(1:4, variable.name = "Model", value.name = "MSEP") %>%
  na.omit

plt_pls <- ggplot(test_err_pls[Model != "ols"], aes(ncomp, MSEP, color = Model)) +
  geom_point(shape = 4, size = 0.8) + geom_line() +
  facet_grid(. ~ Design, labeller = label_both) +
  geom_hline(data = test_err_pls[Model == "ols" & ncomp == 1],
             aes(yintercept = MSEP, color = Model)) +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Number of Components",
       y = "Proportion difference of\nPrediction Error from PLS model") +
  geom_text(x = 10, y = 0.95, hjust = 1,
            aes(label = label_text), family = "mono", color = "black")
ggsave(filename = "prediction-error-pls.pdf", plot = plt_pls, width = 9, height = 4)
