source("02-design-simulation.r")
models <- c("linear", "pcr", "plsr", "envelope", "bayes")

get_fitted("bayes", ncomp = 1)
