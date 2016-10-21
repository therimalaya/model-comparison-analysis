pcr_beta_err <- rbindlist(lapply(1:32, function(dgn){
    lst <- rbindlist(lapply(1:5, function(rep){
            get_beta_error(sim_obj[design == dgn & rep == rep, obj][[1]],
                       pcr_obj[[dgn]][[rep]], "pcr")
    }), idcol = TRUE)
    setnames(lst, ".id", "rep")
    lst[, .(beta_error = mean(beta_error)), by = .(ncomp)]
}), idcol = TRUE)
setnames(beta_err, ".id", "design")
