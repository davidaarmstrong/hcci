print.hcci <- function(x, ..., digits=3){
    fmt <- paste0("%.", digits, "f")
    dub <- ifelse(length(x$ci_lower_double) > 0, T, F)
    mat <- cbind(
        sprintf(fmt, x$beta), 
        sprintf(fmt, x$ci_lower_simple),
        sprintf(fmt, x$ci_upper_simple))
    colnames(mat) <- c("Estimate", "Lower", "Upper")
    rownames(mat) <- names(x$beta)
    if(length(x$ci_lower_double) > 0){
        mat <- cbind(mat, 
            sprintf(fmt, x$ci_lower_double), 
            sprintf(fmt, x$ci_upper_double))
        colnames(mat)[4:5] <- c("Lower (double)", "Upper (double)")
    }
    cat("\n")
    cat("Bootstrap-t Intervals using the ", ifelse(dub, "Double", "Single "), "Bootstrap\n", sep="")
    cat(x$J, " bootrstrap iterations ", ifelse(dub, paste0("with ", x$K, " second bootstrap iterations"), ""), "\n", sep="")
    print(noquote(mat))
}
