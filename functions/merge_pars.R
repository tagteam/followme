# -----------------------------
# Helper: merge user-specified pars with defaults
# -----------------------------
merge_pars <- function(default_par,user_par = NULL) {
    if (is.null(user_par)) return(default_par)
    merged <- default_par
    for (nm in names(user_par)) {
        if (!nm %in% names(default_par)) warning("Unknown parameter: ", nm)
        merged[nm] <- user_par[[nm]]
    }
    merged
}
