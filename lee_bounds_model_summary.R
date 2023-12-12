lee_bounds_model_summary <- function(
    df,
    d,
    m,
    y,
    cluster = NULL,
    c_at_ratio = NULL,
    units = "",
    numdraws = 1000) {
    pt_estimate <- MedBounds::compute_bounds_ats(
        df = df,
        d = d,
        m = m,
        y = y,
        c_at_ratio = c_at_ratio
    )

    bootstrap_draws <- MedBounds:::compute_bootstrap_draws_clustered(
        df = df,
        d = d,
        m = m,
        y = y,
        f = function(..., c_at_ratio_) {
            MedBounds::compute_bounds_ats(..., c_at_ratio = c_at_ratio)
        },
        cluster = cluster,
        fix_n1 = F,
        numdraws = numdraws
    )

    # Use dplyr summarise to compute the sd the columns of bootstrap_draws
    bootstrap_sds <- bootstrap_draws %>%
        summarise_all(sd)

    if (is.null(c_at_ratio)) {
        termlist <- c("Lower bound", "Upper bound")
    } else {
        termlist <- "Point estimate"
    }

    tidy_results <-
        data.frame(
            term = termlist,
            estimate = as.numeric(pt_estimate),
            std.error = as.numeric(bootstrap_sds)
        )


    if (!is.null(c_at_ratio)) {
        glance <- data.frame(c = 1 - c_at_ratio, units = units)
    } # note the c here is 1-c used in paper
    else {
        glance <- data.frame(units = units)
    }
    modresults <- list(
        tidy = tidy_results,
        glance = glance
    )
    class(modresults) <- "modelsummary_list"
    return(modresults)
}
