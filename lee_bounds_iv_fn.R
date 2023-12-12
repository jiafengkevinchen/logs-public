compute_ecdf <- function(df) {
    yvals <- unique(df$y_orig)

    y0_string <- ""
    y1_string <- ""

    # Create LHS variables so that IV estimates the complier CDF for Y0 and for Y1
    for (i in 1:length(yvals)) {
        df[[glue("dm1_times_y_lt_threshold{i}")]] <- (df$d - 1) * (df$y_orig <= yvals[i])
        df[[glue("d_times_y_lt_threshold{i}")]] <- (df$d) * (df$y_orig <= yvals[i])
        y0_string <- paste(y0_string, sep = ifelse(i == 1, "", " , "), glue("dm1_times_y_lt_threshold{i}"))
        y1_string <- paste(y1_string, sep = ifelse(i == 1, "", " , "), glue("d_times_y_lt_threshold{i}"))
    }

    ythreshold_ivs <- suppressMessages(feols(
        fml = as.formula(glue("c({y0_string},{y1_string}) ~ {iv_rhs}")),
        data = df,
        cluster = "respondent_id",
        warn = FALSE # Suppress warnings
    ))

    ecdf_y0 <-
        purrr::map_dbl(
            .x = 1:length(yvals),
            .f = ~ coefficients(ythreshold_ivs[[.x]])[["fit_jikokoa"]]
        )

    ecdf_y1 <- purrr::map_dbl(
        .x = 1:length(yvals) + length(yvals),
        .f = ~ coefficients(ythreshold_ivs[[.x]])[["fit_jikokoa"]]
    )

    ecdf_table <- data.frame(y = yvals, ecdf_y0 = ecdf_y0, ecdf_y1 = ecdf_y1)

    return(ecdf_table)
}

# This function computes Lee bounds from the ecdf
# It assumes monotonicity goes positive (i.e. Y0 > 0 => Y1 > 0)
compute_bounds_from_ecdf <- function(ecdf_table) {
    # F^-1(u) = min{y: F(y) >= u} is monotone
    inv_cdf0 <- function(u) {
        min(ecdf_table$y[ecdf_table$ecdf_y0 >= u])
    }
    inv_cdf1 <- function(u) {
        min(ecdf_table$y[ecdf_table$ecdf_y1 >= u])
    }

    # Read off the fraction of compliers, NTs, and ATs
    frac_nt <- ecdf_table$ecdf_y1[ecdf_table$y == 0]
    frac_c <- ecdf_table$ecdf_y0[ecdf_table$y == 0] - ecdf_table$ecdf_y1[ecdf_table$y == 0]
    frac_at <- 1 - frac_nt - frac_c

    numdraws <- 10^5
    set.seed(0)

    # Compute ub of E[Y(1)| AT] for instrument compliers
    upper_bound_draws <- runif(n = numdraws, min = 1 - frac_at, max = 1) # Generate U ~ Unif[1-p_AT, 1]
    ub_draws_levels <- map_dbl(.x = upper_bound_draws, .f = inv_cdf1) # Generate F^{-1}(U)
    ub_y1_levels <- mean(ub_draws_levels) # take mean and mean of logs
    ub_y1_logs <- mean(log(ub_draws_levels))

    # Compute lb of E[Y(1)| AT]
    lower_bound_draws <- runif(n = numdraws, min = frac_nt, max = frac_nt + frac_at)
    lb_draws_levels <- map_dbl(.x = lower_bound_draws, .f = inv_cdf1)
    lb_y1_levels <- mean(lb_draws_levels)
    lb_y1_logs <- mean(log(lb_draws_levels))

    # Calculate mean of Y0 for ATs
    u0_draws <- runif(numdraws, min = frac_nt + frac_c, max = 1)
    y0_draws_levels <- map_dbl(.x = u0_draws, .f = inv_cdf0)

    mean_y0_ats_levels <- mean(y0_draws_levels)
    mean_y0_ats_logs <- mean(log(y0_draws_levels))

    lee_bounds_levels <- c(lb_y1_levels, ub_y1_levels) - mean_y0_ats_levels
    lee_bounds_logs <- c(lb_y1_logs, ub_y1_logs) - mean_y0_ats_logs

    return(
        data.frame(
            logs_lb = lee_bounds_logs[1],
            logs_ub = lee_bounds_logs[2],
            levels_lb = lee_bounds_levels[1],
            levels_ub = lee_bounds_levels[2]
        )
    )
}

lee_bounds_iv_fn <- function(df) {
    ecdf_table <- compute_ecdf(df)

    # If monotonicity goes in the usual direction, return Lee bounds
    if (ecdf_table$ecdf_y0[ecdf_table$y == 0] > ecdf_table$ecdf_y1[ecdf_table$y == 0]) {
        return(compute_bounds_from_ecdf(ecdf_table = ecdf_table))
    } else {
        # Otherwise, flip the role of Y(1) and Y(0), then reverse the bounds
        flipped_ecdf <- data.frame(y = ecdf_table$y, ecdf_y0 = ecdf_table$ecdf_y1, ecdf_y1 = ecdf_table$ecdf_y0)
        flipped_bounds <- compute_bounds_from_ecdf(ecdf_table = flipped_ecdf)
        unflipped_bounds <- data.frame(
            logs_lb = -flipped_bounds$logs_ub, logs_ub = -flipped_bounds$logs_lb,
            levels_lb = -flipped_bounds$levels_ub, levels_ub = -flipped_bounds$levels_lb
        )
        return(unflipped_bounds)
    }
}
