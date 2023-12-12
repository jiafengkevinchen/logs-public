# This script contains code for generating results in Section 5.3 of the paper

library(haven)
library(dplyr)
library(here)
library(fixest)
library(modelsummary)
options(modelsummary_format_numeric_latex = "mathmode")
library(msm)
library(hdm)
library(purrr)
library(devtools)
library(MedBounds)
source(here("lee_bounds_model_summary.R"))
source(here("extract_tabular.R"))
library(glue)

# Create temp dir (if it doesn't exist) to store bootstrap results
dir.create(here("temp/"), showWarnings = FALSE)

# Toggle whether to re-run the bootstrap or use the results saved on disk
run_bootstrap <- TRUE
set.seed(2138)
df <- read_dta(here("_output/paper10.dta"))

# Back out the original y
# This is weekly dollars in IHS
yname <- quo(ihs_amount_weekly)
dname <- quo(jikokoa)
df <-
    df %>%
    mutate(
        y_orig = sinh(!!yname),
        asinh_original = asinh(y_orig),
        asinh_yearly = asinh(y_orig * 52),
        asinh_shillings = asinh(100 * y_orig),
        asinh_shillings_yearly = asinh(100 * 52 * y_orig),
        d = !!dname
    )


# Compile the control variables used in the regression
control_list <- "finwtp_USD + sms_amount_weekly_pre + d_charcoalbuy_KSH + spend50 + savings_KSH + b_incomeself_KSH + RiskAverse + CreditConstrained + b_residents + b_children + d_jikokoalast_years + v1_beliefs_annual_mean + v1_beliefs_annual_sd"

TsinceV_vars <- "TsinceV2"
for (i in 2:24) {
    TsinceV_vars <- paste(TsinceV_vars, paste0("TsinceV2_", as.character(i)), sep = " + ")
}
control_list <- paste(control_list, TsinceV_vars, sep = " + ")
fe_list <- "SMS_date + treata + treatc"
iv_rhs <- glue("{control_list} | {fe_list} | jikokoa~price_USD")

## Rerun results with different units
# replicate the original IV regression
original_IV <-
    feols(
        fml = as.formula(glue("asinh_original ~ {iv_rhs}")),
        data = df,
        cluster = "respondent_id"
    )

# Do the same thing but for the yearly version
asinh_yearly_IV <-
    feols(
        fml = as.formula(glue("asinh_yearly ~ {iv_rhs}")),
        data = df,
        cluster = "respondent_id"
    )

# Do the same thing but for the shillings version
asinh_shillings_IV <-
    feols(
        fml = as.formula(glue("asinh_shillings ~ {iv_rhs}")),
        data = df,
        cluster = "respondent_id"
    )

# Do the same thing for shillings and yearly
asinh_shillings_yearly_IV <-
    feols(
        fml = as.formula(glue("asinh_shillings_yearly ~ {iv_rhs}")),
        data = df,
        cluster = "respondent_id"
    )

# IV for extensive margin
# Do the same thing for shillings and yearly
extensivemargin_IV <-
    feols(
        fml = as.formula(glue("y_orig >0 ~ {iv_rhs}")),
        data = df,
        cluster = "respondent_id"
    )
# First stage of IV regression
feols(
    fml = as.formula(glue("jikokoa ~ price_USD + {control_list} | {fe_list}")),
    data = df,
    cluster = "respondent_id"
)

# Table with sensitivity to units
modelsummary(list(original_IV, asinh_yearly_IV, asinh_shillings_IV, asinh_shillings_yearly_IV),
    coef_omit = "^(?!fit_jikokoa)",
    coef_rename = c("fit_jikokoa" = "Bought stove"),
    digits = 2,
    add_rows = tribble(
        ~term, ~model1, ~model2, ~model3, ~model4,
        "Units of outcome:", "USD - Weekly", "USD - Yearly", "KSh - Weekly", "KSh - Yearly"
    ),
    gof_map = c(""),
    here("tables/berkouwer-dean-arcsinh.tex"),
    align = "ldddd"
)

## Calculate LATE as a % of the control complier mean
# IV in levels
IV_levels <- feols(
    fml = as.formula(glue("y_orig ~ {iv_rhs}")), data = df, cluster = "respondent_id"
)

# Control complier mean
IV_CCM <- feols(
    fml = as.formula(glue("(d-1)*y_orig ~ {iv_rhs}")), data = df, cluster = "respondent_id"
)

# LATE as a % of the CCM
print("LATE% = ")
print(coefficients(IV_levels)["fit_jikokoa"] / coefficients(IV_CCM)["fit_jikokoa"])

# Bootstrap for the LATE as a % of the CCM
if (run_bootstrap) {
    library(doParallel)
    library(foreach)
    library(tictoc)
    library(parallelly)
    tic()
    numBootstraps <- 1000
    registerDoParallel(cores = availableCores())
    IV_levels_bs_results <- foreach(i = 1:numBootstraps, .combine = c) %dopar% {
        set.seed(i)
        # Create a bootstrap sample of clusters
        uniqueClusters <- unique(df$respondent_id)

        clusters_boot <- data.frame(
            respondent_id = sample(uniqueClusters, replace = TRUE, size = length(uniqueClusters)),
            new_respondent_id = 1:length(uniqueClusters)
        )

        # Create a bootstrap sample of respondents
        df_boot <- left_join(clusters_boot, df, by = "respondent_id")
        df_boot$respondent_id <- df_boot$new_respondent_id

        IV_levels_boot <- suppressMessages(feols(
            fml = as.formula(glue("y_orig ~ {iv_rhs}")),
            data = df_boot,
            cluster = "respondent_id"
        ))

        IV_CCM_boot <- suppressMessages(feols(
            fml = as.formula(glue("(d-1)*y_orig ~ {iv_rhs}")),
            data = df_boot,
            cluster = "respondent_id"
        ))
        coefficients(IV_levels_boot)["fit_jikokoa"] / coefficients(IV_CCM_boot)["fit_jikokoa"]
    }
    toc()
    saveRDS(IV_levels_bs_results, file = here("temp/IV_levels_bs_results.RDS"))
}

IV_levels_bs_results <- readRDS(here("temp/IV_levels_bs_results.RDS"))

# SE for the LATE as a % of the CCM
print("Bootstrap SE for LATE% = ")
print(sd(IV_levels_bs_results, na.rm = TRUE))

# Treatment effect on extensive margin
IV_extmargin <- feols(
    fml = as.formula(glue("y_orig>0 ~ {iv_rhs}")),
    data = df,
    cluster = "respondent_id"
)

extmargin <- coefficients(IV_extmargin)["fit_jikokoa"]


## Compute the Lee bounds for the intensive margin log effect of the instrument-compliers

## Calculate CDFs for compliers
source(here("lee_bounds_iv_fn.R"))
lee_bounds <- lee_bounds_iv_fn(df = df)

## Create a parallelized bootstrap of the lee_bounds_iv_fn using doparallel
if (run_bootstrap) {
    library(doParallel)
    library(foreach)
    library(tictoc)
    library(parallelly)
    tic()
    num_bootstraps <- 1000

    # Use half the available cores
    registerDoParallel(cores = round(availableCores() / 2))

    lee_bounds_boot <- foreach(i = 1:num_bootstraps, .combine = rbind) %dopar% {
        set.seed(i)
        unique_clusters <- unique(df$respondent_id)

        # Create a bootstrap sample of clusters
        clusters_boot <- data.frame(
            respondent_id = sample(unique_clusters, replace = TRUE, size = length(unique_clusters)),
            new_respondent_id = 1:length(unique_clusters)
        )
        # Create a bootstrap sample of respondents
        df_boot <- left_join(clusters_boot, df, by = "respondent_id")
        df_boot$respondent_id <- df_boot$new_respondent_id

        lee_bounds_iv_fn(df = df_boot)
    }

    toc()
    saveRDS(
        file = here("temp/berkouwer-dean-bs.rds"),
        object = lee_bounds_boot
    )
}
lee_bounds_boot <- readRDS(here("temp/berkouwer-dean-bs.rds"))

# Compute the SD of the bootstrap
lee_bounds_boot_sd <- as.data.frame(t(apply(lee_bounds_boot, 2, sd)))
# Append sd to the end of the colnames of lee_bounds_boot_sd
colnames(lee_bounds_boot_sd) <- paste0(colnames(lee_bounds_boot_sd), "_sd")

# Merge lee_bounds_boot_sd with the original lee_bounds
cbind(lee_bounds, lee_bounds_boot_sd) %>%
    select(
        contains("logs_lb"), contains("logs_ub"),
        contains("levels_lb"), contains("levels_ub")
    )


# Clean up all the latex files created by modelsummary and gt
# I.e. remove the info outside of the tabular environment
# Change longtable to tabular
# Remove math mode
extract_tabular(dir = here("tables/"), regex = "berkouwer(.*).tex")
convert_longtable(dir = here("tables/"), regex = "berkouwer(.*).tex")
remove_mathmode(dir = here("tables/"), regex = "berkouwer(.*).tex")
