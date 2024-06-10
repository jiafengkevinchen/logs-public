# This script contains code for generating results in Section 5.1 of the paper

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

df <- read_dta(here("_output/paper3.dta"))

# Seeding for reproducibility
set.seed(2138)

# Using work hours as the LHS variable
yname <- quo(el_emp_hour_all_uncd_i)

df <-
    df %>%
    mutate(
        y_orig = sinh(!!yname),
        asinh_original = asinh(y_orig),
        asinh_yearly = asinh(y_orig * 52),
        asinh_fte = asinh(y_orig / 40)
    )



# Table 3 in the paper: Run asinh regressions

asinh_reg <-
    feols(
        asinh_original ~ public + private + placebo + Edn_Matric + bl_score_num +
            bl_score_lit + bl_score_cft + bl_score_grit + noncog_score_nm + noncog_score_md + bl_belsco_num_nm +
            bl_belsco_num_md + bl_belsco_lit_ter_nm + bl_belsco_lit_ter_md +
            bl_belsco_cft_ter_nm + bl_belsco_cft_ter_md +
            bl_belest_index + bl_age + bl_male + bl_emp_7d + bl_time_med_nm + bl_time_med_md + bl_time_presentbias_nm +
            bl_time_presentbias_md + bl_risk_high_nm + bl_risk_high_md | bl_block,
        df,
        cluster = "bl_date_baseline"
    )

asinh_reg_year <-
    feols(asinh_yearly ~ public + private + placebo + Edn_Matric + bl_score_num + bl_score_lit + bl_score_cft + bl_score_grit + noncog_score_nm + noncog_score_md + bl_belsco_num_nm + bl_belsco_num_md + bl_belsco_lit_ter_nm + bl_belsco_lit_ter_md + bl_belsco_cft_ter_nm + bl_belsco_cft_ter_md + bl_belest_index + bl_age + bl_male + bl_emp_7d + bl_time_med_nm + bl_time_med_md + bl_time_presentbias_nm + bl_time_presentbias_md + bl_risk_high_nm + bl_risk_high_md | bl_block,
        df,
        cluster = "bl_date_baseline"
    )

asinh_reg_fte <-
    feols(asinh_fte ~ public + private + placebo + Edn_Matric + bl_score_num + bl_score_lit + bl_score_cft + bl_score_grit + noncog_score_nm + noncog_score_md + bl_belsco_num_nm + bl_belsco_num_md + bl_belsco_lit_ter_nm + bl_belsco_lit_ter_md + bl_belsco_cft_ter_nm + bl_belsco_cft_ter_md + bl_belest_index + bl_age + bl_male + bl_emp_7d + bl_time_med_nm + bl_time_med_md + bl_time_presentbias_nm + bl_time_presentbias_md + bl_risk_high_nm + bl_risk_high_md | bl_block,
        df,
        cluster = "bl_date_baseline"
    )

modelsummary(
    list(
        "arcsinh(weekly hrs)" = asinh_reg,
        "arcsinh(yearly hrs)" = asinh_reg_year,
        "arcsinh(FTEs)" = asinh_reg_fte
    ),
    coef_omit = "^(?!public)",
    coef_rename = c("public" = "Treatment"),
    digits = 2,
    add_rows = tribble(
        ~term, ~model1, ~model2, ~model3,
        "Units of outcome:", "Weekly Hrs", "Yearly Hrs", "FTEs"
    ),
    gof_map = c(""),
    output = here("tables/carranza-asinh-results.tex")
)


# Lee bounds analysis in Section 5.1 of the paper

# Create a dataset excluding missing and excluding placebo, for the Lee bounds analysis
df_nomiss_noplacebo <-
    df %>%
    filter(treatment == 0 | treatment == 2) %>%
    filter(!is.na(y_orig) & !is.na(public)) %>%
    mutate(
        logYOrig = ifelse(y_orig > 0, log(y_orig), 0),
        positiveY = y_orig > 0
    )

# Lee bounds for Y
lee_y <-
    lee_bounds_model_summary(
        df = df_nomiss_noplacebo,
        d = "public",
        m = "positiveY",
        y = "y_orig",
        units = "Hours",
        cluster = "bl_date_baseline"
    )

# Lee bounds for log(Y)
lee_logY <-
    lee_bounds_model_summary(
        df = df_nomiss_noplacebo,
        d = "public",
        m = "positiveY",
        y = "logYOrig",
        units = "Log(Hours)",
        cluster = "bl_date_baseline"
    )




# Table 5: Lee bounds for Y under different assumptions about compliers vs ATs
# Note that c_at_ratio = E[Y(1) | C]/E[Y(1) | AT]
# Hence, E[Y(1) | C] = c_at_ratio E[Y(1) | AT], and thus c_at_ratio = 1-c for
# the c we use in the paper

lee_c1 <-
    lee_bounds_model_summary(
        df = df_nomiss_noplacebo,
        d = "public",
        m = "positiveY",
        y = "y_orig",
        c_at_ratio = 1,
        units = "Hours",
        cluster = "bl_date_baseline"
    )

lee_c75 <-
    lee_bounds_model_summary(
        df = df_nomiss_noplacebo,
        d = "public",
        m = "positiveY",
        y = "y_orig",
        c_at_ratio = 0.75,
        units = "Hours",
        cluster = "bl_date_baseline"
    )

lee_c50 <-
    lee_bounds_model_summary(
        df = df_nomiss_noplacebo,
        d = "public",
        m = "positiveY",
        y = "y_orig",
        c_at_ratio = 0.5,
        units = "Hours",
        cluster = "bl_date_baseline"
    )

modelsummary(list(lee_logY, lee_y, lee_c1, lee_c75, lee_c50),
    output = here("tables/carranza-lee-bounds.tex"),
    align = "lddddd"
)


## Table 4 in the paper: Average proportional effects / Poisson reg

# Compute the average outcomes in the two groups
df_nomiss_noplacebo %>%
    group_by(public) %>%
    summarise(mean(y_orig, na.rm = T))

# Estimate ratio with Poisson reg
poisson_no_covariates <-
    fepois(
        fml = y_orig ~ public,
        data = df_nomiss_noplacebo,
        cluster = "bl_date_baseline"
    )


# The implied ATE%
implied_ATEpercent <-
    exp(coefficients(poisson_no_covariates)["public"]) - 1

# The SE, which using delta method is just exp(beta)*SE(beta)
se_ATEpercent <-
    exp(coefficients(poisson_no_covariates)["public"]) *
        sqrt(poisson_no_covariates$cov.scaled["public", "public"])

tidy_results <- data.frame(
    term = c("beta_0", "beta_1", "Implied Prop. Effect"),
    estimate = c(coefficients(poisson_no_covariates), implied_ATEpercent),
    std.error = c(sqrt(diag(poisson_no_covariates$cov.scaled)), se_ATEpercent)
)

mod <- list(tidy = tidy_results, glance = data.frame(Covariates = "N"))
class(mod) <- "modelsummary_list"


# Estimated ratio with Poisson reg + covariates
poisson_with_covariates <-
    fepois(
        fml = y_orig ~ public + Edn_Matric + bl_score_num + bl_score_lit + bl_score_cft +
            bl_score_grit + noncog_score_nm + noncog_score_md + bl_belsco_num_nm + bl_belsco_num_md +
            bl_belsco_lit_ter_nm + bl_belsco_lit_ter_md + bl_belsco_cft_ter_nm +
            bl_belsco_cft_ter_md + bl_belest_index +
            bl_age + bl_male + bl_emp_7d + bl_time_med_nm + bl_time_med_md + bl_time_presentbias_nm +
            bl_time_presentbias_md + bl_risk_high_nm + bl_risk_high_md + factor(bl_block),
        data = df_nomiss_noplacebo,
        cluster = "bl_date_baseline"
    )

# The implied ATE%
implied_ATEpercent_covariates <- exp(coefficients(poisson_with_covariates)["public"]) - 1

# The SE, which using delta method is just exp(beta)*SE(beta)
se_ATEpercent_covariates <-
    exp(coefficients(poisson_with_covariates)["public"]) *
        sqrt(poisson_with_covariates$cov.scaled["public", "public"])


tidy_results_covariates <- data.frame(
    term = c("beta_0", "beta_1", "Implied Prop. Effect"),
    # The following line corrects an error in Table 4 in the published version. 
    # See the corrigendum for details.
    estimate = c(coefficients(poisson_with_covariates)[c("(Intercept)", "public")], implied_ATEpercent_covariates),
    std.error = c(sqrt(diag(poisson_with_covariates$cov.scaled[c("(Intercept)", "public"), c("(Intercept)", "public")])), se_ATEpercent_covariates)
)

mod_covariates <- list(tidy = tidy_results_covariates, glance = data.frame(Covariates = "Y"))
class(mod_covariates) <- "modelsummary_list"

# Save Table 4 in the paper
modelsummary(list(mod, mod_covariates), out = here("Tables/carranza-poisson.tex"))

# Clean latex output
extract_tabular(dir = here("tables/"), regex = "carranza(.*).tex")
convert_longtable(dir = here("tables/"), regex = "carranza(.*).tex")
remove_mathmode(dir = here("tables/"), regex = "carranza(.*).tex")
