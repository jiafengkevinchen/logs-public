# This script contains code for generating results in Section 5.2 of the paper

library(haven)
library(dplyr)
library(tidyr)
library(here)
library(fixest)
library(modelsummary)
options(modelsummary_format_numeric_latex = "mathmode")
library(msm)
library(hdm)
library(purrr)
library(MedBounds) 
library(gt)
library(ggplot2)
source(here("modelsummary-helper.R"))
source(here("extract_tabular.R"))

df <- read_dta(here("sequeira_2016/Replication_Final/Bribes_Regressions.dta"))

# Conversion rate was 24.48 in 2007, according to here:
# https://fxtop.com/en/historical-exchange-rates.php?A=1&C1=USD&C2=MZN&DD1=01&MM1=01&YYYY1=2007&B=1&P=&I=1&DD2=31&MM2=05&YYYY2=2023&btnOK=Go%21


conversion <- 24.48
df <-
    df %>%
    mutate(
        y_orig = exp(lba) - 1,
        y_dollars = y_orig / conversion,
        y_thousands = y_orig / 1000,
        log1plus_dollars = log(1 + y_dollars),
        log1plus_thousands = log(1 + y_thousands),
        log1plus_dollars_thousands = log(1 + y_dollars / 1000)
    )

log1plus_reg <-
    feols(
        fml = lba ~ tariff_change_post2008 + tariff_change_2008 + tariff2007 +
            differentiated + agri + perishable + dfs + factor(clear_agent) + lvalue_tonnage +
            day_w_arrival + psi + monitor + post_2008 + factor(hc_group) +
            hc_4digits + rsa + term,
        data = df,
        cluster = "hc_4digits"
    )

log1plus_reg_dollars <-
    feols(
        fml = log1plus_dollars ~ tariff_change_post2008 + tariff_change_2008 + tariff2007 +
            differentiated + agri + perishable + dfs + factor(clear_agent) + lvalue_tonnage + day_w_arrival + psi +
            monitor + post_2008 + factor(hc_group) +
            hc_4digits + rsa + term,
        data = df,
        cluster = "hc_4digits"
    )

log1plus_reg_thousands <-
    feols(
        fml = log1plus_thousands ~ tariff_change_post2008 + tariff_change_2008 +
            tariff2007 + differentiated + agri + perishable + dfs + factor(clear_agent) +
            lvalue_tonnage + day_w_arrival + psi + monitor + post_2008 +
            factor(hc_group) + hc_4digits + rsa + term,
        data = df,
        cluster = "hc_4digits"
    )

log1plus_reg_dollars_thousands <-
    feols(
        fml = log1plus_dollars_thousands ~ tariff_change_post2008 + tariff_change_2008 +
            tariff2007 + differentiated + agri + perishable + dfs +
            factor(clear_agent) + lvalue_tonnage + day_w_arrival + psi + monitor + post_2008 +
            factor(hc_group) + hc_4digits + rsa + term,
        data = df,
        cluster = "hc_4digits"
    )

# Create a table with log1plus results using different units
# Save as a tex file
# These correspond to in-text results in Section 5.2
modelsummary(
    list(
        log1plus_reg,
        log1plus_reg_dollars,
        log1plus_reg_thousands,
        log1plus_reg_dollars_thousands
    ),
    coef_omit = "^(?!tariff_change_post2008)",
    coef_rename = c("tariff_change_post2008" = "Post x Treatment"),
    digits = 2,
    add_rows = tribble(
        ~term, ~model1, ~model2, ~model3, ~model4,
        "Currency of outcome:", "MZN", "USD", "MZN - Thousands", "USD - Thousands"
    ),
    gof_map = c(""),
    align = "ldddd",
    output = here("tables/sequeira-2016-log1plus.tex")
)


# Run the same regressions but without the additional covariates
# Use the observations as in the regression with covariates
df_basereg <-
    df %>%
    filter(!is.na(lba) & !is.na(tariff_change_post2008) & !is.na(tariff2007) & !is.na(differentiated) &
        !is.na(agri) & !is.na(perishable) & !is.na(dfs) & !is.na(clear_agent) & !is.na(lvalue_tonnage) &
        !is.na(clear_agent) & !is.na(value_tonnage) & !is.na(day_w_arrival) & !is.na(psi) & !is.na(monitor) &
        !is.na(post_2008) & !is.na(hc_group) & !is.na(hc_4digits) & !is.na(rsa) & !is.na(term))


log1plus_reg_nocovariates <-
    feols(
        fml = lba ~ tariff_change_post2008 + tariff_change_2008 + post_2008,
        data = df_basereg,
        cluster = "hc_4digits"
    )

log1plus_reg_dollars_nocovariates <-
    feols(
        fml = log1plus_dollars ~ tariff_change_post2008 + tariff_change_2008 + post_2008,
        data = df_basereg,
        cluster = "hc_4digits"
    )

log1plus_reg_thousands_nocovariates <-
    feols(
        fml = log1plus_thousands ~ tariff_change_post2008 + tariff_change_2008 + post_2008,
        data = df_basereg,
        cluster = "hc_4digits"
    )

log1plus_reg_dollars_thousands_nocovariates <-
    feols(
        fml = log1plus_dollars_thousands ~ tariff_change_post2008 + tariff_change_2008 + post_2008,
        data = df_basereg,
        cluster = "hc_4digits"
    )

# Create a table showing the versions with covariates and without covariates
# Add a row at the end showing whether covariates are included with a N or Y
# Have the baseline model with covariates, then the one without
# covariates, then the dollars version with covariates, etc.
# These correspond to in-text results in Section 5.2

modelsummary(
    list(
        log1plus_reg,
        log1plus_reg_nocovariates,
        log1plus_reg_dollars,
        log1plus_reg_dollars_nocovariates,
        log1plus_reg_thousands,
        log1plus_reg_thousands_nocovariates,
        log1plus_reg_dollars_thousands,
        log1plus_reg_dollars_thousands_nocovariates
    ),
    coef_omit = "^(?!tariff_change_post2008)",
    coef_rename = c("tariff_change_post2008" = "Treated x Post"),
    digits = 2,
    add_rows = tribble(
        ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7, ~model8,
        "Currency of outcome:", "MZN", "MZN", "USD", "USD", "MZN (1000s)", "MZN (1000s)", "USD (1000s)", "USD (1000s)",
        "Covariates included:", "Y", "N", "Y", "N", "Y", "N", "Y", "N"
    ),
    gof_map = c(""),
    output = here("tables/sequeira-2016-log1plus-compare-with-without-covariates.tex")
)

# Run the Poisson model without covariates
poisson_no_covariates <-
    fepois(
        fml = y_orig ~ tariff_change_post2008 + post_2008 + tariff_change_2008,
        data = df_basereg,
        cluster = "hc_4digits"
    )
# The implied ATE%
implied_ATEpercent <-
    exp(coefficients(poisson_no_covariates)["tariff_change_post2008"]) - 1

# The SE, which using delta method is just exp(beta)*SE(beta)
se_ATEpercent <-
    exp(coefficients(poisson_no_covariates)["tariff_change_post2008"]) *
        sqrt(poisson_no_covariates$cov.scaled["tariff_change_post2008", "tariff_change_post2008"])

# Create a table with the means pre/post
pre_post_df <-
    df_basereg %>%
    group_by(post_2008, tariff_change_2008) %>%
    summarise(y = mean(y_orig, na.rm = T)) %>%
    pivot_wider(names_from = post_2008, values_from = y) %>%
    rename(pre = `0`, post = `1`) %>%
    mutate(percentage_change = post / pre)


# Manually verify that the percentage effects line up with Poisson
pre0 <- pre_post_df$pre[pre_post_df$tariff_change_2008 == 0]
pre1 <- pre_post_df$pre[pre_post_df$tariff_change_2008 == 1]
post0 <- pre_post_df$post[pre_post_df$tariff_change_2008 == 0]
post1 <- pre_post_df$post[pre_post_df$tariff_change_2008 == 1]

ImpliedY0_Treated <- pre1 * post0 / pre0
prop_effect <- post1 / ImpliedY0_Treated - 1

# Create a table showing the pre/post means by treatment group
# Save to tables/sequeira-2016-pre-post-means.tex
# This is the bottom panel of Table 6 in the paper
data.frame(
    group = c("Treated", "Control"),
    pre = c(pre1, pre0),
    post = c(post1, post0)
) %>%
    gt() %>%
    cols_label(
        group = "Treatment Group",
        pre = "Pre",
        post = "Post"
    ) %>%
    fmt_number(
        columns = c("pre", "post"),
        decimals = 0
    ) %>%
    gtsave(here("tables/sequeira-2016-pre-post-means.tex"))


# Run Poisson reg with covariates
poisson_with_covariates <- fepois(
    fml = y_orig ~ tariff_change_post2008 + tariff_change_2008 + tariff2007 + differentiated +
        agri + perishable + dfs + factor(clear_agent) + lvalue_tonnage + day_w_arrival + psi +
        monitor + post_2008 + factor(hc_group) + hc_4digits + rsa + term,
    data = df_basereg,
    cluster = "hc_4digits"
)

# The implied ATE%
implied_ATEpercent_covariates <-
    exp(coefficients(poisson_with_covariates)["tariff_change_post2008"]) - 1

# The SE, which using delta method is just exp(beta)*SE(beta)
se_ATEpercent_covariates <- exp(coefficients(poisson_with_covariates)["tariff_change_post2008"]) *
    sqrt(poisson_with_covariates$cov.scaled["tariff_change_post2008", "tariff_change_post2008"])

## Put the Poisson results in a table
modelsum_poisson_no_covariates <- add_prop_effect_to_poisson(poisson_no_covariates,
    covariatesToKeep = c("tariff_change_post2008"),
    treatmentVar = "tariff_change_post2008",
    extraStatsDF = data.frame(Covariates = "N")
)


modelsum_poisson_w_covariates <- add_prop_effect_to_poisson(
    poisson_with_covariates,
    covariatesToKeep = c("tariff_change_post2008"),
    treatmentVar = "tariff_change_post2008"
)


# Table 6 in the paper: Make table of Poisson results
modelsummary(
  list(modelsum_poisson_no_covariates, modelsum_poisson_w_covariates),
  align = "ldd",
  coef_rename = c("tariff_change_post2008" = "Post x Treatment"),
  add_rows = tribble(
    ~term, ~model1, ~model2,
    "Treated Group Means (Pre, Post):", as.character(round(pre1,0)), as.character(round(post1,0)),
    "Treated Group Means (Pre, Post):", as.character(round(pre0,0)), as.character(round(post0,0))
    ),
  # notes = list(paste("Treated Group Means (Pre, Post):  ", as.character(round(pre1,0)), as.character(round(post1,0))),
  #              paste("Control Group Means (Pre, Post):  ", as.character(round(pre0,0)), as.character(round(post0,0)))
  # ),
  output = here("tables/sequeira-2016-poisson.tex")
  
)


# Do log(0)=-x for different calibrations of x
min_y <- min(df_basereg$y_orig[df_basereg$y_orig > 0], na.rm = T)
df_basereg$y_normalized <- df_basereg$y_orig / min_y

my_regression <-
    function(x) {
        df_basereg <-
            df_basereg %>% mutate(my = ifelse(y_normalized == 0,
                -x,
                log(y_normalized)
            ))
        my_reg <-
            feols(
                fml = my ~ tariff_change_post2008 + tariff_change_2008 + tariff2007 + differentiated + agri + perishable + dfs + factor(clear_agent) + lvalue_tonnage + day_w_arrival + psi + monitor + post_2008 + factor(hc_group) + hc_4digits + rsa + term,
                data = df_basereg,
                cluster = "hc_4digits"
            )
        my_reg$x <- x
        return(my_reg)
    }

xvec <- c(0, 0.1, 1, 3)
reg_list <- purrr::map(.x = xvec, .f = my_regression)

# Table 7 in the paper
# Create model summary
# Add labels to the models corresponding to the values in xvec
modelsummary(reg_list,
    coef_omit = "^(?!tariff_change_post2008)",
    gof_map = c("x"),
    align = paste0("l", strrep("d", length(reg_list))),
    coef_rename = c("tariff_change_post2008" = "Post x Treatment"),
    add_rows = tribble(
        ~term, ~model1, ~model2, ~model3, ~model4,
        "Extensive margin value (x):", xvec[1], xvec[2], xvec[3], xvec[4]
    ),
    output = here("tables/sequeira-2016-log0-x.tex")
)

# Figure 2 in the paper: Density of bribe amount
df_basereg %>%
    filter(y_orig > 0) %>%
    ggplot(aes(x = y_orig)) +
    stat_density(color = "gray", fill = "gray") +
    theme_light() +
    theme(
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    xlab("Bribe amount (MZN)") +
    ylab("Density")
ggsave(here("figures/sequeira-density.eps"), width = 6, height = 4)


# Clean up all the latex files created by modelsummary and gt
# I.e. remove the info outside of the tabular environment
# Change longtable to tabular
# Remove math mode
extract_tabular(dir = here("tables/"), regex = "sequeira(.*).tex")
convert_longtable(dir = here("tables/"), regex = "sequeira(.*).tex")
remove_mathmode(dir = here("tables/"), regex = "sequeira(.*).tex")
