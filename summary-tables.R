library(here)
library(dplyr)
library(readr)
library(gt)
library(ggplot2)
library(readxl)
library(ggplot2)
library(RColorBrewer)
source(here("extract_tabular.R"))


# Import results from stata
stata_results <- read_csv(here("results/arcsinh-transformation-results.csv"))
stata_results$paperID <- 1:nrow(stata_results) # add paperID

# Import SEs from stata
se_results <- read_csv(here("results/arcsinh-transformation-ses.csv"))
se_results$paperID <- 1:nrow(se_results) # add paperID


# Import info on replicated papers
gsheet_info <- read_xlsx(here("ihs_papers.xlsx"))

# Remove P&P's and papers without IHS on LHS
gsheet_info <-
  gsheet_info %>%
  filter(is.na(`AERI or P&P?`)) %>%
  filter(`Puts IHS on the LHS` == "Yes")

# Extract author info
gsheet_info <-
  gsheet_info %>%
  mutate(Authors = gsub(pattern = ".* by (.*)", replacement = "\\1", x = Paper))

# Merge author info with Stata_results
stata_results <-
  left_join(stata_results,
    gsheet_info %>% select(Authors, ID_rep),
    by = c("paperID" = "ID_rep")
  )

stata_results <-
  stata_results %>%
  mutate(RawChange = YTimes100 - OriginalY) %>%
  mutate(PercentChange = 100 * RawChange / OriginalY)

# Table 1 in the paper
stata_results %>%
  select(-paperID) %>%
  select(Authors, OriginalY, YTimes100, ExtensiveMargin, RawChange, PercentChange) %>% # put authors first
  arrange(-abs(PercentChange)) %>%
  gt() %>%
  fmt_number(columns = -c(Authors, PercentChange), decimals = 3) %>%
  fmt_number(columns = PercentChange, decimals = 0) %>%
  tab_spanner(columns = c(OriginalY, YTimes100), label = "Treatment Effect Using:") %>%
  tab_spanner(columns = c(RawChange, PercentChange), label = "Change from rescaling units:") %>%
  cols_label(
    OriginalY = "arcsinh(Y)",
    YTimes100 = "arcsinh(100 Y)",
    ExtensiveMargin = "Ext. Margin",
    RawChange = "Raw",
    PercentChange = "%",
    Authors = "Paper"
  ) %>%
  gtsave(here("tables/change-from-a100-table.tex"))


# Appendix Table 2 in the paper
stata_results %>%
  select(-paperID) %>%
  select(Authors, Log1pY, Log1p100Y, ExtensiveMargin) %>% # put authors first
  mutate(RawChange = Log1p100Y - Log1pY) %>%
  mutate(PercentChange = 100 * RawChange / Log1pY) %>%
  arrange(-abs(PercentChange)) %>%
  gt() %>%
  fmt_number(columns = -c(Authors, PercentChange), decimals = 3) %>%
  fmt_number(columns = PercentChange, decimals = 0) %>%
  tab_spanner(columns = c(Log1pY, Log1p100Y), label = "Treatment Effect Using:") %>%
  tab_spanner(columns = c(RawChange, PercentChange), label = "Change from rescaling units:") %>%
  cols_label(
    Log1pY = "log(1+Y)",
    Log1p100Y = "log(1+100Y)",
    ExtensiveMargin = "Ext. Margin",
    RawChange = "Raw",
    PercentChange = "%",
    Authors = "Paper"
  ) %>%
  gtsave(here("tables/change-from-log1p100-table.tex"))


# Clean up all the latex files created by gt
# I.e. remove the info outside of the tabular environment
# Change longtable to tabular
# Remove math mode
convert_longtable(dir = here("tables/"), regex = "change-from(.*).tex")
extract_tabular(dir = here("tables/"), regex = "change-from(.*).tex")
remove_mathmode(dir = here("tables/"), regex = "change-from(.*).tex")



# Figure 1 in the paper
stata_results %>%
  mutate(predictedChange = abs(log(100) * ExtensiveMargin)) %>%
  ggplot(aes(x = predictedChange, y = abs(RawChange))) +
  geom_point() +
  xlab("|Extensive Margin| * log(100)") +
  ylab("Actual Absolute Change") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  annotate("text", x = 0.4, y = 0.55, label = "45 Deg Line")

ggsave(here("figures/change-from-a100-actual-vs-predicted.eps"), width = 6, height = 4)


# Appendix Table 1 in the paper
gsheet_info %>%
  select(
    Authors,
    `Interprets Units as Percent`,
    `Quote About Percents / Notes`,
    `Original Units`
  ) %>%
  arrange(desc(`Interprets Units as Percent`), Authors) %>%
  rename(Paper = Authors) %>%
  mutate(`Quote About Percents / Notes` = ifelse(is.na(`Quote About Percents / Notes`), " ", `Quote About Percents / Notes`)) %>%
  gt() %>%
  gtsave(here("tables/asinh-quotes.pdf"))



# Appendix Figure 1 in the paper
## Merge in the SEs
stata_results <- left_join(stata_results, se_results,
  by = "paperID"
)

stata_results <-
  stata_results %>%
  mutate(
    tstat_extensive = ExtensiveMargin / SE_ExtensiveMargin,
    tstat_OriginalY = OriginalY / SE_OriginalY,
    tstat_YTimes100 = YTimes100 / SE_YTimes100
  )

stata_results %>%
  select(ExtensiveMargin, contains("tstat"))

pallete <- RColorBrewer::brewer.pal(3, "Set1")

stata_results %>%
  ggplot(data = ., aes(x = tstat_extensive)) +
  geom_segment(
    aes(y = tstat_OriginalY, xend = tstat_extensive, yend = tstat_YTimes100),
    arrow = arrow(length = unit(0.05, "inches")), color = pallete[2]
  ) +
  geom_point(aes(y = tstat_OriginalY), color = pallete[1]) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("t-stat for Extensive Margin") +
  ylab("t-stat for ATE using arcsinh") +
  annotate(geom = "text", x = -0.2, y = 6, label = "Original units", color = pallete[1]) +
  annotate(geom = "text", x = -0.2, y = 3.5, label = "Units x 100", color = pallete[2]) +
  annotate("text", x = 7.5, y = 6, label = "45 Deg Line") +
  xlim(-8, 8) +
  ylim(-8, 8)

ggsave(here("figures/change-in-tstat-vs-extensive-margin.eps"), width = 6, height = 4)
