---
title: "Repay Rate"
author: Arielle Herman
date: 2021-04-04
always_allow_html: yes
output: 
  html_document:
    keep_md: true
    toc: true
---


```r
library(ggplot2)
library(tidyverse)
source("ourtheme.R")

filepath <- paste0(stringr::str_remove(getwd(),"visuals"), "data/sc_repay.Rdata")
if(file.exists(filepath)) {
  load("../data/sc_repay.Rdata")
} else {
  sc_repay <- educationdata::get_education_data(level = 'college-university', source = 'scorecard',
                                              topic = 'repayment', filters = list(year = 2007:2016),
                                              add_labels = TRUE)
  save(sc_repay, file = "../data/sc_repay.Rdata")
}

scorecard <- readr::read_csv("../data/2019_College_Scorecard_Valid_Admissions_Data.csv")

selectivity <- scorecard %>%
  dplyr::transmute(OPEID, uni_rank = case_when(
    #ADM_RATE < 0.05 ~ 'elite',
    ADM_RATE < 0.2 ~ 'highly selective',
    ADM_RATE < 0.3 ~ 'more selective',
    ADM_RATE < 0.5 ~ 'selective',
    ADM_RATE < 0.7 ~ 'less selective',
    TRUE ~ 'not selective')) %>%
  mutate(uni_rank = factor(uni_rank, levels=c('not selective', 'less selective', 'selective', 'more selective', 'highly selective', 'elite')))
```


```r
#selective_schools <- na.omit(scorecard$OPEID[scorecard$ADM_RATE < 0.3])
#question: select for nonprofit and private schools?
# fraction of repayment cohort that is not currently in default

sc_repay %>%
  # data wrangle
  #filter(years_since_entering_repay <= 1) %>%
  pivot_longer(cols = c("repay_rate_lowincome", "repay_rate_midincome", "repay_rate_highincome"),
               names_to = "class", values_to = "rates") %>%
  filter(rates > 0) %>%
  ## order levels to apply to multiple commands in ggplot
  mutate(class = factor(class, levels =
                          c("repay_rate_highincome", "repay_rate_midincome", "repay_rate_lowincome"))) %>%
  
  # plot
  ggplot(aes(x = year, y = rates, group = cohort_year, fill = class)) +
  geom_violin(color = "lightgray") + facet_grid(. ~ class) +
  ## themes
  ggthemes::theme_tufte() + # add base theme first and then modify
  theme(panel.spacing = unit(1, "cm"),
        panel.border = element_rect(colour = "lightgray", fill=NA, size=1),
        legend.position = "bottom",
        plot.title = element_text(hjust=0.5),
        strip.text.x = element_blank(), axis.ticks = element_blank()) +
  ## labels
  ggtitle("Fraction Of Students In Successive Fiscal Cohorts\nWhose Loan Balances Have Declined") +
  xlab(NULL) + ylab(NULL) +
  scale_fill_viridis_d(name = "Income Range:", labels = c("High Income", "Middle Income", "Low Income")) +
  ## percents
  scale_y_continuous(labels = scales::dollar_format(suffix = "%", prefix = "")) +
  scale_y_continuous(labels = scales::percent_format())
```

![](repay_rate_files/figure-html/repay_rate_dist_draft1_violin-1.png)<!-- -->



```r
totals <- select(sc_repay, contains("count"))/select(sc_repay, contains("rate"))
totals <- setNames(totals, gsub("count", "total", colnames(totals)))

sc_repay %>%
  # data wrangle
  cbind(totals) %>% filter(repay_rate > 0,
                           repay_rate_midincome > 0, repay_rate_lowincome > 0, repay_rate_highincome > 0,
                           years_since_entering_repay == 1) %>%
  group_by(cohort_year) %>% summarize(low = sum(repay_count_lowincome)/sum(repay_total_lowincome),
                                 med = sum(repay_count_midincome)/sum(repay_total_midincome),
                                 high = sum(repay_count_highincome)/sum(repay_total_highincome)) %>%
  pivot_longer(cols = c("low", "med", "high"), names_to = "class", values_to = "rates") %>%
  mutate(class = factor(class, levels = c("high", "med", "low"))) %>%
  
  # plot
  ggplot(aes(x = cohort_year, y = rates, group = class, color = class)) +
  geom_line() + geom_point() +
  
  # labels
  ggtitle("Cohort Performance in First Year of Repayment") +
  xlab("\nFiscal Cohort Year") + ylab("Average Proportion of Fiscal Cohort\nto Decline Loan Balance\n") +
  labs(caption = "\n*Fiscal Cohort: group of students who begin repaying their loans in the same year") +
  scale_color_brewer(name = "Family Income:", labels = c("High", "Medium", "Low"), palette = "greenScale", direction = -1) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "%", prefix = "")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  ourtheme
```

```
## Warning in pal_name(palette, type): Unknown palette greenScale
```

```
## Scale for 'y' is already present. Adding another scale for 'y', which will
## replace the existing scale.
```

![](repay_rate_files/figure-html/repay_rate_ave-1.png)<!-- -->



```r
sc_repay %>%
  # data wrangle
  filter(repay_rate > 0, years_since_entering_repay == 1) %>%
  group_by(cohort_year, year, years_since_entering_repay) %>%
  summarize_if(is.numeric, median) %>% select(contains("year"), contains("rate")) %>%
  pivot_longer(cols = c("repay_rate_lowincome", "repay_rate_midincome",
                        "repay_rate_highincome", "repay_rate"),
               names_to = "class", values_to = "median_rate") %>%
  mutate(total = class == "repay_rate",
         class = factor(class, c("repay_rate", "repay_rate_highincome",
                                 "repay_rate_midincome", "repay_rate_lowincome"))) %>%
  # plot
  ggplot(aes(x = cohort_year, y = median_rate, group = class, color = class, alpha = total)) +
  geom_point() + geom_line() +
  #facet_wrap(. ~ factor(total, c(TRUE, FALSE))) +
  ## labels
  ggtitle("Fraction of Students whose Loan Balances\nDeclined in their First Year of Repayment") +
  xlab("\nCohort Year") + ylab("Median Fraction of Fiscal Cohort\n") + guides(alpha = FALSE) +
  scale_color_viridis_d(name = "Family Income:",
                        labels = c("Overall", "High", "Middle", "Low")) +
  #scale_y_continuous(labels = scales::dollar_format(suffix = "%", prefix = "")) +
  #scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_alpha_manual(values = c(0.5, 1)) +
  ## themes
  ourtheme +
  theme(panel.spacing = unit(1, "cm"))#
```

![](repay_rate_files/figure-html/repay_rate_ave_draft-1.png)<!-- -->

```r
        #panel.border = element_rect(colour = "lightgray", fill=NA, size=1)
```


```r
sc_repay %>% filter(years_since_entering_repay == 1) %>%
  
  # data wrangle
  #filter(years_since_entering_repay <= 1) %>%
  pivot_longer(cols = c("repay_rate_lowincome", "repay_rate_midincome", "repay_rate_highincome"),
               names_to = "class", values_to = "rates") %>%
  filter(rates > 0) %>%
  ## order levels to apply to multiple commands in ggplot
  mutate(class = factor(class, levels =
                          c("repay_rate_highincome", "repay_rate_midincome", "repay_rate_lowincome"))) %>%
  
  # plot
  ggplot(aes(x = rates, y = fct_rev(factor(cohort_year)), fill = class)) +
  ggridges::geom_density_ridges(scale = 2, alpha = 0.80) +
  
  # labels
  ggtitle("Proportion of University Fiscal Cohorts Whose Loan\nBalances Declined in Year One of Repayment\n") +
  ylab("Fiscal Cohort\n") + xlab(NULL) +
  scale_fill_brewer(name = "Family Income:", labels = c("High", "Middle", "Low"), palette = "greens", direction = -1) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "%", prefix = "")) +
  scale_x_continuous(labels = scales::percent_format()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  
  ourtheme
```

```
## Warning in pal_name(palette, type): Unknown palette greens
```

```
## Scale for 'x' is already present. Adding another scale for 'x', which will
## replace the existing scale.
```

```
## Picking joint bandwidth of 0.0254
```

![](repay_rate_files/figure-html/repay_rate_dist-1.png)<!-- -->



```r
sc_repay %>%
  filter(years_since_entering_repay == 1, repay_rate > 0) %>%
  
  # plot
  ggplot(aes(x = repay_rate, y = fct_rev(factor(cohort_year)), fill = stat(x))) +
  scale_fill_distiller(name = "Repay Rate", palette = "greens") +
  ggridges::geom_density_ridges_gradient(scale = 2, alpha = 0.50) +
  
  # labels
  ggtitle("Proportion of University Fiscal Cohorts Whose Loan\nBalances Declined in Year One of Repayment\n") +
  ylab("Fiscal Cohort Year\n") + xlab(NULL) +
  #scale_fill_distiller(direction = -1) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "%", prefix = "")) +
  scale_x_continuous(labels = scales::percent_format()) +
  #guides(fill = guide_legend(reverse = TRUE)) +
  
  ourtheme + theme(legend.position = "none")
```

```
## Warning in pal_name(palette, type): Unknown palette greens
```

```
## Scale for 'x' is already present. Adding another scale for 'x', which will
## replace the existing scale.
```

```
## Picking joint bandwidth of 0.0293
```

![](repay_rate_files/figure-html/repay_rate_dist_draft2_ggridges-1.png)<!-- -->



```r
sc_repay %>% mutate(opeid = as.integer(opeid)) %>%
  left_join(selectivity, by = c("opeid" = "OPEID")) %>%
  filter(years_since_entering_repay == 1) %>%
  select(repay_rate, uni_rank, cohort_year, year, opeid) %>%
  mutate(admission_half = ifelse(uni_rank %in% c("elite", "highly selective", "more selective"), 1, 0)) %>%
  na.omit() %>% # why are there so many NAs?
  # data wrangle
  #filter(years_since_entering_repay <= 1) %>%
  #pivot_longer(cols = c("repay_rate_lowincome", "repay_rate_midincome", "repay_rate_highincome"),
  #             names_to = "class", values_to = "rates") %>%
  filter(repay_rate > 0, uni_rank != "not selective") %>%
  # see number of unis
  #select(opeid) %>% unique() %>% nrow()
  ## order levels to apply to multiple commands in ggplot
  #mutate(class = factor(class, levels =
  #                        c("repay_rate_highincome", "repay_rate_midincome", "repay_rate_lowincome"))) %>%
  
  # plot
  ggplot(aes(x = repay_rate, y = fct_rev(factor(cohort_year)), fill = uni_rank)) +
  ggridges::geom_density_ridges(scale = 2, color = "darkgray", alpha = 0.50) +
  #facet_wrap(. ~ admission_half) +
  
  # labels
  ggtitle("Proportion of University Fiscal Cohorts\nWhose Loan Balances Declined") +
  ylab("Fiscal Cohort\n") + xlab(NULL) +
  scale_fill_viridis_d(name = "Selectivity:", labels = c("Less", "Selective", "More", "Highly"), direction = 1) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "%", prefix = "")) +
  scale_x_continuous(labels = scales::percent_format()) +
  #guides(fill = guide_legend(reverse = TRUE)) +
  # customized theme
  ourtheme
```

```
## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
```

```
## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion to integer
## range
```

```
## Scale for 'x' is already present. Adding another scale for 'x', which will
## replace the existing scale.
```

```
## Picking joint bandwidth of 0.0484
```

![](repay_rate_files/figure-html/repay_rate_dist_draft3_ggridges-1.png)<!-- -->
