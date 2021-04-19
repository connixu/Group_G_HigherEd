
## Group G: Student Loans

# set up
## please add your required things here
library(tidyverse)
library(ggplot2)
library(ggridges)
library(magrittr)
library(stats)
library(manifestoR)
library(readtext)
library(SnowballC)
library(tidyr)
library(tidytext)      # for data manipulation
library(qdapDictionaries)
library(base64enc)
library(RColorBrewer)
#install.packages("wesanderson")
library(wesanderson)
library(rtweet)
library(ggmap)
library(sp)
library(leaflet)
library(tm)
library(htmlwidgets)
library(htmltools)
library(leaflet.extras)
library(zoo)
library(wordcloud)
library(wesanderson)
library(lubridate)


## import data
load("data/sc_repay.Rdata")

## save styles and themes
source("ourtheme.R")


### Plots ###

# Student Loans
## Arielle: Repayment Rates
### Figure 1: Repayment Rates Median by Class
totals <- select(sc_repay, contains("count"))/select(sc_repay, contains("rate"))
totals <- setNames(totals, gsub("count", "total", colnames(totals)))

repay_rate_ave <- sc_repay %>%
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

### figure 2: repayment rate distributions
repay_rate_dist <- sc_repay %>% filter(years_since_entering_repay == 1) %>%
  # data wrangle
  pivot_longer(cols = c("repay_rate_lowincome", "repay_rate_midincome", "repay_rate_highincome"),
               names_to = "class", values_to = "rates") %>%
  filter(rates > 0) %>%
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


## Connie: Student Debts
### Figure 1: <put title here>
# <put code here>
### Figure 2: <put title here>
# <put code here>
### Figure 3: <put title here>
# <put code here>
### Figure 4: <put title here>
# <put code here>
### Figure 5: <put title here>
# <put code here>

# Tweets
## Grace: Tweets and stuff
### Figure 1: <put title here>
# <put code here>
### Figure 2: <put title here>
# <put code here>
### Figure 3: <put title here>
# <put code here>


### UI ###
