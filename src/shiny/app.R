
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
library(ECharts2Shiny)
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
#install.packages("tigris")
library(tigris) #for polygon shape file

## import data
load("data/sc_repay.Rdata")

###Tweets Data
Tweets_state <- readRDS('data/Geotweets_state_cleaned.RDS') %>%
  mutate(id=row_number())

SearchTrend <- read_csv('data/SearchTrend.csv', col_names = TRUE, skip = 2) %>%
  rename(Total_Searches = 'student loan forgiveness: (United States)') %>%
  mutate(Month = zoo::as.yearmon(Month))

Schools <- read_csv('data/2010_2019_student_debt.csv') %>%
  select(INSTNM, CITY,LATITUDE, LONGITUDE, ADM_RATE) %>%
  filter(!is.na(ADM_RATE))

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
### Figure 1: <Interest in #StudentLoanForgiveness over Time>

Trend <- ggplot(SearchTrend,aes(x=Month, y=Total_Searches,group=1)) +
  geom_line(color = "#68A982") +
  scale_color_manual(values="#F2C65F")+
  xlab("") +
  ylab("Total Searches") +
  labs(title="Interest in #StudentLoanForgiveness over Time",caption = "Source: Google Trend Search") +
  theme(text=element_text(size=12,  family="serif"),
        panel.background= element_rect(fill="white"),
        legend.position = "bottom",
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5))

### Figure 2: <Tweets Keywords>
# Use tm package convert dataframe to corpus:
doc_id = c(1:4619)
text_df <- data.frame(doc_id, text = Tweets_state$text, stringsAsFactors = FALSE)

# Convert example_text to a corpus: Success_corpus
tweets_corpus <- VCorpus(DataframeSource(text_df))

corpus <- tm_map(tweets_corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[[:cntrl:]]", "", x))) #remove control characters
corpus <- tm_map(corpus, content_transformer(function(x) gsub("http\\S+", "", x))) #remove website addresses
corpus <- tm_map(corpus, content_transformer(function(x) gsub("@[A-Za-z0-9]+", "", x))) #remove mentions in text
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)

#Document Term Matrix of tweets
tweets_dtm <- DocumentTermMatrix(corpus)

#Create Tidy Dataframe
tweets_tidy <- tidy(tweets_dtm) %>%
  mutate(index = as.numeric(document)) %>%
  left_join(Tweets_state, by=c("index"="id")) %>%
    mutate(state = ifelse(state=="new york:long island","new york",
                        ifelse(state=="new york:main","new york",
                        ifelse(state=="new york:main","new york",
                        ifelse(state=="new york:manhattan","new york",
                        ifelse(state=="north carolina:main","north carolina",
                        ifelse(state=="massachusetts:main","massachusetts",
                        ifelse(state=="michigan:north","michigan",
                        ifelse(state=="michigan:south","michigan",
                        ifelse(state=="virginia:main", "virginia",
                        ifelse(state=="washington:main","washington",state)))))))))))
tweets_tidy_wc <- tweets_tidy %>%
  group_by(term) %>%
  summarize(n = sum(count)) %>%
  arrange(desc(n)) %>%
  filter(! term %in% c("student","loan","debt","cancelstudentdebt","amp","forgiveness","like","can","just","get","will"))

 # Create a wordcloud with wesanderson palette
Twitter_wd <- wordcloud(tweets_tidy_wc$term, tweets_tidy_wc$n,
       max.words = 100, colors = wes_palette(name="Royal2"),
       family = "serif")

### Figure 3: <#CancelStudentDebt Tweets in the US - Location of Selectice Institutions>

#Get list of top words by state
State_top_words <- tweets_tidy %>%
  select(term, count, state) %>%
  group_by(state, term) %>%
  summarise(total=sum(count)) %>%
  arrange(desc(total)) %>%
  filter(! term %in% c("student","loan","debt","cancelstudentdebt","amp")) %>%
  group_by(state) %>%
  slice_max(order_by = total, n=15) %>%
  select(-total) %>%
  summarise(Terms = list(term))

# Get Bing lexicon
bing <- get_sentiments("bing")

# Join text to lexicon
Tweets_bing <- inner_join(tweets_tidy, bing, by = c("term" = "word")) %>%
   # Count by sentiment, index, document
  count(sentiment,index,document, text) %>%
   # Spread sentiments
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive-negative)

# Join sentiment score with original twitter data fame
Tweets_sentiment <- Tweets_state %>%
  inner_join(Tweets_bing, by = c("id" = "index")) %>%
  select(-c(text.y,document, size=favorite_count))

# Student Loan Forgiveness & Selective Institutions Map
# count by state
tw_state <- Tweets_sentiment %>%
  mutate(state = ifelse(state=="new york:long island","new york",
                        ifelse(state=="new york:main","new york",
                        ifelse(state=="new york:main","new york",
                        ifelse(state=="new york:manhattan","new york",
                        ifelse(state=="north carolina:main","north carolina",
                        ifelse(state=="massachusetts:main","massachusetts",
                        ifelse(state=="michigan:north","michigan",
                        ifelse(state=="michigan:south","michigan",
                        ifelse(state=="virginia:main", "virginia",
                        ifelse(state=="washington:main","washington",state))))))))))) %>%
  group_by(state) %>%
  mutate(N=n(), T.Sentiment=sum(sentiment), WordCounts = sum(display_text_width)) %>%   #get total number of tweets & sum of sentiment score by state
  select(display_text_width, lon, lat, state, T.Sentiment, N, WordCounts) %>%  #lon & lat are vary within each state.Keep first one.
  group_by(state, lon, lat) %>%
  summarise(Avg.Sentiment=T.Sentiment/N, WordCounts=WordCounts) %>% #calculate average sentiment score by state and keep total word counts by state
  ungroup(lon,lat) %>%
  filter(row_number()==1) %>%
  mutate(lon=ifelse(state=="new york",-73.935242, lon)) %>%  #change coordinates for New York State
  mutate(lat=ifelse(state=="new york",40.730610,lat)) %>%
  filter(!is.na(state)) %>%
  left_join(State_top_words, by="state")

# Polygon stuff from shape file
states <- states(cb=T) %>%
  mutate(name = tolower(NAME))

# Use the Tigris function geo_join to bring together the states shapefile and the tw_states dataframe
states_shape_tweets <- geo_join(states, tw_state, "name", "state")

#Get Locations of Selective Schools
SelSchools <-Schools %>%
  mutate(Selectivity = case_when(
    ADM_RATE < 0.1 ~ 'elite',
    ADM_RATE < 0.2 ~ 'highly selective',
    ADM_RATE < 0.3 ~ 'selective',
    ADM_RATE < 0.7 ~ 'less selective',
    TRUE ~ 'not selective')) %>%
  filter(Selectivity %in% c('elite', 'highly selective','selective'), !is.na(LONGITUDE)) %>%
  distinct(INSTNM, .keep_all=TRUE)

#Twitter Sentiment Map & Institutions'Location Map

# Creating a color palette based on the number range in the total column
ppal <- colorNumeric("RdYlGn", domain=states_shape_tweets$Avg.Sentiment)

# Creating a color palette based on Selectivity of schools
ppal2 <- colorFactor(palette = c("purple", "forestgreen", "deepskyblue3"),
               levels = c("elite", "highly selective", "selective"))

icon.pop <- pulseIcons(color = ifelse(SelSchools$Selectivity == "elite", "#E64141",
                                      ifelse(SelSchools$Selectivity =="highly selective", "#F9C874", "#97D6FF")))

# Setting up the pop up text
popup_sb <- paste0("State: ", as.character(states_shape_tweets$NAME),"<br/>",
                  "Average Sentiment Score: ", as.character(states_shape_tweets$Avg.Sentiment),"<br/>",
                  "Total Word Count: ",as.character(states_shape_tweets$WordCounts),"<br/>",
                  "Top Words: ",as.character(states_shape_tweets$Terms),"<br/>")

popup_sb2 <- paste0("University: ", as.character(SelSchools$INSTNM),"<br/>",
                  "City: ", as.character(SelSchools$CITY),"<br/>",
                  "Admission Rate: ",as.character(SelSchools$ADM_RATE))

#Setting up color for awesome markers
getColor <- function(SelSchools) {
  sapply(SelSchools$ADM_RATE, function(ADM_RATE) {
  if(ADM_RATE <= 0.1) {
    "purple"
  } else if(ADM_RATE <= 0.2) {
    "green"
  } else {
    "blue"
  } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(SelSchools)
)


# Map title
map_title2 <- tags$p(tags$style('p {color: black; font-size: 14px; family: serif}'),
                    tags$b('#CancelStudentDebt Tweets in the US - Location of Selective Institutions'))

#Build Tweets Map
TweetsMap <- leaflet() %>%
  setView(-98.483330, 38.712046, zoom = 4) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
    attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>', group = "Dark") %>%
#Overlay Group
  addAwesomeMarkers(data = SelSchools,
                    lng = ~LONGITUDE, lat = ~ LATITUDE,
                    icon= icons,
                    popup = popup_sb2,
                    group = 'Selective Institutions') %>%
  addLegend(position = 'bottomright',
            title = 'Selectivity',
            pal = ppal2,
            values = SelSchools$Selectivity,
            opacity = 0.7) %>%
  addCircles(data= states_shape_tweets,
             lng = ~lon, lat = ~lat,
             weight = 2,
             radius = states_shape_tweets$WordCounts*2.8,
             popup=~popup_sb,
             color=~ppal(states_shape_tweets$Avg.Sentiment),
             stroke = TRUE,
             group = 'Twitter Sentiments',
             fillOpacity = 1) %>%
  addLegend(position = "bottomleft",
            title = "Average Sentiment Score",
            pal = ppal,
            values = states_shape_tweets$Avg.Sentiment,
            opacity = 0.7) %>%
  addLayersControl(
          overlayGroups =c("Twitter Sentiments", "Selective Institutions"),
          options = layersControlOptions(collapsed=FALSE),
          position = "bottomright") %>%
  #Add Map Title
  addControl(map_title2,
             position = 'topright') %>%
  addResetMapButton()

### UI ###
