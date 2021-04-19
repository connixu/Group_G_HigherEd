# Install Libraries and Packages 
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
library(dplyr)
library(tidyverse)
library(XML)
library(RCurl)
library(readr)
library("readxl")
# install.packages (themes)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(viridis)
library(hrbrthemes)
library(plotly)
library(RgoogleMaps)
library(ggmap)
install.packages("maps")
install.packages("tmap") # install the CRAN version
library(tmap)
install.packages('rgeos')
library(devtools)
install.packages('leaflet')
library('scales')
install.packages('DT')
library(DT)
install.packages('treemap')
install.packages('highcharter')
library(highcharter)
library(treemap)
library(leaflet.providers)
library(leaflet)
install.packages('quantmod')


# Install Data 
#<<<<<<< Updated upstream
setwd("~/Dropbox (Business)/Spring 2021/QMSS 5063 - Data Visualization /Group_G_HigherEd/src/shiny") #to get rid of later
sc_time <- read.csv('data/2010_2019_student_debt.csv') 
#=======
setwd("~/Dropbox (Business)/Spring 2021/QMSS 5063 - Data Visualization /Group_G_HigherEd") #to get rid of later
sc_time <- read.csv('/src/2010_2019_student_debt.csv') 
#>>>>>>> Stashed changes
sc_time<- sc_time %>% subset(DEBT_MDN !='PrivacySuppressed') %>% 
  transform(DEBT_MDN = as.numeric(DEBT_MDN)) %>% 
  dplyr::mutate(DEBT_MDN = ifelse(is.na(DEBT_MDN), 0, DEBT_MDN)) %>% 
  mutate(DEBT_MDN_STUDENT = DEBT_MDN*UGDS)
sc <- sc_time %>% filter(Year_Ending == 2019)
sc <- sc %>%
  dplyr::mutate(uni_rank = case_when(
    ADM_RATE < 0.2 ~ 'highly selective/elite',
    ADM_RATE < 0.3 ~ 'more selective',
    ADM_RATE < 0.5 ~ 'selective',
    ADM_RATE < 0.7 ~ 'less selective',
    TRUE ~ 'not selective')) %>% mutate(uni_rank = factor(uni_rank, levels=c('not selective', 'less selective', 'selective', 'more selective', 'highly selective/elite')))

# Data Table table_1
sc_dt <- sc %>% subset(DEBT_MDN !='PrivacySuppressed') %>% transform(DEBT_MDN = as.numeric(DEBT_MDN)) %>% group_by(uni_rank) %>% mutate(`Number of Universities` = n()) %>% ungroup() %>% mutate(DEBT_MDN_STUDENTS = DEBT_MDN*UGDS) %>% group_by(uni_rank) %>% mutate(`Median Student Loans` = paste('$',round(sum(DEBT_MDN_STUDENTS, na.rm=TRUE)/sum(UGDS, na.rm=TRUE),2))) %>%
  mutate(`Min Acceptance Rate` = percent(min(ADM_RATE))) %>% mutate(`Max Acceptance Rate` = percent(max(ADM_RATE))) %>% ungroup() %>%
  group_by(uni_rank,`Median Student Loans`,`Number of Universities`,`Min Acceptance Rate`,`Max Acceptance Rate`) %>%
  summarize() %>% dplyr::rename(`University Selectivity` = uni_rank)

table_1 <- datatable(sc_dt,style = "default",filter = 'top',  caption = 'Universities and Selectivity')

# Tree Map treemap
treemap <- sc_dt %>% dplyr::mutate(Description=paste(`University Selectivity`, '\n',`Number of Universities`,'Universities'), sep ="\n") %>%
  treemap(index="Description",
          vSize="Number of Universities",
          type="index",
          fontsize.labels=c(12, 8),
          palette =  viridis(5),
          border.col="white",
          title = 'Universities and Selectivity')

# Scatter Plot admissions_scatter
ShortPuBuGn <- c("#D0D1E6","#A6BDDB","#67A9CF","#3690C0","#02818A")
admissions_scatter <- sc %>% subset(DEBT_MDN !='PrivacySuppressed') %>% transform(DEBT_MDN = as.numeric(DEBT_MDN)) %>%
  ggplot(., aes(x=ADM_RATE, y=DEBT_MDN,color=uni_rank)) +
  geom_point(pch=21) +
  geom_smooth(color='navy', se = FALSE) +
  scale_color_manual(values=ShortPuBuGn)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background= element_rect(fill="white")) +
  scale_y_discrete(limits=c(0,10000,20000,30000), labels=c('0','10','20','30')) +
  labs(x='Admissions Rate', y='Median Loan Amount per Student\n(thousands)',
       title='Student Debt and Admissions Rate',
       color='Selectivity')


# Line Graph plot_line_plotly
  # CPI Inflation Rates - Got Average Yearly Inflation Rate for Scaling for Student Debt 
library(quantmod)
getSymbols("CPIAUCSL", src='FRED')
avg.cpi <- apply.yearly(CPIAUCSL, mean)
cf <- as.data.frame(avg.cpi/as.numeric(avg.cpi['2009'])) 
cf$Year_Ending <- format(as.Date(row.names(cf), format="%Y-%m-%d"),"%Y")
  # Merged for Inflation 
sc_time_df <- sc_time %>% group_by(`Year_Ending`) %>% mutate(`Average Annual Student Debt - National` = sum(DEBT_MDN_STUDENT,na.rm=TRUE)/sum(UGDS,na.rm=TRUE)) %>% ungroup() %>% 
  dplyr::mutate(uni_rank = case_when(
    ADM_RATE < 0.2 ~ 'elite/highly selective',
    ADM_RATE < 0.3 ~ 'more selective',
    ADM_RATE < 0.5 ~ 'selective',
    ADM_RATE < 0.7 ~ 'less selective',
    TRUE ~ 'not selective')) %>%
  mutate(uni_rank = factor(uni_rank, levels=c('not selective', 'less selective', 'selective', 
                                              'more selective', 'elite/highly selective'))) %>%
  group_by(uni_rank,Year_Ending) %>% 
  mutate(`Average Annual Student Debt (by Selectivity)` = sum(DEBT_MDN_STUDENT,na.rm=TRUE)/sum(UGDS,na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(`Year_Ending`,`Average Annual Student Debt (by Selectivity)`,
           uni_rank,`Average Annual Student Debt - National`) %>% summarize() %>% 
  merge(cf) %>% 
  mutate(`Adjusted Average Annual Student Debt` = `Average Annual Student Debt (by Selectivity)`/
           CPIAUCSL) %>% 
  mutate(`Adjusted Average Annual Student Debt - Composite` = `Average Annual Student Debt - National`/
           CPIAUCSL)

sc_df <- sc_time_df %>% group_by(`Average Annual Student Debt - National`,`Adjusted Average Annual Student Debt - Composite`,Year_Ending) %>% summarize() %>% mutate(uni_rank='national average') %>% mutate(`Adjusted Average Annual Student Debt`=`Adjusted Average Annual Student Debt - Composite`) %>% dplyr::mutate(`Average Annual Student Debt (by Selectivity)` = `Average Annual Student Debt - National`) %>% merge(cf) %>% select(Year_Ending,`Average Annual Student Debt (by Selectivity)`, uni_rank, `Average Annual Student Debt - National`, CPIAUCSL, `Adjusted Average Annual Student Debt`,`Adjusted Average Annual Student Debt - Composite`)
sc_time_df <- sc_time_df %>% rbind(sc_df) %>% mutate(uni_rank = factor(uni_rank, levels=c('national average','not selective', 'less selective', 'selective', 'more selective', 'elite/highly selective'))) %>% 
  mutate(`Group Level` = ifelse(uni_rank == 'national average', 'National','Selectivity'))

plot_line <- sc_time_df %>% 
  ggplot(.,aes(x=Year_Ending,y=`Adjusted Average Annual Student Debt`, color=uni_rank)) + 
  geom_line(aes(linetype=`Group Level`)) + 
  scale_color_manual(values=c('grey',"#D0D1E6","#A6BDDB","#67A9CF","#3690C0","#02818A"))+
  scale_linetype_manual(values=c("solid", "dotted"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background= element_rect(fill="white")) +
  scale_x_continuous(breaks = round(seq(min(sc_time$Year_Ending), max(sc_time$Year_Ending), by = 2),1)) +
  labs(x='', y='Inflation-Adjusted Median Loan Amount per Student\n(thousands)', 
       title='Student Debt Has Been Rising Over The Years',
       color='',fill='',group='',linetype='')
plot_line_plotly <- ggplotly(plot_line)


#Chloropleth
library(reshape2)
library(shiny)
ui <- fluidPage(
  titlePanel("I love Graphs about Student Debt"),
  # selectInput('year',
  # 'Year',
  # choices=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)), 
  sliderInput(inputId = "year",
              label = "Year",
              value = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)),
  leafletOutput("studentdebtmap")
  
)

server <- function(input, output, session){
  # Render plot of top 10 most popular names
  sc_time_selective <- reactive({sc_time %>% subset(Year_Ending == input$year) %>% 
      group_by(STABBR) %>% mutate(`Average Student Loans`=sum(DEBT_MDN_STUDENT,na.rm=TRUE)/sum(UGDS,na.rm=TRUE)) %>%
      group_by(STABBR,`Average Student Loans`) %>% summarize()})
  library(tigris)
  states <- states(cb = TRUE)
  states_year <- reactive({states %>% inner_join(sc_time_selective(), by=c(STUSPS='STABBR'))}) 
  
  output$studentdebtmap <- renderLeaflet({
    pal = colorFactor('Greens', domain = states_year()$`Average Student Loans`)
    pop_pop <- paste("State:",states_year()$NAME,"<br/>",
                     "Average Student Loans","<br/>", "of Schools Located in State:",
                     paste('$',round(states_year()$`Average Student Loans`)))
    
    sc_time_selective_title <- tags$p(tags$style('p{color:gray; font-size: 14px; family: serif}'),
                                      tags$b('Average Debt By State (2019)'))
    leaflet(states_year()) %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(states_year()$`Average Student Loans`),
                  color = "white",
                  weight = 0.5,
                  fillOpacity = 0.7,  
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE,
                  ),popup=pop_pop) %>% addLegend(position = "bottomleft", colors =c("#EDF8E9","#BAE4B3","#74C476","#31A354","#006D2C"), 
                                                 labels = c(paste('$',round(min(states_year()$`Average Student Loans`)))," "," "," ", 
                                                            paste('$',round(max(states_year()$`Average Student Loans`)))),
                                                 title = "Average Student Loans (Per Student)") %>%
      leaflet::setView(-98.5795, 39.8282, zoom=3) %>% addControl(sc_time_selective_title, position='topright')
  })
  observe({
    leafletProxy("studentdebtmap", data = states_year())
  }) 
}

shinyApp(ui = ui, server = server)

