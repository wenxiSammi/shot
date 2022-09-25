# load required packages
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

# import libraties
library(dplyr)
library(leaflet)
library(maps)
library(ggplot2)
library(plotly)
library(stringr)
library(shiny)
library(scales)
library(tidyverse)

# load data, import the shot by state CSV
shot_data<- read.csv('shot_data.csv')
data <- read.csv('shot_data_year.csv')

# data processing
data$X2015 <- as.numeric(gsub(',','',data$X2015))
data$X2016 <- as.numeric(gsub(',','',data$X2016))
data$X2017 <- as.numeric(gsub(',','',data$X2017))
data$X2018 <- as.numeric(gsub(',','',data$X2018))
data$X2019 <- as.numeric(gsub(',','',data$X2019))
data$X2020 <- as.numeric(gsub(',','',data$X2020))
data$X2021 <- as.numeric(gsub(',','',data$X2021))
data$X2022 <- as.numeric(gsub(',','',data$X2022))


shot_data$date = as.Date(shot_data$date)
st_min_date = as.Date(min(shot_data$date),"%Y-%m-%d")
st_max_date = as.Date(max(shot_data$date),"%Y-%m-%d")
st_max_date_clean = format(as.POSIXct(st_max_date),"%d %B %Y")


# Add a `popup` column to the shot_data data set 
shot_data$popup <- paste0('<b>', shot_data$name, '</b> (', shot_data$state, ')<br>',
                          'race: ', shot_data$race)
#Order for num by state name in map library through match function
total_shots <- shot_data$Num[match(shot_data$name,shot_data$state)]

##Count the number of shots in each state
state_shots <- shot_data %>% group_by(state) %>% summarise(Num=n())

##Count the number of shots in each age_group
age_group_shots <- shot_data %>% group_by(age_group) %>% summarise(Num=n())

##Count the number of shots in each city
city_shots <- shot_data %>% group_by(city) %>% summarise(Num=n())

##count the number of shots in each race
race_shots <- shot_data %>% group_by(race) %>% summarise(Num=n())


##Count the number of shots in each gender
gender_shots <- shot_data %>% group_by(gender) %>% summarise(Num=n())

#Create plotting parameters for map
bins = c(0,10,50,100,500,1000,Inf)
st_pal <- colorBin("Oranges", domain = race_shots, bins = bins)




##################
# USER INTERFACE #
##################

# Background (intro) tab - static content, no interaction 
intro_tab <- tabPanel(
  'Background',
  
  titlePanel('Police shooting in US'),
  p('people have been shot and killed by police in the past year'),
  p(img(src = 'https://www.washingtonpost.com/resizer/p6fPy7Tp0uzboND3A3Z_Jh9_a8o=/1484x0/arc-anglerfish-washpost-prod-washpost.s3.amazonaws.com/public/J3O2CNZTNJBYFGAZ4MDBUI5534.gif')),
  p('Data source: ',
    a(href = 'https://www.washingtonpost.com/graphics/investigations/police-shootings-database/', 'Washington Post(US-centic)'))
)



# Components of the main tab
shot_map <- tabPanel(
  'Shot Map',
  
  #sidebar layout with a input and output definitions
  sidebarLayout(
    #Inputs: Select variable to map
    sidebarPanel(
      # Shot map filter controls 
      h3('Shootings happen across the country'),
      span(tags$i(h6("In 2015, The Washington Post began to log every fatal shooting by an on-duty police officer in the United States. In that time there have been more than 5,000 such shootings recorded by The Post.")), style="color:#045a8d"),
      selectInput(
        'state',
        label = 'State',
        choices = c('National', sort(unique(shot_data$state))),
        selected = '2022'
      ),
      span(tags$i(h6("Police shootings have taken place in every state and have occurred more frequently in cities where populations are concentrated. ")), style="color:#045a8d"),
      span(tags$i(h6("States with the highest rates of shootings are Alaska,Texas,New Mexico and Oklahoma.")), style="color:#045a8d"),
      
      selectInput(
        'race',
        label = 'Race',
        choices = c('National', sort(unique(shot_data$race))),
        selected = 'National'
      ),
      span(tags$i(h6("Over 95 percent - an overwhelming majority of people shot and killed by police are male. ")), style="color:#045a8d"),
      radioButtons(
        'gender',
        label = 'Gender',
        choices = c('All', 'M', 'F'),
        selected = 'All'
      ),position = c("right")
    ),
    #Output: Show map
    mainPanel(
      leafletOutput('shotMap',height=700),
      plotOutput("genderBar",height=200)
    )
  )
)


# shot analyze
shot_analyze<- tabPanel(
  'Rate of shootings remains steady',
  titlePanel('Shot Data Analyze'),
  
  sidebarLayout(
    #Inputs: Select variable to plot
    
    sidebarPanel(
  
      span(tags$i(h3("Reported about 1,000 people have been shot and killed by police in the past year, Updated Sept.23, 2022. ",)), style="color:#045a8d"),
      span(tags$i(h6("In the United States, it can be seen that CA and TX had the most gun cases in 2022 with 89 and 78 cases respectively.")), style="color:#045a8d"),
      span(tags$i(h6("Also,we can see that CA and TX had the most gun cases in 2021 with 141 and 75, respectively.")), style="color:#045a8d"),
      span(tags$i(h6("In 2020 with 147 and 83, respectively.")), style="color:#045a8d"),
      span(tags$i(h6("In 2019 with 135 and 108, respectively.")), style="color:#045a8d"),
      span(tags$i(h6("In 2018 with 116 and 84, respectively.")), style="color:#045a8d"),
      span(tags$i(h6("In 2017 with 160 and 69, respectively.")), style="color:#045a8d"),
      span(tags$i(h6("In 2016 with 139 and 81 cases respectively.")), style="color:#045a8d"),
      span(tags$i(h6("In 2015 with 190 and 100 cases respectively.")), style="color:#045a8d"),

    
    # shot by state graph controls
    selectInput(
      'year',
      label = 'Year',
      choices = c(
        '2015'='X2015',
        '2016'='X2016',
        '2017'='X2017',
        '2018'='X2018',
        '2019'='X2019',
        '2020'='X2020',
        '2021'='X2021',
        '2022'='X2022'),
      selected = 'X2022'
    ),
    span(tags$i(h3("Most victims are young")), style="color:#045a8d"),
    span(tags$i(h6("An majority of people shot and killed by police more than half the victims are between 20 and 40 years old.")), style="color:#045a8d"),
    span(tags$i(h6("From chart we can found age  between 20 to 30 about 1400 people.")), style="color:#045a8d"),
    span(tags$i(h6("Age between 31 to 40 about 1500 people.")), style="color:#045a8d"),
    span(tags$i(h3("Black Americans are killed at a much higher rate than White Americans")), style="color:#045a8d"),
    
    span(tags$i(h6("Although half of the people shot and killed by police are White, Black Americans are shot at a disproportionate rate. They account for less than 13 percent of the U.S. population")), style="color:#045a8d"),
    span(tags$i(h6("but are killed by police at more than twice the rate of White Americans. ")), style="color:#045a8d"),
  ),
    #Output: Show plot
    mainPanel(
      plotlyOutput("shotPlot",height=400),
      plotOutput("ageBar",height=300),
      plotOutput("raceBar",height=300)

    )
  )
)

# data tab
data_tab <- tabPanel(
  'Row Data',
  titlePanel('Data'),
  numericInput("maxrows", "Rows to show", 40),
  verbatimTextOutput("rawtable"),
  downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
  "Adapted from timeline data published by ", tags$a(href="https://github.com/washingtonpost/data-police-shootings/releases/download/v0.1/fatal-police-shootings-data.csv","The Washington Post")
)

# About tab
about_tab <- tabPanel(
  'About this site',
  titlePanel('About this site'),
  tags$div(
    "This site is analyze the shots by police in US from 2015 to 2022", 
    tags$br(),tags$br(),tags$h4("Background"), 
    "In 2015, The Washington Post began to log every fatal shooting by an on-duty police officer in the United States. In that time there have been more than 5,000 such shootings recorded by The Post.",
    tags$br(),tags$br(),
    "After Michael Brown, an unarmed Black man, was killed in 2014 by police in Ferguson, Mo., a Post investigation found that the FBI undercounted fatal police shootings by more than half. This is because reporting by police departments is voluntary and many departments fail to do so.",
    tags$br(),tags$br(),tags$h4("Code"),
    "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
    tags$br(),tags$br(),tags$h4("Sources"),
    tags$b("Date set source "), tags$a(href="https://github.com/washingtonpost/data-police-shootings", "data-police-shootings"),
 
    tags$br(),tags$br(),tags$h4("Authors"),
    "Sammi zhang , The University of Melbourne",tags$br(),
    tags$br(),tags$br()
  )
)



# Overall Shiny app UI
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  'Shot By Police In US',
  intro_tab,
  shot_map,
  shot_analyze,
  data_tab,
  about_tab
)



################
# SHINY SERVER #
################

# Server function 
server <- function(input, output, session) {
  values <- reactiveValues()
  
  output$shotPlot <- renderPlotly({
    p <- ggplot(data=data, aes_string(x='state_sn', 
                                      y=input$year)) +
      geom_bar(stat='identity', width=0.8) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width=10)) +
      scale_y_continuous(labels = function(y) format(y, big.mark=',')) +
      labs(x='state', y=paste0('Number of shots, ', gsub('X', '', input$year))) +
      theme(axis.text = element_text(size=9),
            axis.title = element_text(size=12),
            panel.background = element_blank(),
            panel.grid.major.y = element_line(colour='#eeeeee'))
    
    values$loaded <- TRUE
    ggplotly(p, source='shotPlot')
  })
  
  # This function observes events that take place within the Shiny app. In this
  # particular case we filter only to the "click" event on the `plot_births` plot.
  # -- Canvas page 7.5.7
  observe({
    # Check whether the data been loaded? If not, stop
    req(values$loaded)
    # Get the data for the "click" event on the `shotPlot` plot
    d <- event_data("plotly_click", source="shotPlot")
    # If there is no such data, stop
    if (is.null(d)) return()
    
    # we have to add 1 because R counts from 1 while Plotly counts from 0
    stateName <- data$state[d$pointNumber + 1]
    # Set the `state` input to the selected state
    updateSelectInput(session, 'state', selected=stateName)
  })


  # map --function -specific plots
  # filter data as needed through input reactive
  
  filteredMapData <- reactive({
    filter(shot_data, 
           if (input$gender == 'All') TRUE else gender == input$gender,
           if (input$state == 'National') TRUE else state == input$state,
           if (input$race == 'National') TRUE else race == input$race)
  })
  
  output$shotMap <- renderLeaflet({
    leaflet(filteredMapData()) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addAwesomeMarkers(~longitude, ~latitude,
                        icon=~awesomeIcons(icon=case_when(gender == 'M' ~ 'stethoscope',
                                                          TRUE ~ 'plus-square'),
                                           library='fa',
                                           markerColor=case_when(gender == 'F' ~ 'darkred',
                                                                 TRUE ~ 'red'),
                                           iconColor='#fff'),
                        label=~name,
                        popup=~popup,
                        clusterOptions = markerClusterOptions())%>% 
      
      addLegend(
        position = "bottomright",
        pal = st_pal,
        values = filteredMapData(),
        title = paste("Number of<br>shots in US")
      )
    
  })
  
  
   # 
  output$raceBar <- renderPlot({
    ggplot(race_shots,aes(x=reorder(race,Num), y=Num)) +
      ## draw bars
      #Without stat='identity' ggplot wants to aggregate your data into counts.
      geom_bar(stat='identity', fill = 'lightblue', width = 0.4) + 
      labs(x="Race",y="Number of Shots",title="Shots' situation in USA") +
      geom_text(aes(label = Num)) +
      coord_flip() +
      theme_minimal()
  })
  
  output$ageBar <- renderPlot({
    ggplot(age_group_shots,aes(x=reorder(age_group,Num), y=Num)) +
      ## draw bars
      #Without stat='identity' ggplot wants to aggregate your data into counts.
      geom_bar(stat='identity', fill = 'lightblue', width = 0.6) + 
      labs(x="Age Group",y="Number of Shots",title="Shots' situation  by age group in USA") +
      geom_text(aes(label = Num)) +
      ## for horization bar
      coord_flip() +
      theme_minimal()
  })
  
  output$genderBar <- renderPlot({
    ggplot(gender_shots,aes(x=reorder(gender,Num), y=Num)) +
      ## draw bars
      #Without stat='identity' ggplot wants to aggregate your data into counts.
      geom_bar(stat='identity', fill = 'lightblue', width = 0.4) + 
      labs(x="Race",y="Number of Shots",title="Shots' situation in USA by gender") +
      geom_text(aes(label = Num)) +
      coord_flip() +
      theme_minimal()
  })

  # output to download data
  output$downloadCsv <- downloadHandler(
  filename = function() {
    paste("shot_data.csv", sep="")
  },
  content = function(file) {
    shot_data_sub = shot_data %>% select(c(id,name,date,manner_of_death,armed,age,gender,race,city,state,signs_of_mental_illness,threat_level
,flee,body_camera,longitude,latitude,is_geocoding_exact
))
    names(shot_data_sub) = c("id", "name","date", "manner_of_death", "armed","age","gender", "race", "city",
                            "state", "signs_of_mental_illness",	"threat_level","flee", "body_camera","longitude","latitude","is_geocoding_exact")
    write.csv(shot_data_sub, file)
  })
  
  output$rawtable <- renderPrint({
    shot_data_sub = shot_data %>% select(c(id,name,date,manner_of_death,armed,age,gender,race,city,state,signs_of_mental_illness,threat_level
                                          ,flee,body_camera,longitude,latitude,is_geocoding_exact
    ))
    names(shot_data_sub) = c("id","name","date","manner_of_death","armed","age","gender", "race", "city",
                             "state", "signs_of_mental_illness",	"threat_level","flee", "body_camera","longitude","latitude","is_geocoding_exact")
  raw <- options(width = 800)
  print(head(shot_data_sub, input$maxrows), row.names = FALSE)
  options(raw)
  })
}



#############
# Run Shiny #
#############

shinyApp(ui, server)
