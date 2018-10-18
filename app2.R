#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(lubridate)
library(rgdal)
library(jsonlite)
library(httr)
library(shinydashboard)
library(shinydashboardPlus)
library(data.table)
library(ggmap)
library(leaflet)
library(plotly)
library(DT)


#Get proper year
properyear <- function(x, year=1968){
  m <- year(x) %% 100
  year(x) <- ifelse(m >= year %% 100, 1900+m, 2000+m)
  x
}


#Get lifers data and
lifers.load <- PA_DOC_Lifers_10_12_16_years <- read_csv("PA DOC Lifers-10.12.16-years.csv") %>%
  mutate(`Date of Birth` = properyear(as.Date(`Date of Birth`,format = "%m/%d/%y"), year = year(now())),
         `Committing Date` = properyear(as.Date(`Committing Date`, format = "%m/%d/%y"), year = year(now())),
         `Age at time of commitment` = round(as.numeric(difftime(`Committing Date`, `Date of Birth`, unit="weeks"))/52.25),
         `# of Years in Prison` = round(as.numeric(difftime(now(), `Committing Date`, unit="weeks"))/52.25),
         `Current Age` = round(as.numeric(difftime(now(), `Date of Birth`, unit="weeks"))/52.25),
         `Committing County` = str_to_title(`Committing County`),
         `Year Committed` = year(`Committing Date`))

#round((`Committing Date` - `Date of Birth`)/365))
  # mutate(`Years in Prison (as of today)` = year(now())-year(properyear(as.Date(`Committing Date`,format = "%m/%d/%y"), year = year(now()))))
# properyear(as.Date("12/31/68",format = "%m/%d/%y"), year = year(now()))
# properyear(mdy(lifers.load$`Date of Birth`[1]), year(today()))


# geography level: 050
aa_varcode <- "B01001B_002E" #Estimate!!Total!!Male
white_varcode <- "B01001A_002E"
key <- "db29ecc2d9ec905998b48dd3dafe73475ddfb106"

# https://api.census.gov/data/2016/acs/acs5?get=
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_16_5YR_B02001&prodType=table
  
# url <- "https://api.census.gov/data/2016/acs/acs5?get=NAME,B01001B_001EA,&for=county:*&in=state:42"

# works <- "https://api.census.gov/data/2016/acs/acs5?get=NAME,B01001B_001E&for=county:*&in=state:42&key=db29ecc2d9ec905998b48dd3dafe73475ddfb106"
# B01001B_002E




# Sample Charts
# offense_category <- lifers.load %>% group_by(`Offense Category`) %>% count(sort = T)
# offense <- lifers.load %>% group_by(`Offense`) %>% count(sort = T)
# race <- lifers.load %>% group_by(`Race`) %>% count(sort = T)
# ggplot(data = lifers.load, aes(x = `Age at time of commitment`)) + geom_bar()


cut(lifers.load$`Age at time of commitment`, breaks = seq(from = 15, to = 80, by = 5), right = FALSE, labels = FALSE)
agebreaks <- c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,500)
agelabels <- c("5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85+")

lifers.load <- setDT(lifers.load)[ , `Current Age Group` := cut(lifers.load$`Current Age`, 
                                breaks = agebreaks, 
                                right = FALSE, 
                                labels = agelabels)]
# ggplot(lifers.load, aes(x = `Current Age Group`)) + geom_bar() + facet_grid(rows = vars(Race))










# prison
prisonpop <- lifers.load %>%
              group_by(`Current Location`) %>%
              count(sort = T) 




# prison_latlon <- geocode(prisonpop$`Current Location`, output = "latlona")

# prison_latlon


# df <- data.frame(fromJSON(json)) %>%
#   mutate(X1 = as.character(X1),
#          X2 = as.character(X2),
#          X3 = as.character(X3),
#          X4 = as.character(X4))


# sum(county_lifers$n)
# DATA CLEANING

# Color Palette




# load counties
counties.load <- readOGR("Pennsylvania County Boundaries.geojson",layer = "OGRGeoJSON")
# View(counties.load@data)


# Define UI Elements
header <- dashboardHeader(title = "Pennsylvania Prisons Serving Life in Prison",
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "", 
                                                        icon = icon("users"))
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 10, color = "green",
                                                text =  "")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "",
                                         message = HTML(""),
                                         icon = icon("exclamation-circle"))
                          )
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    menuItem("Map", icon = icon("map-marker"), tabName = "map"),
    sliderInput("yearInput",
                "Committed Year:",
                min = min(lifers.load$`Year Committed`, na.rm = T),
                max = max(lifers.load$`Year Committed`, na.rm = T),
                sep = '',
                step = 1,
                round = T,
                dragRange = T, 
                value = c(min(lifers.load$`Year Committed`, na.rm = T), max(lifers.load$`Year Committed`, na.rm = T)))
                ,
    # Year
    selectInput(inputId = "raceInmate",
                label = "Race of Inmate:",
                multiple = TRUE,
                choices = c("All",str_to_title(sort(unique(lifers.load$Race)))),
                selected = "All"),
    menuItem("BarChart", tabName = "scatter",badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            inputPanel(selectInput("variable",
                                   label = "Select Measure",
                                   choices = c(names(select(lifers.load,`Age at time of commitment`:`Year Committed`))),
                                   selectize = T,
                                   multiple = F,
                                   selected = "# of Years in Prison"),
                       checkboxInput("checkbox", "Show by race"))),
          fluidRow(plotlyOutput("plot"))
                       
              
            )
          ,
  tabItem("table",
          fluidPage(
            box(title = "Prisoners: Life", DT::dataTableOutput("table"), width = 12))),
  tabItem("map",
          fluidPage(
            fluidRow(
              box(
                selectInput("racemap",
                            "Per Capita Life in Prison by Race",
                            choices = c("WHITE" = "B01001A_002E","BLACK" = "B01001B_002E"),
                            selected = "B01001B_002E"
              )
            ),
            fluidRow(
              box(leafletOutput("leaflet") , width = 12)
            )))
  )))

ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # initial reactive
  liferInput <- reactive({
    lifers <- lifers.load %>%
      # Slider Filter
      filter(`Year Committed` >= input$yearInput[1] & `Year Committed` <= input$yearInput[2])
    if (!("All" %in% input$raceInmate)) {
    lifers <- subset(lifers, Race %in% input$raceInmate)}
    return(lifers)
  })
  
  output$table <- DT::renderDataTable({
    dat <- liferInput()
    datatable(dat[, !names(dat) %in% c("Last Name", "First Name","Mid Name")] 
,options = list(scrollX = TRUE))
    
    
  })
  
  output$plot <- renderPlotly({
    dat <- liferInput() 
    dat <- dat %>%
      rename(chartvar = input$variable)
    g <- ggplot(dat, aes(x = chartvar, fill = chartvar)) + geom_histogram()
    if (input$checkbox)
      {ggplotly(g + facet_grid(rows = vars(Race)))}
    else
      {ggplotly(g)}
    
    
  })

  
  #map
  output$leaflet <- renderLeaflet({
              male.aa.url <- paste0("https://api.census.gov/data/2016/acs/acs5?get=NAME,", input$race, "&for=county:*&in=state:42&key=db29ecc2d9ec905998b48dd3dafe73475ddfb106")
              
              
              r <- GET(male.aa.url)
              ls(r)
              c <- content(x = r, as = "text")
              typeof(c)
              json <- gsub('NaN', 'NA', c, perl = TRUE)
              df <- data.frame(jsonlite::fromJSON(json))
              
              # function to make first row column names
              header.true <- function(df) {
                names(df) <- as.character(unlist(df[1,]))
                df[-1,]
              }
              
              df <- header.true(df)
              # Pull out County name without the word "County")
              separate(df, "NAME", c("County Name","State"),sep = ",")
              df$`County Short` <- word(df$`NAME`, 1)
              
              
              counties.load@data <- counties.load@data %>%
                inner_join(df, by= c("fips_count" = "county"))
    
              # count by county
              all_lifers <- lifers.load %>%
                group_by(`Committing County`) %>%
                dplyr::count()
              
              aa_lifers <- lifers.load %>% 
                filter(Race == "BLACK") %>%
                group_by(`Committing County`) %>%
                dplyr::count()
              
              # total aa pop by county
              df <- mutate(df, aapop = as.numeric(as.character(B01001B_002E)))
              df %>%
                group_by(`County Short`) %>% 
                summarize(sum(aapop))
              
              # merge on county
              df <- df %>%
                left_join(aa_lifers,by = c("County Short" = "Committing County")) %>%
                # dplyr::rename(`LIP Black` = `n`) %>%
                left_join(all_lifers,by = c("County Short" = "Committing County")) #%>%
              # dplyr::rename(`LIP All` = `n`)
              
              df <- rename(df, `LIP Black` = `n.x`, `LIP All` = `n.y`, `AA Pop` = `aapop`) %>%
                mutate(`% AA Males LIP` = round(`LIP Black`/`AA Pop`,4))
              
              aa_pal <- colorNumeric("Reds",domain = df$`% AA Males LIP`, na.color = "white")
              
              prisons <- read_csv("Prison Locations.csv")
              
    
    counties.load %>%
      leaflet() %>%
      addProviderTiles("Stamen.Toner") %>%
      setView(lat = 40.8766, 
              lng = -77.8367,
              zoom = 7) %>%
      addPolygons(weight = 1,
                  fillOpacity = 1,
                  color = ~aa_pal(`% AA Males LIP`), 
                  label = ~paste0(round(`% AA Males LIP`*1000,1)," out of every 1,000 ", ifelse(input$racemap == "B01001B_002E", "black","white"), " males from ", `County Short`, " County are serving Life in Prison"),
                  highlight = highlightOptions(weight = 3, color = "gray", bringToFront = T), group = "Per Capita") %>%
      addCircleMarkers(data = prisons, 
                       lng = ~longitude, 
                       lat = latitude, 
                       pop = ~Name, 
                       color = "#FF0000",group = 'Prisoners') 
      
    
    keytable <- data.frame(RACE = c("WHITE","BLACK"),key = c(white_varcode, aa_varcode))
    
    # import geojson
    # counties.load <- readOGR("https://data.pa.gov/resource/n96m-gp6j",layer = "OGRGeoJSON")
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


