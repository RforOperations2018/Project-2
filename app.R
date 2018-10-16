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

#Get proper year
properyear <- function(x, year=1968){
  m <- year(x) %% 100
  year(x) <- ifelse(m >= year %% 100, 1900+m, 2000+m)
  x
}


#Get data
lifers.load <- PA_DOC_Lifers_10_12_16_years <- read_csv("PA DOC Lifers-10.12.16-years.csv") %>%
  mutate(`Date of Birth` = properyear(as.Date(`Date of Birth`,format = "%m/%d/%y"), year = year(now())),
         `Committing Date` = properyear(as.Date(`Committing Date`, format = "%m/%d/%y"), year = year(now())),
         `Age at time of commitment` = round(as.numeric(difftime(`Committing Date`, `Date of Birth`, unit="weeks"))/52.25),
         `# of Years in Prison` = round(as.numeric(difftime(now(), `Committing Date`, unit="weeks"))/52.25),
         `Current Age` = round(as.numeric(difftime(now(), `Date of Birth`, unit="weeks"))/52.25))

#round((`Committing Date` - `Date of Birth`)/365))
  # mutate(`Years in Prison (as of today)` = year(now())-year(properyear(as.Date(`Committing Date`,format = "%m/%d/%y"), year = year(now()))))
# properyear(as.Date("12/31/68",format = "%m/%d/%y"), year = year(now()))
# properyear(mdy(lifers.load$`Date of Birth`[1]), year(today()))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

