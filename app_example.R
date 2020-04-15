options(stringsAsFactors=FALSE)

library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)

theme_set(theme_bw())

# Most recent data from ECDC. read.csv() can load from URL
# NOTE: If the ECDC  changes formats without notice we will get an error
covid19 <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# NOTE: ECDC renamed some columns and changed date from mdy to dmy
covid19 <- covid19 %>%
    mutate(date=dmy(dateRep)) %>%
    group_by(geoId) %>%
    arrange(date) %>%
    mutate(cum_deaths=cumsum(deaths)) %>%
    ungroup()


ui <- fluidPage(

    titlePanel("COVID-19"),

    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId="countries", label="Countries",
                           choices=unique(covid19$countriesAndTerritories),
                           # pre-select some countries
                           selected=c("Canada", "Italy", "Spain"),
                           multiple=TRUE)
        ),

        mainPanel(
            plotOutput("covidPlot")
        )
    )
)

server <- function(input, output) {

    output$covidPlot <- renderPlot({
        forPlotting <- filter(covid19, countriesAndTerritories %in% input$countries)

        ggplot(forPlotting, aes(x=date, y=cum_deaths, col=countriesAndTerritories)) +
            geom_point(shape=1) +
            geom_line() +
            labs(x="", y="Total # of deaths due to COVID-19", col="")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
