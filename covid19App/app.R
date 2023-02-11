######## COVID19 Dashboard #########
# Covid19 Dashboard based on the COVID-19 Data Analysis with R - Worldwide analysis report of the Novel Coronavirus.
# Find original study here: http://www.rdatamining.com/docs/Coronavirus-data-analysis-world.pdf
# COVID 19 API: https://covid19-api-access.herokuapp.com/
# https://github.com/deidax
#          
####################################

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(leaflet)
library(plotly)
library(DT)
library(shinyjs)




# CSS for the loading icons
appCSS <- "
  #loading-content {
    position: absolute;
    background: rgba(255,255, 255);
    z-index: 100;
    justify-content: center;
    align-items: center;
    top: 3.5em;
    left: 0;
    right: 0;
    height: 100%;
    width: 100%;
    text-align: center;
    color: black;
  }
  
  #loading-icon {
    position: fixed;
    top: 50%;
    left: 65%;
    margin-top: -50px;
    margin-left: -100px;
  }
  
"


ui <- dashboardPage(
  dashboardHeader(title = "Covid19 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tags$b("Worldwide", style = "font-size: 13px"), tabName = "wr" ,icon = icon("globe-africa")),
      menuItem(tags$b("Top 10 countries", style = "font-size: 13px"), tabName = "top10", icon = icon("globe")),
      menuItem(tags$b("Search by country", style = "font-size: 13px"), tabName = "bycountry", icon = icon("search-location")),
      menuItem(tags$b("COVID19 API", style = "font-size: 13px"),href = "https://covid19-api-access.herokuapp.com/", icon = icon("project-diagram")),
      menuItem(tags$b("About this site", style = "font-size: 13px"), tabName = "about", icon = icon("address-card"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "wr",
              tags$style(
                  "#map-loading {
                  position: absolute;
                  display: flex;
                  justify-content: center;
                  align-items: center;
                  text-align:center;
                  width: 92%;
                  height: 600px;
                  background-color: rgba(255, 225, 225, 0);;
                  text-align: center;
                  }"
              ),
              fluidRow(
                
                valueBoxOutput('confirmed', width = 4),
                valueBoxOutput('deaths', width = 4),
                valueBoxOutput('recovered', width = 4),
                
                box(
                  width = 12,
                  height = "700px",
                  title = 'Covid19 world confirmed cases visualisation',
                  status = "primary",
                  solidHeader = TRUE,
                  tags$div(id = "map-loading",
                           tags$div(
                             tags$img(src = "https://mir-s3-cdn-cf.behance.net/project_modules/disp/ab79a231234507.564a1d23814ef.gif",width = "50px", height = "50px"),
                             tags$br(),
                             tags$span("Loading Map..."))
                            ),
                  leafletOutput('map',width = "100%", height = "600px")
                  
                  
                ),
                
                
                box(title = "Numbers of Cases Worldwide", status = "primary", plotOutput("plot1", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                
                box(title = "Numbers of Cases Worldwide (log scale)", status = "primary", plotOutput("plot2", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                
                box(title = "Current Confirmed Cases", status = "primary", plotOutput("plot3", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                
                box(title = "Daily New Confirmed Cases", status = "primary", plotOutput("plot4", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                
                box(title = "New deaths", status = "primary", plotOutput("plot5", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                
                box(title = "New Recovered Cases", status = "primary", plotOutput("plot6", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                
                box(title = "Death rate overall", status = "primary", plotOutput("plot10", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                
                box(title = "Death rate for last two weeks", status = "primary", plotOutput("plot11", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                
                box(title = "Report coronavirus cases for Today", status = "primary",width=12, DT::dataTableOutput("countries_in_date_table") %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6)))
        
                
              )),
      
      tabItem(tabName = "top10",
              fluidRow(
                
                box(title = "Top 10 countries with most confirmed cases", status = "primary", plotOutput("barChart", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                
                box(title = "Total confirmed and deaths for Top10", status = "primary", plotOutput("plot7", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                
                box(title = "Total confirmed and deaths for Top 10 Countries (log scale)", status = "primary", plotOutput("plot8", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                
                box(title = "Top 10 Countries - New Confirmed vs New Deaths (log scale)", status = "primary", plotOutput("plot9", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6)))
                
              
              )),
      
      tabItem(tabName = "bycountry",
              useShinyjs(),
              inlineCSS(appCSS),
              div(
                id = "loading-content",
                # h2("Loading...",style = "position:center")
                div(id="loading-icon",
                  tags$img(src = "https://mir-s3-cdn-cf.behance.net/project_modules/disp/ab79a231234507.564a1d23814ef.gif", width = "50px", height = "50px"),
                  tags$br(),
                  tags$span("Loading...")
                )
              ),
         
              fluidPage(
                
                box(title = "Country Data",width = 12,
                    
                    div(style="text-align: center;",
                      
                      div(
                        style="display: inline-block;",
                        tags$span(htmlOutput("ranking"), style="position:relative; top:-1em")
                      ),
                      
                      div(
                        style="padding: 15px;width: 50%; display: inline-block;",
                        uiOutput("countries_list",width = "100%")
                      ),
                      
                      div(
                        style="display: inline-block",
                        tags$span(id="flag",style="position:relative; top:-1em",
                            uiOutput("flag_img")
                          )
                      ),
                      
                      tags$br(),
                      infoBoxOutput("country_confirmed"),
                      infoBoxOutput("country_deaths"),
                      infoBoxOutput("country_recovered"),
                      tags$br(),
                      
                      div(id="country_dt",
                          DT::dataTableOutput("country_data") %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))
                      ),
                      # Download csv file Button
                      downloadButton("downloadData", "Download Data")
                    )
                    
                ),
                
                box(title = "Numbers of Cases", status = "primary", plotOutput("country_plot1", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                box(title = "Numbers of Cases (Log scale)", status = "primary", plotOutput("country_plot2", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                box(title = "Current Confirmed Cases", status = "primary", plotOutput("country_plot3", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                box(title = "Daily New Confirmed Cases", status = "primary", plotOutput("country_plot4", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                box(title = "New deaths", status = "primary", plotOutput("country_plot5", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                box(title = "New Recovered Cases", status = "primary", plotOutput("country_plot6", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                box(title = "Death Rate Overall", status = "primary", plotOutput("country_plot10", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6))),
                box(title = "Death rate for last two weeks", status = "primary", plotOutput("country_plot11", height = 500) %>% withSpinner(color="#0dc5c1",type = getOption("spinner.type", default = 6)))
                
              )
              
              ),
      
      
      tabItem(tabName = "about",
              fluidRow(
                
                box(title = tags$h4("About this website"), status = "primary", width = 12,
                    
                    tags$div(class = "body", checked = NA,
                             tags$p("This website is a ",
                                    
                                    tags$b("Covid19 Dashboard"),
                                    " based on the",
                                    tags$b("COVID-19 Data Analysis with R - Worldwide"),
                                    " analysis report of the Novel Coronavirus.",
                                    tags$br(),
                                    " You can find the analysis report in the Sources below. "
          
                                    ),
                             tags$h4("Sources"),
                             tags$p("Worldometers: ",
                                    tags$a(href = "https://www.worldometers.info/coronavirus/", "https://www.worldometers.info/coronavirus/")
                                    ),
                             tags$p("COVID-19 Data Analysis with R - Worldwide: ",
                                    tags$a(href = "http://www.rdatamining.com/docs/Coronavirus-data-analysis-world.pdf", "Yanchang Zhao, COVID-19 Data Analysis with R – Worldwide. RDataMining.com, 2020")
                             ),
                             
                             tags$h4("Contact"),
                             tags$p("Email: ",
                                   tags$br(), 
                                   tags$span( tags$a(href = "mailto:chafiq.512@gmail.com", "chafiq.512@gmail.com")),
                                   tags$br(),
                                   tags$span( tags$a(href = "mailto:khalil.dida01@gmail.com", "khalil.dida01@gmail.com"))
                             )
                    )
                    
                    )
      
              ))
    )
    
  ))




server <- function(input, output, session) {
  
  source('global.R',local = TRUE)

  output$map <- renderLeaflet({map})

  output$countries_list<-renderUI({
    hide(id = "loading-content", anim = TRUE, animType = "fade") 
    selectInput("country","Choose a country", "Select a country:", choices=dt[,1])
  })
  
  # diplay ranking
  output$ranking <- renderUI({
      if(!is.null(input$country)){
        country <- dt %>% filter(country == input$country)
        HTML(paste('<b style = "font-size: 20px">Ranking: ',country$ranking,'</b>'))
      }
  })
  
  output$flag_img <- renderUI({
    if(!is.null(input$country)){
      tags$img(src = getSelectedCountryFlag(input$country),width="60", border="1 px solid #aaa")
    }
    
  })
  # InfoBoxOutputs
  output$country_confirmed <- renderInfoBox({
    if(!is.null(input$country))
    {
      country <- countryCurrentData()
      infoBox(
        tags$b("Confirmed"),
        country$confirmed,
        tags$span("Current cases: ",tags$b(country$current.confirmed),style = "font-size: 14px"),
        icon = icon("procedures"),
        color = "orange", fill = TRUE
      )
    }
    else
    {
      infoBox(
        "Confirmed", "Loading...", icon = icon("procedures"),
        color = "orange", fill = TRUE
      )
    }
  })
  
  output$country_deaths <- renderInfoBox({
    if(!is.null(input$country))
    {
      country <- dt %>% filter(country == input$country)
      infoBox(
        tags$b("Deaths"),
        country$deaths,
        tags$span("Death rate: ",tags$b(country$death.rate),style = "font-size: 14px"),
        icon = icon("bed"),
        color = "red", fill = TRUE
      )
    }
    else
    {
      infoBox(
        "Deaths", "Loading...", icon = icon("procedures"),
        color = "red", fill = TRUE
      )
    }
  })
  
  output$country_recovered <- renderInfoBox({
    if(!is.null(input$country))
    {
      country <- countryCurrentData()
      infoBox(
        tags$b("Recovered"),
        country$recoverd,
        icon = icon("heartbeat"),
        color = "lime", fill = TRUE
      )
    }
    else
    {
      infoBox(
        "Recovered", "Loading...", icon = icon("heartbeat"),
        color = "lime", fill = TRUE
      )
    }
  })
  
  
  output$confirmed <- renderValueBox({
    valueBox(
      format(world$confirmed, big.mark=" "),icon = icon("procedures"), tags$span(
        tags$b("Confirmed", style = "font-size: 20px"),
        tags$br(),
        tags$span("New Confirmed: ", style = "font-size: 13px",tags$b(format(data.world.date.max$new.confirmed, big.mark=" "))),
        tags$br(),
        tags$span("Current Confirmed: ", style = "font-size: 13px",tags$b(format(data.world.date.max$current.confirmed, big.mark=" ")))
        ),
      
      color = "orange"
    )
  })
  
  output$deaths <- renderValueBox({
    valueBox(
      format(world$deaths, big.mark=" "), tags$span(
        tags$b("Deaths", style = "font-size: 20px"),
        tags$br(),
        tags$span("New Deaths: ", style = "font-size: 13px",tags$b(format(data.world.date.max$new.deaths, big.mark=" "))),
        tags$br(),
        tags$span("Daily Rate: ", style = "font-size: 13px",tags$b(data.world.date.max$rate.daily," %"))
      ),
      icon = icon("bed"),
      color = "red"
    )
  })
  
  output$recovered <- renderValueBox({
    valueBox(
      format(world$recoverd, big.mark=" "), tags$span(
        tags$b("Recovered", style = "font-size: 20px"),
        tags$br(),
        tags$br(),
        tags$span("New Recovered: ", style = "font-size: 13px",tags$b(format(data.world.date.max$new.recoverd, big.mark=" "))),
        tags$br()
      ), 
      icon = icon("heartbeat"),
      color = "lime"
    )
  })

  
  
  output$plot1 <- renderPlot({ numberOfCases(world.long) })
  
  output$country_plot1 <- renderPlot({
    if(!is.null(input$country)){
      numberOfCases(dataLong(input$country)) }
    })
  
  output$plot2 <- renderPlot({ numberOfCasesLogScale(world.long) })
  
  output$country_plot2 <- renderPlot({ 
    if(!is.null(input$country)){
      numberOfCasesLogScale(dataLong(input$country)) }
  })
  
  output$plot3 <- renderPlot({ currentConfirmedCases('World') })
  
  output$country_plot3 <- renderPlot({
    if(!is.null(input$country)){
      currentConfirmedCases(input$country) }
  })
  
  output$plot4 <- renderPlot({ DailyNewConfirmedCases('World') })
  
  output$country_plot4 <- renderPlot({ 
    if(!is.null(input$country)){
      DailyNewConfirmedCases(input$country) }
  })
  
  output$plot5 <- renderPlot({ newDeaths('World') })
  
  output$country_plot5 <- renderPlot({ 
    if(!is.null(input$country)){
      newDeaths(input$country) }
  })
  
  output$plot6 <- renderPlot({ newRecoveredCases('World') })
  
  output$country_plot6 <- renderPlot({ 
    if(!is.null(input$country)){
      newRecoveredCases(input$country) }
  })
  # Top 10 ranked countries
  output$barChart <- renderPlot({barChart})
  
  output$plot7 <- renderPlot({plot7})
  
  output$plot8 <- renderPlot({plot8})
  
  output$plot9 <- renderPlot({plot9})
  # Death rates
  output$plot10 <- renderPlot({ deathRateOverall('World') })
  
  output$country_plot10 <- renderPlot({
    if(!is.null(input$country)){
      deathRateOverall(input$country) }
  })
  
  output$plot11 <- renderPlot({ deathRateLast2weeks('World') })
  
  output$country_plot11 <- renderPlot({
    if(!is.null(input$country)){
      deathRateLast2weeks(input$country) }
  })
  
  output$countries_in_date_table <- DT::renderDataTable({
    dt$confirmed <- format(dt$confirmed, big.mark=" ")
    dt$new.confirmed <- format(dt$new.confirmed, big.mark=" ")
    dt$current.confirmed <- format(dt$current.confirmed, big.mark=" ")
    dt$recoverd <- format(dt$recoverd, big.mark=" ")
    dt$deaths <- format(dt$deaths, big.mark=" ")
    dt$new.deaths <- format(dt$new.deaths, big.mark=" ")
    DT::datatable(dt[, c("ranking","country", "confirmed","new.confirmed","current.confirmed","recoverd", "deaths","new.deaths","death.rate")],
                  rownames = FALSE, options = list(pageLength = 50, order = list(list(2, 'desc')))
    )
  })
  
  
  countryData <- reactive({
    country <- getSelectedCountryData(input$country)
    country$confirmed <- format(country$confirmed, big.mark=" ")
    country$new.confirmed <- format(country$new.confirmed, big.mark=" ")
    country$current.confirmed <- format(country$current.confirmed, big.mark=" ")
    country$recoverd <- format(country$recoverd, big.mark=" ")
    country$deaths <- format(country$deaths, big.mark=" ")
    country$new.deaths <- format(country$new.deaths, big.mark=" ")
    country
    
  })
  
  countryCurrentData <- reactive({
    country <- countryData() %>% filter(date == date.max)
    country
  })
  
  output$country_data <- DT::renderDataTable({
    
    if(!is.null(input$country)){
      country <- countryData()
      DT::datatable(country[, c("date","new.confirmed","new.deaths","new.recoverd")],
                    rownames = FALSE, options = list(pageLength = 10, order = list(list(0, 'desc')))
      )
    }
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$country, ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(countryData(), file, row.names = TRUE)
      contentType = "text/csv"
    }
  )
  

  
                    

}



shinyApp(ui, server)


