library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(DT)
library(shinyjs)

#Reading the file
CTAData <- read.csv("CTA2.csv")
#Using lubridate the add helpful date columns/extract date information
CTAData$newDate <- mdy(CTAData$date)
CTAData$year <- year(mdy(CTAData$date))
CTAData$dayOfTheWeek <- weekdays(mdy(CTAData$date))
CTAData$month <- months(mdy(CTAData$date))

#Subset of CTAData that only contains the UIC-Halsted stop
UICHalsted <- subset(CTAData, station_id == 40350)
#Subset of UICHalsted that only contains 2021 information
UICHalsted2021 <- subset(UICHalsted, year == 2021)



#Subset of CTAData that only contains the O'hare stop
Ohare <- subset(CTAData, station_id == 40890)

#Subset of CTAData that only contains the Cumberland stop
Cumberland <- subset(CTAData, station_id == 40230)

years<-c(2001:2021)
stops <- c("UIC-Halsted", "O'Hare Airport", "Cumberland")


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     menuItem("", tabName = "default", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("Dashboard", tabName = "dashboard", icon = NULL),
                     menuItem("Compare Plots", tabName = "comparePlots", icon = NULL),
                     menuItem("Compare Charts", tabName = "compareCharts", icon = NULL),
                     menuItem("About", tabName = "about", icon = NULL)
                     )
  ),
  
  
  dashboardBody(
    useShinyjs(),
    




  tabItems(
    tabItem(
      tabName = "about",
      strong(h1("About this application")),
      h3("This interface allows users to look at CTA data over the past couple of decades for the stops UIC-Halsted, O'Hare Airport, and Cumberland.
        The user has an option to compare plots against each other from the afformentioned stops and years. The user also has the option to compare charts of data from which the plots were created 
        from the afformentioned stops and years. The data for this application is provided by Chicago Data portal. You may access it using this link: 
        https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f")
    ),
    tabItem(
      tabName = "default",
      column(3,
             fluidRow(
               id = "halstedEntryBox",
               box(title = "All Entries For UIC-Halsted", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("HalstedEntries", height = 150),
               ),
               box( title = "All Entries For UIC-Halsted", solidHeader = TRUE, status = "primary", width = 12,
                    DT::dataTableOutput("HalstedEntriesTable", height = 100)
               )
             ), 
      ),
      
      column(3,
             fluidRow(
               id = "ohareEntryBox",
               box(title = "All Entries For O'Hare", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("OhareEntries", height = 150),
               ),
               box( title = "All Entries For O'Hare", solidHeader = TRUE, status = "primary", width = 12,
                    DT::dataTableOutput("OhareEntriesTable", height = 100)
               )
             ), 
      ),
      
      column(3,
             fluidRow(
               id = "cumberlandEntryBox",
               box(title = "All Entries For Cumberland", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("CumberlandEntries", height = 150),
               ),
               box( title = "All Entries For Cumberland", solidHeader = TRUE, status = "primary", width = 12,
                    DT::dataTableOutput("CumberlandEntriesTable", height = 100)
               )
             ), 
      ),
      
    ),
    
    tabItem(
      tabName = "dashboard",
      column(3,
             fluidRow(
               id = "halstedEntryBox",
               box(title = "All Entries For UIC-Halsted", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("HalstedEntriesDash", height = 150),
               ),
               box( title = "All Entries For UIC-Halsted", solidHeader = TRUE, status = "primary", width = 12,
                    DT::dataTableOutput("HalstedEntriesTableDash", height = 100)
               )
             ), 
      ),
      
      column(3,
             fluidRow(
               id = "ohareEntryBox",
               box(title = "All Entries For O'Hare", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("OhareEntriesDash", height = 150),
               ),
               box( title = "All Entries For O'Hare", solidHeader = TRUE, status = "primary", width = 12,
                    DT::dataTableOutput("OhareEntriesTableDash", height = 100)
               )
             ), 
      ),
      
      column(3,
             fluidRow(
               id = "cumberlandEntryBox",
               box(title = "All Entries For Cumberland", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("CumberlandEntriesDash", height = 150),
               ),
               box( title = "All Entries For Cumberland", solidHeader = TRUE, status = "primary", width = 12,
                    DT::dataTableOutput("CumberlandEntriesTableDash", height = 100)
               )
             ), 
      ),
      
    ),
    
    
    tabItem(
      tabName = "comparePlots",
      column(3,
             fluidRow(
               fluidRow(
                 box(
                   selectInput("YearOne", "Select the year to visualize", years, selected = 2021), 
                   selectInput("StopOne", "Select the stop to visualize", stops, selected = "UIC-Halsted")
                 ),
                 box(width = 5,
                   p("Choose what data"),
                   checkboxInput("toggleDayEntriesOne", "Every Day of the Year", value = TRUE),
                   checkboxInput("toggleMonthEntriesOne", "Every Month of the Year", value = TRUE),
                   checkboxInput("toggleDayOfTheWeekEntriesOne", "Every Day of the Week for the Year", value = TRUE),
                 ),
               ),
               fluidRow(
                 id = "DayEntriesBoxOne",
                 box(status = "primary", width = 12,
                     plotOutput("DayEntriesOne", height = 150),
                 )
               ),
               fluidRow(
                 id = "MonthEntriesBoxOne",
                 box(status = "primary", width = 12,
                     plotOutput("MonthEntriesOne", height = 150),
                 )
               ),
               fluidRow(
                 id = "DayOfTheWeekEntriesBoxOne",
                 box(status = "primary", width = 12,
                     plotOutput("DayOfTheWeekEntriesOne", height = 150),
                 )
               ),
             
             ), 
      ),
      
      column(3,
             fluidRow(
               id = "dayEntryBoxTwo",
               fluidRow(
                 box(
                   selectInput("YearTwo", "Select the year to visualize", years, selected = 2021),
                   selectInput("StopTwo", "Select the stop to visualize", stops, selected = "O'Hare Airport"),
                 ),
                 box(width = 5,
                   p("Choose what data"),
                   checkboxInput("toggleDayEntriesTwo", "Every Day of the Year", value = TRUE),
                   checkboxInput("toggleMonthEntriesTwo", "Every Month of the Year", value = TRUE),
                   checkboxInput("toggleDayOfTheWeekEntriesTwo", "Every Day of the Week for the Year", value = TRUE),
                 ),
               ),
               fluidRow(
                 id = "DayEntriesBoxTwo",
                 box(status = "primary", width = 12,
                     plotOutput("DayEntriesTwo", height = 150),
                 )
               ),
               fluidRow(
                 id = "MonthEntriesBoxTwo",
                 box(status = "primary", width = 12,
                     plotOutput("MonthEntriesTwo", height = 150),
                 )
               ),
               fluidRow(
                 id = "DayOfTheWeekEntriesBoxTwo",
                 box(status = "primary", width = 12,
                     plotOutput("DayOfTheWeekEntriesTwo", height = 150),
                 )
               ),

             ), 
      ),
      
      
      column(3,
             fluidRow(
               id = "dayEntryBoxThree",
               fluidRow(
                 box(
                   selectInput("YearThree", "Select the year to visualize", years, selected = 2021),
                   selectInput("StopThree", "Select the stop to visualize", stops, selected = "Cumberland"),
                 ),
                 box(width = 5,
                   p("Choose what data"),
                   checkboxInput("toggleDayEntriesThree", "Every Day of the Year", value = TRUE),
                   checkboxInput("toggleMonthEntriesThree", "Every Month of the Year", value = TRUE),
                   checkboxInput("toggleDayOfTheWeekEntriesThree", "Every Day of the Week for the Year", value = TRUE),
                 ),
               ),
               fluidRow(
                 id = "DayEntriesBoxThree",
                 box(status = "primary", width = 12,
                     plotOutput("DayEntriesThree", height = 150),
                 )
               ),
               fluidRow(
                 id = "MonthEntriesBoxThree",
                 box(status = "primary", width = 12,
                     plotOutput("MonthEntriesThree", height = 150),
                 )
               ),
               fluidRow(
                 id = "DayOfTheWeekEntriesBoxThree",
                 box(status = "primary", width = 12,
                     plotOutput("DayOfTheWeekEntriesThree", height = 150),
                 )
               ),
             ), 
      ),
      
    
    ),
    
    
    tabItem(
      tabName = "compareCharts",
      column(3,
             fluidRow(
               fluidRow(
                 box(
                   selectInput("YearOneTable", "Select the year to visualize", years, selected = 2021), 
                   selectInput("StopOneTable", "Select the stop to visualize", stops, selected = "UIC-Halsted")
                 ),
                 box(
                   width = 5,
                   p("Choose what data"),
                   checkboxInput("toggleDayEntriesTableOne", "Every Day of the Year", value = TRUE),
                   checkboxInput("toggleMonthEntriesTableOne", "Every Month of the Year", value = TRUE),
                   checkboxInput("toggleDayOfTheWeekTableEntriesOne", "Every Day of the Week for the Year", value = TRUE),
                 ),
               ),
               fluidRow(
                 id = "DayEntriesBoxTableOne",
                 box(status = "primary", width = 12,
                     DT::dataTableOutput("DayEntriesTableOne", height = 100)
                 )
               ),
               fluidRow(
                 id = "MonthEntriesBoxTableOne",
                 box(status = "primary", width = 12,
                     DT::dataTableOutput("MonthEntriesTableOne", height = 100)
                 )
               ),
               fluidRow(
                 id = "DayOfTheWeekEntriesBoxTableOne",
                 box(status = "primary", width = 12,
                     DT::dataTableOutput("DayOfTheWeekEntriesTableOne", height = 100)
                 )
               ),
             )
        
      ),
      
      
      
      column(3,
             fluidRow(
               fluidRow(
                 box(
                   selectInput("YearTwoTable", "Select the year to visualize", years, selected = 2021), 
                   selectInput("StopTwoTable", "Select the stop to visualize", stops, selected = "O'Hare Airport")
                 ),
                 box(
                   width = 5,
                   p("Choose what data"),
                   checkboxInput("toggleDayEntriesTableTwo", "Every Day of the Year", value = TRUE),
                   checkboxInput("toggleMonthEntriesTableTwo", "Every Month of the Year", value = TRUE),
                   checkboxInput("toggleDayOfTheWeekTableEntriesTwo", "Every Day of the Week for the Year", value = TRUE),
                 ),
               ),
               fluidRow(
                 id = "DayEntriesBoxTableTwo",
                 box(status = "primary", width = 12,
                     DT::dataTableOutput("DayEntriesTableTwo", height = 100)
                 )
               ),
               fluidRow(
                 id = "MonthEntriesBoxTableTwo",
                 box(status = "primary", width = 12,
                     DT::dataTableOutput("MonthEntriesTableTwo", height = 100)
                 )
               ),
               fluidRow(
                 id = "DayOfTheWeekEntriesBoxTableTwo",
                 box(status = "primary", width = 12,
                     DT::dataTableOutput("DayOfTheWeekEntriesTableTwo", height = 100)
                 )
               ),
             )
             
      ),
      
      

      column(3,
        fluidRow(
          fluidRow(
            box(
              selectInput("YearThreeTable", "Select the year to visualize", years, selected = 2021), 
              selectInput("StopThreeTable", "Select the stop to visualize", stops, selected = "Cumberland")
            ),
            box(width = 5,
                p("Choose what data"),
                checkboxInput("toggleDayEntriesTableThree", "Every Day of the Year", value = TRUE),
                checkboxInput("toggleMonthEntriesTableThree", "Every Month of the Year", value = TRUE),
                checkboxInput("toggleDayOfTheWeekTableEntriesThree", "Every Day of the Week for the Year", value = TRUE),
            ),
          ),
          
          fluidRow(
            id = "DayEntriesBoxTableThree",
            box(status = "primary", width = 12,
                DT::dataTableOutput("DayEntriesTableThree", height = 100)
            )
          ),
          fluidRow(
            id = "MonthEntriesBoxTableThree",
            box(status = "primary", width = 12,
                DT::dataTableOutput("MonthEntriesTableThree", height = 100)
            )
          ),
          fluidRow(
            id = "DayOfTheWeekEntriesBoxTableThree",
            box(status = "primary", width = 12,
                DT::dataTableOutput("DayOfTheWeekEntriesTableThree", height = 100)
            )
          ),
        )
      )
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  )



  )
)

server <- function(input, output){
  stopReactiveOne <- reactive({subset(CTAData, CTAData$stationname == input$StopOne & year(CTAData$newDate) == input$YearOne)})
  stopReactiveTwo <- reactive({subset(CTAData, CTAData$stationname == input$StopTwo & year(CTAData$newDate) == input$YearTwo)})
  stopReactiveThree <- reactive({subset(CTAData, CTAData$stationname == input$StopThree & year(CTAData$newDate) == input$YearThree)})
  
  stopReactiveOneTable <- reactive({subset(CTAData, CTAData$stationname == input$StopOneTable & year(CTAData$newDate) == input$YearOneTable)})
  stopReactiveTwoTable <- reactive({subset(CTAData, CTAData$stationname == input$StopTwoTable & year(CTAData$newDate) == input$YearTwoTable)})
  stopReactiveThreeTable <- reactive({subset(CTAData, CTAData$stationname == input$StopThreeTable & year(CTAData$newDate) == input$YearThreeTable)})
  
  
  
  #Default/Dashboard
  output$HalstedEntries <- renderPlot({
    ggplot(UICHalsted, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(y= "Entries", x = "Date") 
  })
  output$HalstedEntriesDash <- renderPlot({
    ggplot(UICHalsted, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(y= "Entries", x = "Date") 
  })
  output$OhareEntries <- renderPlot({
    ggplot(Ohare, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(y= "Entries", x = "Date")
  })
  output$OhareEntriesDash <- renderPlot({
    ggplot(Ohare, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(y= "Entries", x = "Date")
  })
  output$CumberlandEntries <- renderPlot({
    ggplot(Cumberland, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(y= "Entries", x = "Date")
  })
  output$CumberlandEntriesDash <- renderPlot({
    ggplot(Cumberland, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(y= "Entries", x = "Date")
  })
  
  output$HalstedEntriesTable <- DT::renderDataTable(
    DT::datatable(
      {
        UICHalsted[c(6,5)]
        
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  output$HalstedEntriesTableDash <- DT::renderDataTable(
    DT::datatable(
      {
        UICHalsted[c(6,5)]
        
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  
  output$OhareEntriesTable <- DT::renderDataTable(
    DT::datatable(
      {
        Ohare[c(6,5)]
        
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  
  output$OhareEntriesTableDash <- DT::renderDataTable(
    DT::datatable(
      {
        Ohare[c(6,5)]
        
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  

  
  output$CumberlandEntriesTable <- DT::renderDataTable(
    DT::datatable(
      {
        Cumberland[c(6,5)]
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  
  output$CumberlandEntriesTableDash <- DT::renderDataTable(
    DT::datatable(
      {
        Cumberland[c(6,5)]
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  
  
  
  #CompareDay
  output$DayEntriesOne <- renderPlot({
    stop <- stopReactiveOne()
    ggplot(stop, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Daily Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Date")
  })
  
  output$MonthEntriesOne <- renderPlot({
    stop <- stopReactiveOne()
    orderOfMonths <- factor(stop$month, level = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
    ggplot(stop, aes(x = orderOfMonths, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Monthly Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Month")
  })
  
  output$DayOfTheWeekEntriesOne <- renderPlot({
    stop <- stopReactiveOne()
    orderOfDays <- factor(stop$dayOfTheWeek, level = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    ggplot(stop, aes(x = orderOfDays, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Day of the Week Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Day of The Week")
  })
  
  observeEvent(input$toggleDayEntriesOne, {
    if(input$toggleDayEntriesOne == TRUE){
      shinyjs::show("DayEntriesBoxOne")
    }
    else{
      shinyjs::hide("DayEntriesBoxOne")
    }
    
  })
  
  observeEvent(input$toggleMonthEntriesOne, {
    if(input$toggleMonthEntriesOne == TRUE){
      shinyjs::show("MonthEntriesBoxOne")
    }
    else{
      shinyjs::hide("MonthEntriesBoxOne")
    }
  })
  
  observeEvent(input$toggleDayOfTheWeekEntriesOne, {
    if(input$toggleDayOfTheWeekEntriesOne == TRUE){
      shinyjs::show("DayOfTheWeekEntriesBoxOne")
    }
    else{
      shinyjs::hide("DayOfTheWeekEntriesBoxOne")
    }
  })
  
  output$DayEntriesTwo <- renderPlot({
    stop <- stopReactiveTwo()
    ggplot(stop, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Daily Entries")) + theme(plot.title = element_text(hjust = 1, size = 10))+ labs(y= "Entries", x = "Date")
  })
  
  output$MonthEntriesTwo <- renderPlot({
    stop <- stopReactiveTwo()
    orderOfMonths <- factor(stop$month, level = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
    ggplot(stop, aes(x = orderOfMonths, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Monthly Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Month")
  })
  
  output$DayOfTheWeekEntriesTwo <- renderPlot({
    stop <- stopReactiveTwo()
    orderOfDays <- factor(stop$dayOfTheWeek, level = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    ggplot(stop, aes(x = orderOfDays, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Day of the Week Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Day of The Week")
  })
  
  
  observeEvent(input$toggleDayEntriesTwo, {
    if(input$toggleDayEntriesTwo == TRUE){
      shinyjs::show("DayEntriesBoxTwo")
    }
    else{
      shinyjs::hide("DayEntriesBoxTwo")
    }
  })
  
  observeEvent(input$toggleMonthEntriesTwo, {
    if(input$toggleMonthEntriesTwo == TRUE){
      shinyjs::show("MonthEntriesBoxTwo")
    }
    else{
      shinyjs::hide("MonthEntriesBoxTwo")
    }
  })
  
  observeEvent(input$toggleDayOfTheWeekEntriesTwo, {
    if(input$toggleDayOfTheWeekEntriesTwo == TRUE){
      shinyjs::show("DayOfTheWeekEntriesBoxTwo")
    }
    else{
      shinyjs::hide("DayOfTheWeekEntriesBoxTwo")
    }
  })
  
  
  
  
  output$DayEntriesThree <- renderPlot({
    stop <- stopReactiveThree()
    ggplot(stop, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Daily Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Date")
  })
  
  output$MonthEntriesThree <- renderPlot({
    stop <- stopReactiveThree()
    orderOfMonths <- factor(stop$month, level = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
    ggplot(stop, aes(x = orderOfMonths, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Monthly Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Month")  })
  
  output$DayOfTheWeekEntriesThree <- renderPlot({
    stop <- stopReactiveThree()
    orderOfDays <- factor(stop$dayOfTheWeek, level = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    ggplot(stop, aes(x = orderOfDays, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Day of the Week Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Day of The Week")
  })
  
  
  observeEvent(input$toggleDayEntriesThree, {
    if(input$toggleDayEntriesThree == TRUE){
      shinyjs::show("DayEntriesBoxThree")
    }
    else{
      shinyjs::hide("DayEntriesBoxThree")
    }
  })
  
  observeEvent(input$toggleMonthEntriesThree, {
    if(input$toggleMonthEntriesThree == TRUE){
      shinyjs::show("MonthEntriesBoxThree")
    }
    else{
      shinyjs::hide("MonthEntriesBoxThree")
    }
  })
  
  observeEvent(input$toggleDayOfTheWeekEntriesThree, {
    if(input$toggleDayOfTheWeekEntriesThree == TRUE){
      shinyjs::show("DayOfTheWeekEntriesBoxThree")
    }
    else{
      shinyjs::hide("DayOfTheWeekEntriesBoxThree")
    }
  })
  

  
  
  output$DayEntriesTableOne <- DT::renderDataTable(
    DT::datatable(
    {
      stop <- stopReactiveOneTable()[c(6,5)]
    },
    options = list(searching = FALSE, pageLength = 2), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )

  output$MonthEntriesTableOne <- DT::renderDataTable({
    DT::datatable(
      {
        stop <- stopReactiveOneTable()
        Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        Entries <- c(sum(stop[which(stop$month=="January"), 5]),sum(stop[which(stop$month=="February"), 5]),sum(stop[which(stop$month=="March"), 5]),sum(stop[which(stop$month=="April"), 5]),sum(stop[which(stop$month=="May"), 5]),sum(stop[which(stop$month=="June"), 5]),sum(stop[which(stop$month=="July"), 5]),sum(stop[which(stop$month=="August"), 5]),sum(stop[which(stop$month=="September"), 5]),sum(stop[which(stop$month=="October"), 5]),sum(stop[which(stop$month=="November"), 5]),sum(stop[which(stop$month=="December"), 5]))
        df <- data.frame(Month, Entries)
        as.data.frame(df)
        
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE 
    )
  })
  
  
  output$DayOfTheWeekEntriesTableOne <- DT::renderDataTable(
    DT::datatable(
      {
        stop <- stopReactiveOneTable()
        DayOfTheWeek <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
        Entries <- c(sum(stop[which(stop$dayOfTheWeek=="Monday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Tuesday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Wednesday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Thursday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Friday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Saturday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Sunday"), 5]))
        df <- data.frame(DayOfTheWeek, Entries)
        as.data.frame(df)
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE, colnames = c("Day of the Week", "Entries")  
    )
  )
  
  observeEvent(input$toggleDayEntriesTableOne, {
    if(input$toggleDayEntriesTableOne == TRUE){
      shinyjs::show("DayEntriesBoxTableOne")
    }
    else{
      shinyjs::hide("DayEntriesBoxTableOne")
    }
  })
  
  observeEvent(input$toggleMonthEntriesTableOne, {
    if(input$toggleMonthEntriesTableOne == TRUE){
      shinyjs::show("MonthEntriesBoxTableOne")
    }
    else{
      shinyjs::hide("MonthEntriesBoxTableOne")
    }
  })
  
  observeEvent(input$toggleDayOfTheWeekTableEntriesOne, {
    if(input$toggleDayOfTheWeekTableEntriesOne == TRUE){
      shinyjs::show("DayOfTheWeekEntriesBoxTableOne")
    }
    else{
      shinyjs::hide("DayOfTheWeekEntriesBoxTableOne")
    }
  })
  
  
  
  
  output$DayEntriesTableTwo <- DT::renderDataTable(
    DT::datatable(
      {
        stop <- stopReactiveTwoTable()[c(6,5)]
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  
  output$MonthEntriesTableTwo <- DT::renderDataTable(
    DT::datatable(
      {
        stop <- stopReactiveTwoTable()
        Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        Entries <- c(sum(stop[which(stop$month=="January"), 5]),sum(stop[which(stop$month=="February"), 5]),sum(stop[which(stop$month=="March"), 5]),sum(stop[which(stop$month=="April"), 5]),sum(stop[which(stop$month=="May"), 5]),sum(stop[which(stop$month=="June"), 5]),sum(stop[which(stop$month=="July"), 5]),sum(stop[which(stop$month=="August"), 5]),sum(stop[which(stop$month=="September"), 5]),sum(stop[which(stop$month=="October"), 5]),sum(stop[which(stop$month=="November"), 5]),sum(stop[which(stop$month=="December"), 5]))
        df <- data.frame(Month, Entries)
        as.data.frame(df)
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE 
    )
  )
  
  
  output$DayOfTheWeekEntriesTableTwo <- DT::renderDataTable(
    DT::datatable(
      {
        stop <- stopReactiveTwoTable()
        DayOfTheWeek <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
        Entries <- c(sum(stop[which(stop$dayOfTheWeek=="Monday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Tuesday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Wednesday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Thursday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Friday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Saturday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Sunday"), 5]))
        df <- data.frame(DayOfTheWeek, Entries)
        as.data.frame(df)
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE, colnames = c("Day of the Week", "Entries") 
    )
  )
  
  
  observeEvent(input$toggleDayEntriesTableTwo, {
    if(input$toggleDayEntriesTableTwo == TRUE){
      shinyjs::show("DayEntriesBoxTableTwo")
    }
    else{
      shinyjs::hide("DayEntriesBoxTableTwo")
    }
  })
  
  observeEvent(input$toggleMonthEntriesTableTwo, {
    if(input$toggleMonthEntriesTableTwo == TRUE){
      shinyjs::show("MonthEntriesBoxTableTwo")
    }
    else{
      shinyjs::hide("MonthEntriesBoxTableTwo")
    }
  })
  
  observeEvent(input$toggleDayOfTheWeekTableEntriesTwo, {
    if(input$toggleDayOfTheWeekTableEntriesTwo == TRUE){
      shinyjs::show("DayOfTheWeekEntriesBoxTableTwo")
    }
    else{
      shinyjs::hide("DayOfTheWeekEntriesBoxTableTwo")
    }
  })
  
  
  output$DayEntriesTableThree <- DT::renderDataTable(
    DT::datatable(
      {
        stop <- stopReactiveThreeTable()[c(6,5)]
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  
  output$MonthEntriesTableThree <- DT::renderDataTable(
    DT::datatable(
      {
        stop <- stopReactiveThreeTable()
        Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        Entries <- c(sum(stop[which(stop$month=="January"), 5]),sum(stop[which(stop$month=="February"), 5]),sum(stop[which(stop$month=="March"), 5]),sum(stop[which(stop$month=="April"), 5]),sum(stop[which(stop$month=="May"), 5]),sum(stop[which(stop$month=="June"), 5]),sum(stop[which(stop$month=="July"), 5]),sum(stop[which(stop$month=="August"), 5]),sum(stop[which(stop$month=="September"), 5]),sum(stop[which(stop$month=="October"), 5]),sum(stop[which(stop$month=="November"), 5]),sum(stop[which(stop$month=="December"), 5]))
        df <- data.frame(Month, Entries)
        as.data.frame(df)
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE 
    )
  )
  
  output$DayOfTheWeekEntriesTableThree <- DT::renderDataTable(
    DT::datatable(
      {
        stop <- stopReactiveThreeTable()
        DayOfTheWeek <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
        Entries <- c(sum(stop[which(stop$dayOfTheWeek=="Monday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Tuesday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Wednesday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Thursday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Friday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Saturday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Sunday"), 5]))
        df <- data.frame(DayOfTheWeek, Entries)
        as.data.frame(df)
      },
      options = list(searching = FALSE, pageLength = 2), rownames = FALSE, colnames = c("Day of the Week", "Entries") 
    )
  )
  
  observeEvent(input$toggleDayEntriesTableThree, {
    if(input$toggleDayEntriesTableThree == TRUE){
      shinyjs::show("DayEntriesBoxTableThree")
    }
    else{
      shinyjs::hide("DayEntriesBoxTableThree")
    }
  })
  
  observeEvent(input$toggleMonthEntriesTableThree, {
    if(input$toggleMonthEntriesTableThree == TRUE){
      shinyjs::show("MonthEntriesBoxTableThree")
    }
    else{
      shinyjs::hide("MonthEntriesBoxTableThree")
    }
  })
  
  observeEvent(input$toggleDayOfTheWeekTableEntriesThree, {
    if(input$toggleDayOfTheWeekTableEntriesThree == TRUE){
      shinyjs::show("DayOfTheWeekEntriesBoxTableThree")
    }
    else{
      shinyjs::hide("DayOfTheWeekEntriesBoxTableThree")
    }
  })
  
  
  

  
  

}

shinyApp(ui, server)