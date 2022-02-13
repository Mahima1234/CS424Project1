library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(DT)
library(shinyjs)

#Reading the file
CTAData <- read.csv("CTA2.csv")
#Using lubridate to add helpful date columns/extract date information
CTAData$newDate <- mdy(CTAData$date)
CTAData$year <- year(mdy(CTAData$date))
CTAData$dayOfTheWeek <- weekdays(mdy(CTAData$date))
CTAData$month <- months(mdy(CTAData$date))

#Subset of CTAData that only contains the UIC-Halsted stop
UICHalsted <- subset(CTAData, station_id == 40350)

#Subset of CTAData that only contains the O'hare stop
Ohare <- subset(CTAData, station_id == 40890)

#Subset of CTAData that only contains the Cumberland stop
Cumberland <- subset(CTAData, station_id == 40230)

#Vector for dropdown menu to change years
years<-c(2001:2021)
#Vector for dropdown menu to change stops
stops <- c("UIC-Halsted", "O'Hare Airport", "Cumberland")

#UI
ui <- dashboardPage(
  #Title for the application
  dashboardHeader(title = "CS 424 Project 1"),
  #Components on sidebar to switch from tab to tab
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     #First set of plots and tables that the user sees
                     menuItem("", tabName = "default", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     #Same contents as the default tab, but low enough to be reached on the big screen if the user wants to go back
                     menuItem("Dashboard", tabName = "dashboard", icon = NULL),
                     #This tab allows the user to compare the three bar graphs for the three stations
                     menuItem("Compare Plots", tabName = "comparePlots", icon = NULL),
                     #This tab allows the user to compare the three tables for the three stations
                     menuItem("Compare Tables", tabName = "compareCharts", icon = NULL),
                     #About page to get an idea of what the application is about
                     menuItem("About", tabName = "about", icon = NULL)
                     )
  ),
  
  dashboardBody(
    #ShinyJS used to hide and view the plots and charts
    useShinyjs(),
    tabItems(
      tabItem(
        #About Page
        tabName = "about",
        strong(h1("About this application")),
        h3("This interface allows users to look at CTA data over the past couple of decades for the stops UIC-Halsted, O'Hare Airport, and Cumberland.
        The user has an option to compare plots against each other from the afformentioned stops and years. The user also has the option to compare tables of data from which the plots were created 
        from the afformentioned stops and years. The data for this application is provided by Chicago Data portal. You may access it using this link: 
        https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f")
      ),
      tabItem(
        #Default Page
        tabName = "default",
        column(3,
          fluidRow(
            id = "halstedEntryBox",
            #Bar graph that shows all of the entries over the years at UIC Halsted
            box(title = "All Entries For UIC-Halsted", solidHeader = TRUE, status = "primary", width = 12,
              plotOutput("HalstedEntries", height = 150),
            ),
            #Chart that shows the data behind the bar graph above
            box(title = "All Entries For UIC-Halsted", solidHeader = TRUE, status = "primary", width = 12,
              DT::dataTableOutput("HalstedEntriesTable", height = 100)
            )
          ), 
        ),
        column(3,
               fluidRow(
                 id = "ohareEntryBox",
                 #Bar graph that shows all of the entries over the years at O'Hare
                 box(title = "All Entries For O'Hare", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("OhareEntries", height = 150),
                 ),
                 #Chart that shows the data behind the bar graph above
                 box( title = "All Entries For O'Hare", solidHeader = TRUE, status = "primary", width = 12,
                      DT::dataTableOutput("OhareEntriesTable", height = 100)
                 )
               ), 
        ),
        column(3,
               fluidRow(
                 id = "cumberlandEntryBox",
                 #Bar graph that shows all of the entries over the years at Cumberland
                 box(title = "All Entries For Cumberland", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("CumberlandEntries", height = 150),
                 ),
                 #Chart that shows the data behind the bar graph above
                 box( title = "All Entries For Cumberland", solidHeader = TRUE, status = "primary", width = 12,
                      DT::dataTableOutput("CumberlandEntriesTable", height = 100)
                 )
               ), 
        ),
        
      ),
      
      tabItem(
        #This tab has the same function as the default tab
        tabName = "dashboard",
        column(3,
               fluidRow(
                 id = "halstedEntryBox",
                 #Bar graph that shows all of the entries over the years at UIC-Halsted
                 box(title = "All Entries For UIC-Halsted", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("HalstedEntriesDash", height = 150),
                 ),
                 #Chart that shows the data behind the bar graph above
                 box( title = "All Entries For UIC-Halsted", solidHeader = TRUE, status = "primary", width = 12,
                      DT::dataTableOutput("HalstedEntriesTableDash", height = 100)
                 )
               ), 
        ),
        
        column(3,
               fluidRow(
                 id = "ohareEntryBox",
                 #Bar graph that shows all of the entries over the years at O'Hare
                 box(title = "All Entries For O'Hare", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("OhareEntriesDash", height = 150),
                 ),
                 #Chart that shows the data behind the bar graph above
                 box( title = "All Entries For O'Hare", solidHeader = TRUE, status = "primary", width = 12,
                      DT::dataTableOutput("OhareEntriesTableDash", height = 100)
                 )
               ), 
        ),
        
        column(3,
               fluidRow(
                 id = "cumberlandEntryBox",
                 #Bar graph that shows all of the entries over the years at Cumberland
                 box(title = "All Entries For Cumberland", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("CumberlandEntriesDash", height = 150),
                 ),
                 #Chart that shows the data behind the bar graph above
                 box( title = "All Entries For Cumberland", solidHeader = TRUE, status = "primary", width = 12,
                      DT::dataTableOutput("CumberlandEntriesTableDash", height = 100)
                 )
               ), 
        ),
        
      ),
      
    
    tabItem(
      #This tab will allow users to compare bar graphs at the three stops during different years
      tabName = "comparePlots",
      column(3,
             fluidRow(
               fluidRow(
                 #The dropdown menu to allow the user to select the year and stop they want to visualize for the left-most graphs
                 box(
                   selectInput("YearOne", "Select the year to visualize", years, selected = 2021), 
                   selectInput("StopOne", "Select the stop to visualize", stops, selected = "UIC-Halsted")
                 ),
                 box(width = 5,
                   #The checkboxes allow the user to pick which visualizations they want to see
                   p("Choose what data"),
                   checkboxInput("toggleDayEntriesOne", "Every Day of the Year", value = TRUE),
                   checkboxInput("toggleMonthEntriesOne", "Every Month of the Year", value = TRUE),
                   checkboxInput("toggleDayOfTheWeekEntriesOne", "Every Day of the Week for the Year", value = TRUE),
                 ),
               ),
               #This row has the bar graph for every single entry in a user specified year at the user specified stop (left-most)
               fluidRow(
                 id = "DayEntriesBoxOne",
                 box(status = "primary", width = 12,
                     plotOutput("DayEntriesOne", height = 150),
                 )
               ),
               #This row has the bar graph for every single entry split in months in a user specified year at a user specified stop (left-most)
               fluidRow(
                 id = "MonthEntriesBoxOne",
                 box(status = "primary", width = 12,
                     plotOutput("MonthEntriesOne", height = 150),
                 )
               ),
               #This row has the bar graph for every single entry split in days of the week in a user specified year at a user specified stop (left-most)
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
                   #The dropdown menu to allow the user to select the year and stop they want to visualize for the middle graphs
                   selectInput("YearTwo", "Select the year to visualize", years, selected = 2021),
                   selectInput("StopTwo", "Select the stop to visualize", stops, selected = "O'Hare Airport"),
                 ),
                 box(width = 5,
                   #The checkboxes allow the user to pick which visualizations they want to see
                   p("Choose what data"),
                   checkboxInput("toggleDayEntriesTwo", "Every Day of the Year", value = TRUE),
                   checkboxInput("toggleMonthEntriesTwo", "Every Month of the Year", value = TRUE),
                   checkboxInput("toggleDayOfTheWeekEntriesTwo", "Every Day of the Week for the Year", value = TRUE),
                 ),
               ),
               #This row has the bar graph for every single entry in a user specified year at the user specified stop (middle)
               fluidRow(
                 id = "DayEntriesBoxTwo",
                 box(status = "primary", width = 12,
                     plotOutput("DayEntriesTwo", height = 150),
                 )
               ),
               #This row has the bar graph for every single entry split in months in a user specified year at a user specified stop (middle)
               fluidRow(
                 id = "MonthEntriesBoxTwo",
                 box(status = "primary", width = 12,
                     plotOutput("MonthEntriesTwo", height = 150),
                 )
               ),
               #This row has the bar graph for every single entry split in days of the week in a user specified year at a user specified stop (middle)
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
                   #The dropdown menu to allow the user to select the year and stop they want to visualize for the right-most graphs
                   selectInput("YearThree", "Select the year to visualize", years, selected = 2021),
                   selectInput("StopThree", "Select the stop to visualize", stops, selected = "Cumberland"),
                 ),
                 box(width = 5,
                   #The checkboxes allow the user to pick which visualizations they want to see
                   p("Choose what data"),
                   checkboxInput("toggleDayEntriesThree", "Every Day of the Year", value = TRUE),
                   checkboxInput("toggleMonthEntriesThree", "Every Month of the Year", value = TRUE),
                   checkboxInput("toggleDayOfTheWeekEntriesThree", "Every Day of the Week for the Year", value = TRUE),
                 ),
               ),
               #This row has the bar graph for every single entry in a user specified year at the user specified stop (right-most)
               fluidRow(
                 id = "DayEntriesBoxThree",
                 box(status = "primary", width = 12,
                     plotOutput("DayEntriesThree", height = 150),
                 )
               ),
               #This row has the bar graph for every single entry split in months in a user specified year at a user specified stop (right-most)
               fluidRow(
                 id = "MonthEntriesBoxThree",
                 box(status = "primary", width = 12,
                     plotOutput("MonthEntriesThree", height = 150),
                 )
               ),
               #This row has the bar graph for every single entry split in days of the week in a user specified year at a user specified stop (right-most)
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
      #This tab will allow users to compare the data behind the bar graphs at the three stops during different years
      tabName = "compareCharts",
      column(3,
             fluidRow(
               fluidRow(
                 box(
                   #The dropdown menu to allow the user to select the year and stop they want to see the data for the left-most tables
                   selectInput("YearOneTable", "Select the year to visualize", years, selected = 2021), 
                   selectInput("StopOneTable", "Select the stop to visualize", stops, selected = "UIC-Halsted")
                 ),
                 box(
                   width = 5,
                   #The checkboxes allow the user to pick which tables they want to see
                   p("Choose what data"),
                   checkboxInput("toggleDayEntriesTableOne", "Every Day of the Year", value = TRUE),
                   checkboxInput("toggleMonthEntriesTableOne", "Every Month of the Year", value = TRUE),
                   checkboxInput("toggleDayOfTheWeekTableEntriesOne", "Every Day of the Week for the Year", value = TRUE),
                 ),
               ),
               #This row has the table for every single entry in a user specified year at the user specified stop (left-most)
               fluidRow(
                 id = "DayEntriesBoxTableOne",
                 box(status = "primary", width = 12,
                     DT::dataTableOutput("DayEntriesTableOne", height = 100)
                 )
               ),
               #This row has the table for every single entry split in months in a user specified year at a user specified stop (left-most)
               fluidRow(
                 id = "MonthEntriesBoxTableOne",
                 box(status = "primary", width = 12,
                     DT::dataTableOutput("MonthEntriesTableOne", height = 100)
                 )
               ),
               #This row has the table for every single entry split in days of the week in a user specified year at a user specified stop (left-most)
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
                   #The dropdown menu to allow the user to select the year and stop they want to see the data for the middle tables
                   selectInput("YearTwoTable", "Select the year to visualize", years, selected = 2021), 
                   selectInput("StopTwoTable", "Select the stop to visualize", stops, selected = "O'Hare Airport")
                 ),
                 box(
                   #The checkboxes allow the user to pick which tables they want to see
                   width = 5,
                   p("Choose what data"),
                   checkboxInput("toggleDayEntriesTableTwo", "Every Day of the Year", value = TRUE),
                   checkboxInput("toggleMonthEntriesTableTwo", "Every Month of the Year", value = TRUE),
                   checkboxInput("toggleDayOfTheWeekTableEntriesTwo", "Every Day of the Week for the Year", value = TRUE),
                 ),
               ),
               #This row has the table for every single entry in a user specified year at the user specified stop (middle)
               fluidRow(
                 id = "DayEntriesBoxTableTwo",
                 box(status = "primary", width = 12,
                     DT::dataTableOutput("DayEntriesTableTwo", height = 100)
                 )
               ),
               #This row has the bar graph for every single entry split in months in a user specified year at a user specified stop (middle)
               fluidRow(
                 id = "MonthEntriesBoxTableTwo",
                 box(status = "primary", width = 12,
                     DT::dataTableOutput("MonthEntriesTableTwo", height = 100)
                 )
               ),
               #This row has the table for every single entry split in days of the week in a user specified year at a user specified stop (middle)
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
              #The dropdown menu to allow the user to select the year and stop they want to see the data for the right-most tables
              selectInput("YearThreeTable", "Select the year to visualize", years, selected = 2021), 
              selectInput("StopThreeTable", "Select the stop to visualize", stops, selected = "Cumberland")
            ),
            box(width = 5,
                #The checkboxes allow the user to pick which tables they want to see
                p("Choose what data"),
                checkboxInput("toggleDayEntriesTableThree", "Every Day of the Year", value = TRUE),
                checkboxInput("toggleMonthEntriesTableThree", "Every Month of the Year", value = TRUE),
                checkboxInput("toggleDayOfTheWeekTableEntriesThree", "Every Day of the Week for the Year", value = TRUE),
            ),
          ),
          #This row has the table for every single entry in a user specified year at the user specified stop (right-most)
          fluidRow(
            id = "DayEntriesBoxTableThree",
            box(status = "primary", width = 12,
                DT::dataTableOutput("DayEntriesTableThree", height = 100)
            )
          ),
          #This row has the bar graph for every single entry split in months in a user specified year at a user specified stop (right-most)
          fluidRow(
            id = "MonthEntriesBoxTableThree",
            box(status = "primary", width = 12,
                DT::dataTableOutput("MonthEntriesTableThree", height = 100)
            )
          ),
          #This row has the table for every single entry split in days of the week in a user specified year at a user specified stop (right-most)
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
  #Reactives for the three sides of the screen (bar graph)
  #Combines the year and the stop in one
  stopReactiveOne <- reactive({subset(CTAData, CTAData$stationname == input$StopOne & year(CTAData$newDate) == input$YearOne)})
  stopReactiveTwo <- reactive({subset(CTAData, CTAData$stationname == input$StopTwo & year(CTAData$newDate) == input$YearTwo)})
  stopReactiveThree <- reactive({subset(CTAData, CTAData$stationname == input$StopThree & year(CTAData$newDate) == input$YearThree)})
  
  #Reactives for the three sides of the screen (table)
  #Combines the year and the stop in one
  stopReactiveOneTable <- reactive({subset(CTAData, CTAData$stationname == input$StopOneTable & year(CTAData$newDate) == input$YearOneTable)})
  stopReactiveTwoTable <- reactive({subset(CTAData, CTAData$stationname == input$StopTwoTable & year(CTAData$newDate) == input$YearTwoTable)})
  stopReactiveThreeTable <- reactive({subset(CTAData, CTAData$stationname == input$StopThreeTable & year(CTAData$newDate) == input$YearThreeTable)})
  
  #Default/Dashboard
  #ggplots for entries of 2001-2021 at UIC-Halsted
  output$HalstedEntries <- renderPlot({
    ggplot(UICHalsted, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(y= "Entries", x = "Date") 
  })
  output$HalstedEntriesDash <- renderPlot({
    ggplot(UICHalsted, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(y= "Entries", x = "Date") 
  })
  #ggplots for entries of 2001-2021 at O'Hare
  output$OhareEntries <- renderPlot({
    ggplot(Ohare, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(y= "Entries", x = "Date")
  })
  output$OhareEntriesDash <- renderPlot({
    ggplot(Ohare, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(y= "Entries", x = "Date")
  })
  #ggplots for entries of 2001-2021 at Cumberland
  output$CumberlandEntries <- renderPlot({
    ggplot(Cumberland, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(y= "Entries", x = "Date")
  })
  output$CumberlandEntriesDash <- renderPlot({
    ggplot(Cumberland, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(y= "Entries", x = "Date")
  })
  #datatables for entries of 2001-2021 at UIC-Halsted
  output$HalstedEntriesTable <- DT::renderDataTable(
    DT::datatable(
      {
        UICHalsted[c(6,5)]
        
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  output$HalstedEntriesTableDash <- DT::renderDataTable(
    DT::datatable(
      {
        UICHalsted[c(6,5)]
        
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  #datatables for entries of 2001-2021 at O'Hare
  output$OhareEntriesTable <- DT::renderDataTable(
    DT::datatable(
      {
        Ohare[c(6,5)]
        
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  output$OhareEntriesTableDash <- DT::renderDataTable(
    DT::datatable(
      {
        Ohare[c(6,5)]
        
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  #datatables for entries of 2001-2021 at Cumberland
  output$CumberlandEntriesTable <- DT::renderDataTable(
    DT::datatable(
      {
        Cumberland[c(6,5)]
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  output$CumberlandEntriesTableDash <- DT::renderDataTable(
    DT::datatable(
      {
        Cumberland[c(6,5)]
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )

  #Compare Plots
  #ggplots for daily entries (left-most)
  output$DayEntriesOne <- renderPlot({
    #based on the user chosen year and stop
    stop <- stopReactiveOne()
    #title created by concatenating the user chosen stop and the rest of the text
    ggplot(stop, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Daily Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Date")
  })
  #ggplots for monthly entries (left-most)
  output$MonthEntriesOne <- renderPlot({
    #based on the user chosen year and stop
    stop <- stopReactiveOne()
    #orderOfMonths is for the proper x axis ordering (instead of the default alphabetical)
    orderOfMonths <- factor(stop$month, level = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
    #title created by concatenating the user chosen stop and the rest of the text
    ggplot(stop, aes(x = orderOfMonths, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Monthly Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Month")
  })
  #ggplots for day of the week entries (left-most) 
  output$DayOfTheWeekEntriesOne <- renderPlot({
    #based on the user chosen year and stop
    stop <- stopReactiveOne()
    #orderOfDays is for the proper x axis ordering (instead of the default alphabetical)
    orderOfDays <- factor(stop$dayOfTheWeek, level = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    #title created by concatenating the user chosen stop and the rest of the text
    ggplot(stop, aes(x = orderOfDays, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Day of the Week Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Day of The Week")
  })
  #Allows the users to toggle the graph's visibility using the checkboxes
  observeEvent(input$toggleDayEntriesOne, {
    #Source for ShinyJS show/hide: https://rdrr.io/cran/shinyjs/man/visibilityFuncs.html
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
  
  #ggplots for daily entries (middle)
  output$DayEntriesTwo <- renderPlot({
    stop <- stopReactiveTwo()
    #title created by concatenating the user chosen stop and the rest of the text
    ggplot(stop, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Daily Entries")) + theme(plot.title = element_text(hjust = 1, size = 10))+ labs(y= "Entries", x = "Date")
  })
  #ggplots for monthly entries (middle)
  output$MonthEntriesTwo <- renderPlot({
    #based on the user chosen year and stop
    stop <- stopReactiveTwo()
    #orderOfMonths is for the proper x axis ordering (instead of the default alphabetical)
    orderOfMonths <- factor(stop$month, level = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
    #title created by concatenating the user chosen stop and the rest of the text
    ggplot(stop, aes(x = orderOfMonths, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Monthly Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Month")
  })
  #ggplots for day of the week entries (middle)
  output$DayOfTheWeekEntriesTwo <- renderPlot({
    #based on the user chosen year and stop
    stop <- stopReactiveTwo()
    #orderOfDays is for the proper x axis ordering (instead of the default alphabetical)
    orderOfDays <- factor(stop$dayOfTheWeek, level = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    #title created by concatenating the user chosen stop and the rest of the text
    ggplot(stop, aes(x = orderOfDays, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Day of the Week Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Day of The Week")
  })
  
  #Allows the users to toggle the graph's visibility using the checkboxes
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
  
  #ggplots for daily entries (right-most)
  output$DayEntriesThree <- renderPlot({
    #based on the user chosen year and stop
    stop <- stopReactiveThree()
    #title created by concatenating the user chosen stop and the rest of the text
    ggplot(stop, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Daily Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Date")
  })
  #ggplots for monthly entries (right-most)
  output$MonthEntriesThree <- renderPlot({
    #based on the user chosen year and stop
    stop <- stopReactiveThree()
    #orderOfMonths is for the proper x axis ordering (instead of the default alphabetical)
    orderOfMonths <- factor(stop$month, level = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
    #title created by concatenating the user chosen stop and the rest of the text
    ggplot(stop, aes(x = orderOfMonths, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Monthly Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Month")  })
  #ggplots for day of the week entries (right-most)
  output$DayOfTheWeekEntriesThree <- renderPlot({
    #based on the user chosen year and stop
    stop <- stopReactiveThree()
    #orderOfDays is for the proper x axis ordering (instead of the default alphabetical)
    orderOfDays <- factor(stop$dayOfTheWeek, level = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    #title created by concatenating the user chosen stop and the rest of the text
    ggplot(stop, aes(x = orderOfDays, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "stationname"], "Day of the Week Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Day of The Week")
  })
  #Allows the users to toggle the graph's visibility using the checkboxes
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
  #Compare Tables
  #datatable for daily entries (left-most)
  output$DayEntriesTableOne <- DT::renderDataTable(
    DT::datatable(
    {
      #taking out the specific columns (newDate and rides)
      stop <- stopReactiveOneTable()[c(6,5)]
    },
    #disabled searching, changing length, and displaying row names
    options = list(searching = FALSE,lengthChange = FALSE, pageLength = 1), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  #datatable for monthly entries (left-most)
  output$MonthEntriesTableOne <- DT::renderDataTable({
    DT::datatable(
      {
        #using the reactive data to create a new dataframe that tallies up the total number of entries per month
        stop <- stopReactiveOneTable()
        Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        #Source: https://www.statology.org/r-sum-column-with-condition/
        Entries <- c(sum(stop[which(stop$month=="January"), 5]),sum(stop[which(stop$month=="February"), 5]),sum(stop[which(stop$month=="March"), 5]),sum(stop[which(stop$month=="April"), 5]),sum(stop[which(stop$month=="May"), 5]),sum(stop[which(stop$month=="June"), 5]),sum(stop[which(stop$month=="July"), 5]),sum(stop[which(stop$month=="August"), 5]),sum(stop[which(stop$month=="September"), 5]),sum(stop[which(stop$month=="October"), 5]),sum(stop[which(stop$month=="November"), 5]),sum(stop[which(stop$month=="December"), 5]))
        df <- data.frame(Month, Entries)
        as.data.frame(df)
      },
      #disabled searching, changing length, and displaying row names
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE 
    )
  })
  #datatable for day of the week entries (left-most)
  output$DayOfTheWeekEntriesTableOne <- DT::renderDataTable(
    DT::datatable(
      {
        #using the reactive data to create a new dataframe that tallies up the total number of entries per day of the week
        stop <- stopReactiveOneTable()
        DayOfTheWeek <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
        #Source: https://www.statology.org/r-sum-column-with-condition/
        Entries <- c(sum(stop[which(stop$dayOfTheWeek=="Monday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Tuesday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Wednesday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Thursday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Friday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Saturday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Sunday"), 5]))
        df <- data.frame(DayOfTheWeek, Entries)
        as.data.frame(df)
      },
      #disabled searching, changing length, and displaying row names
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE, colnames = c("Day of the Week", "Entries")  
    )
  )
  #Allows the users to toggle the table's visibility using the checkboxes
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
  
  #datatable for daily entries (middle)
  output$DayEntriesTableTwo <- DT::renderDataTable(
    DT::datatable(
      {
        #taking out the specific columns (newDate and rides)
        stop <- stopReactiveTwoTable()[c(6,5)]
      },
      #disabled searching, changing length, and displaying row names
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  #datatable for monthly entries (middle)
  output$MonthEntriesTableTwo <- DT::renderDataTable(
    DT::datatable(
      {
        #using the reactive data to create a new dataframe that tallies up the total number of entries per month
        stop <- stopReactiveTwoTable()
        Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        Entries <- c(sum(stop[which(stop$month=="January"), 5]),sum(stop[which(stop$month=="February"), 5]),sum(stop[which(stop$month=="March"), 5]),sum(stop[which(stop$month=="April"), 5]),sum(stop[which(stop$month=="May"), 5]),sum(stop[which(stop$month=="June"), 5]),sum(stop[which(stop$month=="July"), 5]),sum(stop[which(stop$month=="August"), 5]),sum(stop[which(stop$month=="September"), 5]),sum(stop[which(stop$month=="October"), 5]),sum(stop[which(stop$month=="November"), 5]),sum(stop[which(stop$month=="December"), 5]))
        df <- data.frame(Month, Entries)
        as.data.frame(df)
      },
      #disabled searching, changing length, and displaying row names
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE 
    )
  )
  #datatable for day of the week entries (middle)
  output$DayOfTheWeekEntriesTableTwo <- DT::renderDataTable(
    DT::datatable(
      {
        #using the reactive data to create a new dataframe that tallies up the total number of entries per day of the week
        stop <- stopReactiveTwoTable()
        DayOfTheWeek <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
        Entries <- c(sum(stop[which(stop$dayOfTheWeek=="Monday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Tuesday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Wednesday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Thursday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Friday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Saturday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Sunday"), 5]))
        df <- data.frame(DayOfTheWeek, Entries)
        as.data.frame(df)
      },
      #disabled searching, changing length, and displaying row names
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE, colnames = c("Day of the Week", "Entries") 
    )
  )
  #Allows the users to toggle the table's visibility using the checkboxes
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
  
  #datatable for daily entries (right-most)
  output$DayEntriesTableThree <- DT::renderDataTable(
    DT::datatable(
      {
        #taking out the specific columns (newDate and rides)
        stop <- stopReactiveThreeTable()[c(6,5)]
      },
      #disabled searching, changing length, and displaying row names
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  #datatable for monthly entries (right-most)
  output$MonthEntriesTableThree <- DT::renderDataTable(
    DT::datatable(
      {
        #using the reactive data to create a new dataframe that tallies up the total number of entries per month
        stop <- stopReactiveThreeTable()
        Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        Entries <- c(sum(stop[which(stop$month=="January"), 5]),sum(stop[which(stop$month=="February"), 5]),sum(stop[which(stop$month=="March"), 5]),sum(stop[which(stop$month=="April"), 5]),sum(stop[which(stop$month=="May"), 5]),sum(stop[which(stop$month=="June"), 5]),sum(stop[which(stop$month=="July"), 5]),sum(stop[which(stop$month=="August"), 5]),sum(stop[which(stop$month=="September"), 5]),sum(stop[which(stop$month=="October"), 5]),sum(stop[which(stop$month=="November"), 5]),sum(stop[which(stop$month=="December"), 5]))
        df <- data.frame(Month, Entries)
        as.data.frame(df)
      },
      #disabled searching, changing length, and displaying row names
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE 
    )
  )
  #datatable for day of the week entries (right-most)
  output$DayOfTheWeekEntriesTableThree <- DT::renderDataTable(
    DT::datatable(
      {
        #using the reactive data to create a new dataframe that tallies up the total number of entries per day of the week
        stop <- stopReactiveThreeTable()
        DayOfTheWeek <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
        Entries <- c(sum(stop[which(stop$dayOfTheWeek=="Monday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Tuesday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Wednesday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Thursday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Friday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Saturday"), 5]),sum(stop[which(stop$dayOfTheWeek=="Sunday"), 5]))
        df <- data.frame(DayOfTheWeek, Entries)
        as.data.frame(df)
      },
      #disabled searching, changing length, and displaying row names
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 1), rownames = FALSE, colnames = c("Day of the Week", "Entries") 
    )
  )
  #Allows the users to toggle the table's visibility using the checkboxes
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