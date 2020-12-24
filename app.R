library(zoo)
library(xts)
library(ggplot2)
library(forecast)
library(shiny)
library(shinythemes)
library(dashboardthemes)
library(shinydashboard)

#### DATA ####
#data = read.csv("Travel_Times_Daily.csv")
#date = as.Date(data$Date, "%m/%d/%Y")
#day = xts(data$Daily.Mean.Travel.Time..Seconds., order.by=date)
#day = na.omit(day)

#year = "2016"
#start_date = paste(year, "-01-01", sep="")
#start_index = which(index(day)==start_date)
#end_date = paste(year, "-12-31", sep="")
#end_index = which(index(day)==end_date)
#train = day[start_index:end_index]

#test_end_index = which(index(day)=="2019-12-31")
#train_end_index = test_end_index - 7
#day.lag = diff(day, lag=365, differences=1)
#train_start_index = last(which(is.na(day.lag))) + 1

#train = day.lag[train_start_index:train_end_index]
#test = day.lag[(train_end_index+1):test_end_index]

#model = Arima(train, order=c(1,0,0), seasonal=list(order=c(1, 1, 1), period=7))

#model.pred = as.data.frame(forecast(model,h=7)$mean)
#ggplot(data=model.pred, aes(x=c(1:7),y=x,group=1)) + geom_line()

#### Theme & Color ####
theme_grey_dark <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(205,205,205)"
  ,primaryFontColor = "rgb(255,255,255)"
  ,infoFontColor = "rgb(255,255,255)"
  ,successFontColor = "rgb(255,255,255)"
  ,warningFontColor = "rgb(255,255,255)"
  ,dangerFontColor = "rgb(255,255,255)"
  ,bodyBackColor = "rgb(35,35,35)" #rgb(45,55,65)
  
  ### header
  ,logoBackColor = "rgb(60,60,60)"
  
  ,headerButtonBackColor = "rgb(60,60,60)"
  ,headerButtonIconColor = "rgb(205,205,205)"
  ,headerButtonBackColorHover = "rgb(60,60,60)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(60,60,60)"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "rgb(45,45,45)" #rgb(52,62,72)
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 15
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "rgb(205,205,205)"
  
  ,sidebarSearchBackColor = "rgb(45,55,65)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(45,55,65)"
  
  ,sidebarTabTextColor = "rgb(205,205,205)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "#d87600" #ff8c00
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "5px"
  
  ,sidebarTabBackColorHover = "#d87600"
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "5px"
  
  ### boxes
  ,boxBackColor = "rgb(45,45,45)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 16
  ,boxDefaultColor = "#d87600"
  ,boxPrimaryColor = "rgb(200,200,200)"
  ,boxInfoColor = "rgb(80,95,105)"
  ,boxSuccessColor = "rgb(45,45,45)"
  ,boxWarningColor = "rgb(240,80,210)"
  ,boxDangerColor = "rgb(240,80,80)"
  
  ,tabBoxTabColor = "rgb(52,62,72)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(205,205,205)"
  ,tabBoxTabTextColorSelected = "rgb(205,205,205)"
  ,tabBoxBackColor = "rgb(52,62,72)"
  ,tabBoxHighlightColor = "rgb(70,80,90)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(230,230,230)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(50,50,50)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(180,180,180)"
  ,buttonTextColorHover = "rgb(50,50,50)"
  ,buttonBorderColorHover = "rgb(50,50,50)"
  
  ,textboxBackColor = "rgb(60,60,60)"
  ,textboxBorderColor = "rgb(45,45,45)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(80,90,100)"
  ,textboxBorderColorSelect = "rgb(255,255,255)"
  
  ### tables
  ,tableBackColor = "rgb(45,45,45)"
  ,tableBorderColor = "#d87600"
  ,tableBorderTopSize = 0.5
  ,tableBorderRowSize = 0
  
)
customLogo <- shinyDashboardLogoDIY(
  
  boldText = "DSO522"
  ,mainText = "Final Project"
  ,textSize = 19
  ,badgeText = ""
  ,badgeTextColor = "white"
  ,badgeTextSize = 1
  ,badgeBackColor = "rgb(60,60,60)"
  ,badgeBorderRadius = 7
  
)
#### UI ####
ui <- dashboardPage(dashboardHeader(
                        title = customLogo,
                        titleWidth=271
                        
                    ), 
                    dashboardSidebar(
                        width=300,
                        sidebarMenu(
                          menuItem("HOME",tabName="home",icon=icon("home")),
                          menuItem("PLOT",tabName="plot",icon=icon("poll")),
                          menuItem("FORECAST",tabName="pred",icon=icon("chart-line"))
                        )
                    ),
                    dashboardBody(
                      theme_grey_dark,
                      tabItems(
                        tabItem(tabName="home",
                                tags$h4("Welcome to Oakland Bay Bridge Travel Time Forecast!", style="font-size:250%"),
                                br(),
                                tags$h4("DSO522 Applied Time Series Analysis for Forecasting", style="font-size:120%"),
                                tags$h4("Team 1", style="font-size:120%"),
                                br(),
                                br(),
                                tags$h4("<BODY>", style="font-size:120%"),
                                br(),
                                br(),
                                br(),
                                fileInput("file",
                                          "Please upload .csv file here:",
                                          multiple=F,
                                          accept=c(".csv")),
                                uiOutput("upload_heading"),
                                tableOutput("upload_data")

                        ),
                        tabItem(tabName="plot",
                                box(
                                  title=tags$h4("Choose a Period", style="font-size:180%"),
                                  width=20,
                                  selectInput('y', "Year:", c("2016", "2017", "2018", "2019", "2020", "ALL"))
                                ),
                                box(
                                    title=tags$h4("Choose a Visualization Type", style="font-size:180%"),
                                    width=20,
                                    selectInput('p', "Types:", c("TIME PLOT", "ACF", "PACF"))),
                                br(),
                                box(status="success",
                                    width=20,
                                    plotOutput('plot'))
                                ),
                        tabItem(tabName="pred",
                                tags$h4("Get Your Predictions!", style="font-size:250%"),
                                br(),
                                downloadButton("dl", "Download Predictions"),
                                br(),
                                br(),
                                plotOutput("pred_plot"))
                      )
                    )
       )

#### Server ####
server <- function(input, output) {
  
  fileSelector <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      train = read.csv(input$file$datapath)[,c(1:6,9,12)]
      date = as.Date(train$Date, "%m/%d/%Y")
      day = xts(train$Daily.Mean.Travel.Time..Seconds., order.by=date)
      day = na.omit(day)
      return(day)
    }
  })
  
  yearSelector <- reactive({
    train = fileSelector()
    if (input$y == "ALL") {
      return(train)
    } 
    if (input$y == "2020") {
      year = input$y
      start_date = paste(year, "-01-01", sep="")
      start_index = which(index(train)==start_date)
      train = train[start_index:length(train)]
      return(train)
    } else {
      year = input$y
      start_date = paste(year, "-01-01", sep="")
      start_index = which(index(train)==start_date)
      end_date = paste(year, "-12-31", sep="")
      end_index = which(index(train)==end_date)
      train = train[start_index:end_index]
      return(train)
    }
  })
  
  plotSelector <- reactive({
    train = yearSelector()
    if (input$p=="ACF") {
      plot <- ggAcf(train, color='#c8c8c8')+ 
        theme(
          panel.background = element_rect(fill="#3c3c3c"),
          axis.title = element_blank(),
          axis.text = element_text(colour="#FFFAFF", size = 15),
          axis.text.y = element_text(colour="#FFFAFF", size = 15),
          panel.grid = element_blank(),
          panel.border=element_blank(),
          plot.title = element_blank(),
          plot.background = element_rect(fill = "#2d2d2d",color="#2d2d2d",size=0),
          line = element_line(colour="#FFFAFF")
        )
      return(plot)
    }
    if (input$p=="PACF") {
      plot <- ggPacf(train, color='#c8c8c8')+ 
        theme(
          panel.background = element_rect(fill="#3c3c3c"),
          axis.title = element_blank(),
          axis.text = element_text(colour="#FFFAFF", size = 15),
          axis.text.y = element_text(colour="#FFFAFF", size = 15),
          panel.grid = element_blank(),
          panel.border=element_blank(),
          plot.title = element_blank(),
          plot.background = element_rect(fill = "#2d2d2d",color="#2d2d2d",size=0),
          line = element_line(colour="#FFFAFF")
        )
      return(plot)
    }
    if (input$p=="TIME PLOT") {
      plot <- autoplot(train, colour='#c8c8c8')+
        #scale_fill_manual(values=c("deepskyblue4"))+
        theme(
          panel.background = element_rect(fill="#3c3c3c"),
          axis.title = element_blank(),
          axis.text = element_text(colour="#FFFAFF", size = 15),
          axis.text.y = element_text(colour="#FFFAFF", size = 15),
          panel.grid = element_blank(),
          panel.border=element_blank(),
          plot.title = element_blank(),
          plot.background = element_rect(fill = "#2d2d2d",color="#2d2d2d",size=0),
          line = element_line(colour="#FFFAFF"),
          legend.position = "none"
        )
      return(plot)
    }
  })
  
  output$plot <- renderPlot({
    plot = plotSelector()
    print(plot)
  })
  
  output$upload_heading <- renderUI({
    file = input$file
    
    if (is.null(file)) {
      return(NULL)
    } else {
      tags$h4("Take a look at the data", style="font-size:100%")
    }
  })
  
  output$upload_data <- renderTable({
    file = input$file
    
    if (is.null(file)) {
      return(NULL)
    } else {
      data = read.csv(file$datapath)[,c(1:6,9,12)]
      colnames(data) = c("Data", "Origin.Movement.ID","Origin.Display.Name", "Destination.Movement.ID",
                         "Destination.Display.Name", "Daily.Mean.Travel.Time..Seconds.",
                         "AM.Mean.Travel.Time..Seconds.", "PM.Mean.Travel.Time..Seconds.")
      head(data)
    }
  })
  
  predictionPlot <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      train = read.csv(input$file$datapath)[,c(1:6,9,12)]
      date = as.Date(train$Date, "%m/%d/%Y")
      day = xts(train$Daily.Mean.Travel.Time..Seconds., order.by=date)
      day = na.omit(day)
      add_back_index = length(day) - 365 + 1
      add_back = day[add_back_index:(add_back_index+7-1)]
      day.lag = diff(day, lag=365, differences=1)
      model = Arima(day.lag, order=c(1,0,0), seasonal=list(order=c(1, 1, 1), period=7))
      model.pred = as.data.frame(forecast(model,h=7)$mean + add_back)
      colnames(model.pred) = c("Predictions")
      #ggplot(data=model.pred, aes(x=c(1:7),y=x,group=1)) + geom_line()
      plot = ggplot(data=model.pred, aes(x=c(1:7),y=Predictions)) + 
        geom_line(color='#c8c8c8') + geom_point(color='#d87600') + 
        theme(
        panel.background = element_rect(fill="#3c3c3c"),
        axis.title = element_blank(),
        axis.text = element_text(colour="#FFFAFF", size = 15),
        axis.text.y = element_text(colour="#FFFAFF", size = 15),
        panel.grid = element_blank(),
        panel.border=element_blank(),
        plot.title = element_blank(),
        plot.background = element_rect(fill = "#2d2d2d",color="#2d2d2d",size=0),
        line = element_line(colour="#FFFAFF"),
        legend.position = "none"
      )
      return(plot)
    }
  })
  
  predictions <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      train = read.csv(input$file$datapath)[,c(1:6,9,12)]
      date = as.Date(train$Date, "%m/%d/%Y")
      day = xts(train$Daily.Mean.Travel.Time..Seconds., order.by=date)
      day = na.omit(day)
      day.lag = diff(day, lag=365, differences=1)
      model = Arima(day.lag, order=c(1,0,0), seasonal=list(order=c(1, 1, 1), period=7))
      model.pred = forecast(model,h=7)$mean
      return(model.pred)
    }
  })
  
  output$pred_plot <- renderPlot({
    plot = predictionPlot()
    print(plot)
  })
  
  output$dl <- downloadHandler(
    filename = function() {
      paste("predictions", ".csv", sep="")
    },
    content = function(file) {
      write.csv(predictions(), file, row.names=F)
    }
  )
  

}
#### APP #### 
shinyApp(ui = ui, server = server)
