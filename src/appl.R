library(shiny)
library(ggplot2)
library(dplyr)
library(forecast)
library(plotly)
library(DT)        
library(dplyr)     
library(changepoint)
library(magrittr)
library(plotly)
library(shinyjs)


# shiny user interface function
############# ui ####################
ui <-  navbarPage(
  
  shinyjs::useShinyjs(),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }",
             ".shiny-output-error-validation {color: red;} "
  ),
  title="Time Series Forecasting",
  theme="bootstrap.css",
  inverse=TRUE,
  
  tabPanel("Time Series Forecasting", icon = icon("line-chart"),
           
           fluidPage(
             
             titlePanel('Time Series Forecasting', windowTitle='Predicted Oil Forecast'),
             
             
             #fluidRow(
               sidebarPanel(
                 fileInput("i_file", "Upload your CSV file",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 
                 selectInput("vary","Select forecasting variable", choices = c("")),

                 selectInput(inputId = "algorithm", label = "Select changepoint measure",
                             choices = c(   "None"="None",
                                            "Changepoint in mean" = "cpt.mean",
                                            "Changepoint in variance" = "cpt.var",
                                            "Changepoint in both mean and variance" = "cpt.meanvar"
                             )),
                 
                 
                 
                 # common parameters
                 conditionalPanel(condition = "input.algorithm != 'None'",
                                  
                                  
                                  radioButtons(inputId = "method", label = "Select method",
                                               choices = c("Binary Segmentation" = "BinSeg",
                                                           "PELT" = "PELT",
                                                           "Segmentation Neighborhoods" = "SegNeigh")
                                  ),
                                  
                                  selectInput(inputId = "penalty", label = "Select penalty", 
                                              choices = c("MBIC", "SIC", "BIC", "AIC", "Hannan-Quinn", "Asymptotic"), multiple = F),
                                  
                                  
                                  conditionalPanel(condition = "input.method != 'PELT'",
                                                   
                                                   sliderInput(inputId = "Q", label = "Maximum number of changepoints", min = 1, max=100, value = 12)
                                                   
                                  ),
                                  
                                  
                                  numericInput(inputId = "minseglen", label = "Minimum segment length", min = 1, value = 10)
                                  
                 ,
                 # cpt.mean
                 conditionalPanel(condition = "input.algorithm == 'cpt.mean'",
                                  selectInput(inputId = "distribution", label = "Data distribution", choices = c("Normal", "CUSUM"))
                 ),
                 # cpt.var
                 conditionalPanel(condition = "input.algorithm == 'cpt.var'",
                                  
                                  selectInput(inputId = "distribution", label = "Data distribution", choices = c("Normal", "CSS")),
                                  
                                  selectInput(inputId = "known_mean", label = "Known mean?", choices = c("TRUE", "FALSE"), selected = "FALSE"),
                                  
                                  conditionalPanel(condition = "input.known_mean == 'TRUE'",
                                                   numericInput(inputId = "mu", label = "Mean", value = 0)
                                  )
                 ),
                 # cpt.meanvar
                 conditionalPanel(condition = "input.algorithm == 'cpt.meanvar'",
                                  
                                  selectInput(inputId = "distribution", label = "Data distribution",
                                              choices = c("Normal", "Gamma", "Exponential", "Poisson")
                                  ),
                                  
                                  conditionalPanel(condition = "input.distribution == 'Gamma'",
                                                   numericInput(inputId = "shape", label = "Assumed shape of parameter for Gamma", value = 1)
                                  )
                                  
                 )),

                 br(),
                 br(),
                
                 splitLayout(
                   downloadButton("downloadData", "Download data"),

                   
                    width=4) 
                 ),
             
             mainPanel(
               uiOutput("error"),
               tabsetPanel(type = "tabs",
                           tabPanel("Data", 
                                    br(),
                                    br(),
                                    splitLayout(
                                      plotlyOutput("RawPlot"),
                                      plotlyOutput("HisPlot")
                                    ),
                                    br(),
                                    br(),
                                    verbatimTextOutput("summary")
                                    
                           ),
                           tabPanel("Correlations",
                                    plotOutput("AcfPlot"),
                                    plotOutput("PacfPlot")
                           ),
                          tabPanel("TS Decomposition",
                                   plotOutput("components.ts"),
                                   plotOutput("ts_detrend"),
                                   plotOutput("res_check")),
                          tabPanel(
                            
                            "Changepoints",
 
                                   br(),
                                   plotlyOutput("cusum_man"),
                                  checkboxInput("show_console","Show console",value = T),
                            
                                   verbatimTextOutput("console"),
                                   verbatimTextOutput("segment_stats")
                          ),
                          
                           tabPanel("Time-series fits",
                                    plotlyOutput("sims_plot"),
                                    br(),
                                    br(),
                                    plotlyOutput("resisuals"),
                                    br(),
                                    br(),
                                    dataTableOutput("manhattan")
                                    ),
                          
                          tabPanel("Simulations",
                                   br(),
                                   numericInput(inputId = "number_of_points",label = "Number of points to simulate",value = 100),
                                   splitLayout(
                                     cellWidths = c("100","100"),
                                     numericInput("ar","ar",value = 0.5),
                                     numericInput("ma", "ma", value = 1)
                                   )
                                   ,
                                   br(),
                                   splitLayout(
                                     cellWidths = c("100","100"),
                                     numericInput("p","p",value = ""),
                                     numericInput("d", "d",value = ""),
                                     numericInput("q", "q",value = "")
                                   ),
                                   br(),
                                   
                                   plotlyOutput("arima_sim")
                                   )
                          
               )#end tabset panel
             ) #main panel
           ) #fluid page
  )#tab panel
) 


server <- function(input, output,session){
 
  # read data and extract numeric columns
  data <- reactive({
    
    inFile <- input$i_file
    if(is.null(inFile)){
      return(NULL)
    }
    
    df <- read.csv(inFile$datapath,
                   header = T,
                   strip.white=T,
                   stringsAsFactors=F,
                   fill=T)
    
    numeric_columns <- sapply(df, class) %>% .[. %in% c("numeric")] %>% names
    
    updateSelectInput(session, 'vary', choices = numeric_columns)
    
    return(df)

  })
  # transform date column as date 
  data_prep <- reactive({
    data <- data()
    data$Date %<>% as.Date()
    data <- data %>% arrange(Date)
    
    return(data)
  })
  # fit multiple model from forecast package
  data_fitted <- reactive({
    
    data <- data_prep()
    holt_fit <- HoltWinters(data[,input$vary],gamma = F)
    data$holt_winters_residuals <-  c(residuals(holt_fit)[1:2],residuals(holt_fit))
    data$holt_winters <- c(as.numeric(holt_fit$fitted[,"xhat"])[1:2],as.numeric(holt_fit$fitted[,"xhat"]))
    

    baggedmodel <- baggedModel(data[,input$vary])
    data$baggedmodel_residuals <- residuals(baggedmodel)
    data$baggedmodel <- baggedmodel$fitted
    
    batsmodel <- bats(data[,input$vary])
    data$bats_residuals <- residuals(batsmodel)
    data$bats <- batsmodel$fitted.values
    
    
    # stas_column <- diff(data[,input$vary])
    # Lambda<- BoxCox.lambda(stas_column)
  
    model_arima <-auto.arima(data[,input$vary],approximation = FALSE,allowdrift = T,allowmean = T)
    data$arima <- model_arima$fitted
    data$arima_residuals <- residuals(model_arima)

    return(data)

    
  })
  # change points algorithm
  cpt_res <- reactive({
    
    df <- data_prep()
    
    
    tryCatch({
      if(input$algorithm %in% c("cpt.mean", "cpt.var", "cpt.meanvar")){
        res <- get(input$algorithm)(data = df[,input$vary], 
                                    penalty = input$penalty,
                                    #pen.value = input$pen.val,
                                    method = input$method,
                                    Q = input$Q,
                                    minseglen = input$minseglen,
                                    test.stat = input$distribution)
        imadeit <- T
        
      }  else{
        res <- "Select an Algorithm to see the results here..."
      }
      
      return(res)},error = function(e){
        imadeit <- F
        e
      }
    )
    
  })
  # calculate manhattan distance
  manhattan_distance <- reactive({
    
    data <- data_fitted()
    
    holt_winters_manhattan <- sum(abs(data$holt_winters_residuals))
    baggedmodel_manhattan <- sum(abs(data$baggedmodel_residuals))
    bats_manhattan <- sum(abs(data$bats_residuals))
    arima_manhattan <- sum(abs(data$arima_residuals))
    
    df <-t(data.frame(holt_winters = holt_winters_manhattan,
                      baggedmodel=baggedmodel_manhattan,
                      bats =bats_manhattan,
                      arima =arima_manhattan)) %>% as.data.frame()
    
    
    colnames(df)[1] <- "manhattan_score"
    return(df)
  })
  # display manhattan distance 
  output$manhattan <- renderDataTable({
    
    manhattan <- manhattan_distance()
    
   })
  # display descriptive statistics 
  output$summary <- renderPrint({
    data <- data_prep()
    summary(data[,input$vary])
  })
  # display cusum manhattan 
  output$cusum_man <- renderPlotly({

    data <- data_prep()
    cpt_res <- cpt_res()


    segment_id <- rep(1:length(cpt_res@cpts), times = c(cpt_res@cpts[1], diff(cpt_res@cpts)))
    data$segment_id <- segment_id
    
    
    segment_mean_mid_value <- data %>%
      group_by(segment_id) %>%
      summarise(mean_mid_value = mean(Value1)) %>%
      merge(data,.,by = "segment_id")
    

    some_data <- segment_mean_mid_value %>%
      group_by(segment_id) %>%
      mutate(diff = Value1 - mean_mid_value,
             cumsum_diff = cumsum(diff),
             cumsum_lag =lag(cumsum_diff),
             cumsum_mean_mid = cumsum_lag + mean_mid_value,
             cumsum_mean_mid = case_when(is.na(cumsum_mean_mid) ~ mean_mid_value,
                                         TRUE ~ cumsum_mean_mid)) %>% ungroup()

    p <-ggplot(some_data) +
      geom_point(aes(x = Date, y = Value1), colour = "blue") + 
      geom_line(aes(x = Date, y = cumsum_mean_mid),colour = "red") +
      geom_line(aes(x = Date, y = mean_mid_value), colour = "orange") + ggtitle("Data Manhattan and Seg CuSum")+
      ylab(input$vary)

    ggplotly(p)

    return(p)

  })
  # create arima simulations data
  arima_sim_data <- reactive({
    
    arima.sim(n=input$number_of_points, list(order = c(input$p,input$d,input$q),ar = input$ar,ma = input$ma))
    
  })
  # display arima sim plot
  output$arima_sim <- renderPlotly({

    arima_sim_data <- arima_sim_data()
    arima_sim_data <- data_frame(value = arima_sim_data)
    arima_sim_data$index <- 1:nrow(arima_sim_data)
    
    p <-ggplot(arima_sim_data) +
      geom_line(aes(index, value),color = "red") + ggtitle("Arima Simulation") +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  # plot initial data 
  output$RawPlot<-  renderPlotly({
     data <- data_prep()
     p <- ggplot(data,aes_string(x = "Date", y = input$vary)) + geom_line() 
     ggplotly(p)
      })
  # histogram plot
  output$HisPlot <- renderPlotly({
     data <- data_prep()
     p<-ggplot(data, aes_string(x=input$vary)) +
          geom_histogram(aes(y=..density..), colour="black", fill="white")+
          geom_density(alpha=.2, fill="#FF6666")
        ggplotly(p)
      })
  # acf plot     
  output$AcfPlot <- renderPlot({
      data <- data_prep()
      acf(data[,input$vary], main = "Acf")})
  # pacf plot  
  output$PacfPlot <- renderPlot({
      data <- data_prep()
      
      Pacf(data[,input$vary], main = "PACF")})
  # Differenced and Stationary plot    
  output$ts_detrend <- renderPlot({
    data <- data_prep()
    data_diff <- diff(data$Value1, lag=frequency(data$Value1), differences=1)
    plot(data_diff, type="l", main="Differenced and Stationary")})
  # decomposition of time serie 
  output$components.ts <- renderPlot({
      
      data <- data_prep()
      tsData = ts(data$Value1, start = c(2010,4), frequency = 12)
      components.ts = decompose(tsData)
      plot(components.ts)})
  # display console 
  output$console <- renderPrint({
    if(input$show_console) cpt_res()
  })
  # display simulations
  output$sims_plot <- renderPlotly({
    
    data <- data_fitted()
    cpt_res <- cpt_res()
    segment_id <- rep(1:length(cpt_res@cpts), times = c(cpt_res@cpts[1], diff(cpt_res@cpts)))
    data$segment_id <- segment_id

    segment_mean_mid_value<- data %>%
      group_by(segment_id) %>%
      summarise(mean_mid_value = mean(Value1)) %>%
      merge(data,.,by = "segment_id")
    


    # data$arima_fitted <- df[,input$vary] + as.numeric(model_residuals)
    # 
    # 
    # holt_fit <- HoltWinters(data[,input$vary],gamma = F)
    # data$holt_winters <- c(as.numeric(holt_fit$fitted[,"xhat"])[1:2],as.numeric(holt_fit$fitted[,"xhat"]))
    # 
    # baggedmodel <- baggedModel(data[,input$vary])
    # data$baggedmodel <- baggedmodel$fitted
    # 
    # batsmodel <- bats(df$Value1)
    # data$bats <- model$fitted.values

    p <-ggplot(data) +
      geom_line(aes_string(x = "Date", y = input$vary)) +
      geom_line(aes_string(x = "Date", y = "arima",colour = '"arima"'))+
      geom_line(data = segment_mean_mid_value,aes_string(x = "Date", y = "mean_mid_value", colour = '"changepoints"'))+
      geom_line(aes_string(x = "Date", y = "holt_winters", colour = '"holt_winters"')) +
      geom_line(aes_string(x = "Date", y = "baggedmodel", colour = '"baggedmodel"')) +
      geom_line(aes_string(x = "Date", y = "bats", colour = '"bats"')) 
            
    
    ggplotly(p)
  })
  # display residuals 
  output$resisuals <- renderPlotly({
    
    data <- data_fitted()
    
    cpt_res <- cpt_res()
    segment_id <- rep(1:length(cpt_res@cpts), times = c(cpt_res@cpts[1], diff(cpt_res@cpts)))
    data$segment_id <- segment_id
    
    segment_mean_mid_value<- data %>%
      group_by(segment_id) %>%
      summarise(mean_mid_value = mean(Value1)) %>%
      merge(data,.,by = "segment_id")
    
    some_data <- segment_mean_mid_value %>%
      group_by(segment_id) %>%
      mutate(diff = Value1 - mean_mid_value,
             cumsum_diff = cumsum(diff),
             cumsum_lag =lag(cumsum_diff),
             cumsum_mean_mid = cumsum_lag + mean_mid_value,
             cumsum_mean_mid = case_when(is.na(cumsum_mean_mid) ~ mean_mid_value,
                                         TRUE ~ cumsum_mean_mid)) %>% ungroup()

    p <-ggplot(data) +
      geom_line(aes_string(x = "Date", y = "holt_winters_residuals",colour = '"holt_winters_residuals"'))+
      geom_line(aes_string(x = "Date", y = "baggedmodel_residuals", colour = '"baggedmodel_residuals"')) +
      geom_line(aes_string(x = "Date", y = "bats_residuals", colour = '"bats_residuals"')) +
      geom_line(aes_string(x = "Date", y = "arima_residuals", colour = '"arima_residuals"')) +
      geom_line(data = some_data, aes_string(x = "Date", y = "diff", colour = "'changepoint_residuals'")) +
      ylab("residuals")
    
    
    ggplotly(p)
  })
  # download funcionality 
  datasetInput <- reactive({
    data_fitted()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$data, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  

}

  
shinyApp(ui,server)