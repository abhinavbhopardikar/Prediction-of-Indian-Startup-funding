rm(list = ls())
library(shiny)
library(shinythemes)
library(MASS)
library(neuralnet)
library(ggplot2)
library(plyr)

###set seed so that we get consistant results
set.seed(123)

### store 'startup_funding' dataset in DataFrame
DataFrame <- read.csv(file="startup_funding.csv", header=TRUE, sep=",")
InputData <- read.csv(file="./cleanData/abc.csv", header=TRUE, sep=",")
#str(InputData$IndustryVertical)
#DataFrame <- startup_funding

### get rid of SNo column
DataFrame = subset(DataFrame,select = -c(SNo,Remarks)) 

##convert the AmountInUSD to numeric type
DataFrame$AmountInUSD=as.numeric(gsub(",","",DataFrame$AmountInUSD))
dataCopy <- DataFrame
InputCopy <- InputData
str(DataFrame)

##remove all na records
is.na(DataFrame)
DataFrame = na.omit(DataFrame)  ### discard NA records
str(DataFrame)

mm11 <- model.matrix(~InvestmentType, data = InputData)
mm22 <- model.matrix(~IndustryVertical, data = InputData)
mm33 <- model.matrix(~CityLocation, data = InputData)
#InputData = subset(InputData,select = -c(Date,StartupName,SubVertical,CityLocation,IndustryVertical,InvestorsName,InvestmentType))
InputData = subset(InputData,select = -c(CityLocation,IndustryVertical,InvestmentType))
result2 <- InputData
result1 <- InputData
#result2 = subset(result,select=c(AmountInUSD))

maxV <- apply(InputData,2,max)
minV <- apply(InputData,2,min)

InputData$AmountInUSD <- scale(InputData$AmountInUSD,center = minV,scale = maxV-minV)
InputData <- data.frame(InputData,mm11)
InputData <- data.frame(InputData,mm22)
InputData <- data.frame(InputData,mm33)


###Create train and test dataset
i <- sample(1:nrow(InputData)-1)
traindf2 <- InputData[i,]
testdf2 <- InputData[-i,]

j <- sample(1:nrow(InputData),690)
traindf <- InputData[j,]
testdf <- InputData[-j,]

allVars<-colnames(InputData)
predictorVars<-allVars[!allVars%in%"AmountInUSD"]
predictorVars<-paste(predictorVars,collapse = "+")
str(predictorVars)
form=as.formula(paste("AmountInUSD~",predictorVars,collapse = "+"))

neuralModel1<- neuralnet(formula=form,hidden=c(4,2),linear.output = T,data=traindf)
neuralModel2<- neuralnet(formula=form,hidden=c(4,2),linear.output = T,data=traindf2)
#subDataFrame <- cleanDataCopy
#write.csv(subDataFrame,file="./cleanData/abc.csv",row.names = FALSE, quote = TRUE, sep = ",")
#plot(neuralModel2)
#str(predictions)


InputData$unscaled <- InputData$AmountInUSD * attr(InputData$AmountInUSD, 'scaled:scale') + attr(InputData$AmountInUSD, 'scaled:center')
testdf2 <- InputData[-i,]
testdf <- InputData[-j,]
###predictions <- predictions$net.result

result2$AmountInUSD <- scale(result2$AmountInUSD,center = minV,scale = maxV-minV)
result1$AmountInUSD <- scale(result2$AmountInUSD,center = minV,scale = maxV-minV)

predictions2 <- compute(neuralModel2,testdf2[,2:64])
plot(neuralModel2)
predictions1 <- compute(neuralModel1,testdf[,2:64])
predictionResult2 <- testdf2
predictionResult1 <- predictions1$net.result
predictionResult2 <- subset(predictionResult2,select=c(AmountInUSD))
predictionResult1 <- subset(predictionResult1,select=c(AmountInUSD))
predictionResult2$AmountInUSD <- predictions2$net.result

str(InputData)

#ind2 <- 1:nrow(predictions2$net.result)
result2[1,1]<- predictionResult2
result1[-j,]<- predictionResult1
result2 <- result2[1]
result2$unscaled <- result2$AmountInUSD * attr(result2$AmountInUSD, 'scaled:scale') + attr(result2$AmountInUSD, 'scaled:center')
testdf2$prediction <- result2[1,]$unscaled
output <- testdf2
output$ExpectedAmtInUSD <- testdf2$unscaled
output$PredictedAmtInUSD <- testdf2$prediction
#last <- as.numeric(nrow(InputCopy))
#output$Industry <- InputCopy[last,]$IndustryVertical
output <- subset(output,select=c(ExpectedAmtInUSD,PredictedAmtInUSD))
write.csv(output,file="./cleanData/output.csv",row.names = FALSE, quote = TRUE, sep = ",")

result1$unscaled <- result1$AmountInUSD * attr(result1$AmountInUSD, 'scaled:scale') + attr(result1$AmountInUSD, 'scaled:center')
testdf$predictions <- result1[-j,]$unscaled 
#comparisonData <- table(predictions,testDF$unscaled)
resultSet <- testdf
resultSet$ActualAmtInUSD <- testdf$unscaled
resultSet$PredictedAmtInUSD <- testdf$predictions
resultSet <- subset(resultSet,select = c(ActualAmtInUSD,PredictedAmtInUSD))

outputDir <- "cleanData"
saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("abc.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.table(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE, sep = ",", append = TRUE,col.names = FALSE
  )
  #write.csv(output,file="./cleanData/output.csv",row.names = FALSE, quote = TRUE, sep = ",")
}


loadData <- function() {
  # Read all the files into a list
  #files <- list.files(outputDir, full.names = TRUE)
  data <- lapply("./cleanData/output.csv", read.csv, stringsAsFactors = FALSE) 
  data <- do.call(rbind, data)
  data
}

# Define the fields we want to save from the form
fields <- c("IndustryVertical","CityLocation","InvestmentType","AmountInUSD")

# Shiny app with 4 fields that the user can submit data for
shinyApp(
  ui = fluidPage(theme = shinytheme("slate"),
    titlePanel("Prediction of Indian Startup Funding"),             
    sidebarLayout(
      sidebarPanel(
        selectInput("IndustryVertical", "Select the industry vertical", choices =c("Consumer Internet", "eCommerce", "ECommerce", "Education", "Food & Beverage", "Healthcare", "Logistics", "Technology"), selected = "Consumer Internet"),
        br(),
        selectInput("CityLocation", "Select the location", choices =c("Bangalore", "Mumbai", "Chennai", "New Delhi", "Gurgaon", "Ahmedabad", "Hyderabad"), selected = "Bangalore"),
        br(),
        selectInput("InvestmentType", "Select the type of investment", choices =c("Private Equity", "Seed Funding"), selected = "Private Equity"),
        br(),
        numericInput("AmountInUSD", "Enter the expected investment", ""),
        br(),
        actionButton("submit", "Submit", style="background:#ff3300")
      ),
      mainPanel(
        tabsetPanel(type="tab",
                    #tabPanel("Neural Net", plotOutput("nnModel", width = "100%", height = "800px")),
                    tabPanel("Plot", plotOutput("myhist", click = "plot_click")),
                    tabPanel("Prediction", tableOutput("responses")),
                    tabPanel("Test result",tableOutput("result")),
                    tabPanel("Data", tableOutput("dataset"))
        )
       # DT::dataTableOutput("responses", width = 300), tags$hr()
      )
    )
  ),
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
      #quit(save="default",status = 0, runLast = TRUE)
      stopApp()
      #runApp()
      #computeData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- renderTable({
      input$submit
      loadData()
    })
    
    output$result <- renderTable({
      #data <- lapply("./cleanData/comparison.csv", read.csv, stringsAsFactors = FALSE)
      #data <- do.call(rbind, data)
      #data
      resultSet
    })
    
    output$dataset <- renderTable({
      dataCopy
    })
    
    output$myhist <- renderPlot({
      plot(testdf$AmountInUSD,predictions1$net.result,col='red',main = 'Real vs Predicted',pch=1,cex=0.9,type = "p",xlab = "Actual",ylab = "Predicted")
      abline(0,1,col="blue")
    })
  }
)