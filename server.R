## Coursera Data Science
## Developing Data Products Course Project:
## Shiny application
## server.R
## 

library(shiny)

if(!require(pyramid)){
  install.packages("pyramid")
  library(pyramid)
}
## constants for caption and info
sName <- c("Russian Empire Census", "Soviet Union Census", "Russian Federation Census")
tst <- "In general: 
        Men:
        Women:"

## load the data of the censuses
mainData <- read.csv(file = "data/ruscensus.csv", header = TRUE, sep = ";")

## routine to calculate max value of population amount axis
maxMark <- function(num){
  numOrd <- trunc(log10(num))
  num1 <- signif(num / 10^numOrd, 2)
  num2 <- trunc(num / 10^numOrd)
  return(10^numOrd * (ceiling((num1 - num2) / .25) * .25 + num2))
}

## population pyramid drawing
pyramidm <- function(data, census, breack){
  ## vector of lower limits of cohorts ages
  stepNum <- seq(from = 1, to = nrow(data), by = breack)
  if(breack > 1)
  ## vector of labels for the cohorts
    stepLab <- sapply(stepNum, FUN = function(n){paste0(as.character(n-1), "-", as.character(n+breack-2))})
  else
    stepLab <- as.character(stepNum-1)
  stepLab[length(stepLab)] <- paste0(as.character(max(stepNum)-1), "+") 
  ## calculate the population of the cohorts and bind a final data frame
  plotData <- sapply(stepNum, FUN = function(n){sum(data[n:(n+breack-1),1])})
  plotData <- as.data.frame(cbind(plotData, sapply(stepNum, FUN = function(n){sum(data[n:(n+breack-1),2])})))
  plotData <- as.data.frame(cbind(plotData, stepLab))
  plotData[nrow(plotData), 1] <- sum(data[max(stepNum):nrow(data),1])
  plotData[nrow(plotData), 2] <- sum(data[max(stepNum):nrow(data),2])
  ## calculate marks of the population amount axis
  axisMarks <- seq(0, maxMark(max(plotData[,1:2])), length.out = 5)
  ## draw the diagram
  pyramid(plotData, Laxis = axisMarks, AxisFM = "fg", Cgap = .15, Cstep = ifelse(breack < 5, 6 - breack, 1), 
          Lcol="Blue", Rcol="Red", main=paste0("Population pyramid (Russia, ", census,")"))
}

## calculate population parameters
getPopParam <- function(data){
  gPop <- sum(data[,1:2]); 
  mPop <- sum(data[,1])
  wPop <- sum(data[,2]); 
  ages <- 1:nrow(data)
  mSumAges <- 0
  for(i in ages)
    mSumAges <- mSumAges + data[i, 1] * (i-1)
  mMeanAge <- mSumAges / mPop
  wSumAges <- 0
  for(i in ages)
    wSumAges <- wSumAges + data[i, 2] * (i-1)
  wMeanAge <- wSumAges / wPop
  gSumAges <- mSumAges + wSumAges
  gMeanAge <- gSumAges / gPop
  medInt <- 1
  while(sum(data[1:medInt,1:2]) < (gPop / 2))
    medInt <- medInt + 1
  medInt <- medInt - 1
  gMedAge <- medInt + ((gPop / 2 - sum(data[1:medInt,1:2])) / sum(data[medInt+1,1:2]))
  medInt <- 1
  while(sum(data[1:medInt,1]) < (mPop / 2))
    medInt <- medInt + 1
  medInt <- medInt - 1
  mMedAge <- medInt + ((mPop / 2 - sum(data[1:medInt, 1])) / sum(data[medInt+1, 1]))
  while(sum(data[1:medInt,2]) < (wPop / 2))
    medInt <- medInt + 1
  medInt <- medInt - 1
  wMedAge <- medInt + ((wPop / 2 - sum(data[1:medInt, 2])) / sum(data[medInt+1, 2]))
  return(data.frame(total = c(paste(round(sum(data[,1:2]) / 1000000, 1), "M"),
                                  round(gMeanAge, 1), round(gMedAge, 1)),
                    men = c(paste(round(sum(data[,1]) / 1000000, 1), "M"), 
                            round(mMeanAge, 1), round(mMedAge, 1)),
                    women = c(paste(round(sum(data[,2]) / 1000000, 1), "M"),
                              round(wMeanAge, 1), round(wMedAge, 1)),
                    row.names = c("population", "mean age", "median age")))
}


shinyServer(
  function(input, output) {
    
    output$main <- renderPlot({   
      curCensus <- input$census
      curData <- mainData[,grep(curCensus, names(mainData))]
      curBreack <- input$breack
      pyramidm(curData, curCensus, curBreack)
      
    })
    
    output$caption <- renderText({
      curCensus <- input$census
      if(as.numeric(curCensus) == 1897){
        stateName <- sName[1]
      }else{
        if(as.numeric(curCensus) >= 2002){
          stateName <- sName[3]
        }else{
          stateName <- sName[2]
        }
      }  
      paste(curCensus, stateName)
      })
    
    output$info <- renderTable({
      curCensus <- input$census
      curData <- mainData[,grep(curCensus, names(mainData))]
      getPopParam(curData)
    })
  }
)