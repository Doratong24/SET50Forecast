#library("quantmod")
#library("lubridate")

list.of.packages <- c("e1071", "TTR","rminer","rpart","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(readr)
if(exists("SET50") == FALSE){
  SET50 <- read_csv("SET50.csv", 
  col_types = cols(Timestamp = col_date(format = "%d/%m/%y"), 
  X7 = col_skip()))
  }


library("e1071")
library("TTR")
library("rminer")
library("rpart")


# windowDays = 0
# foldNumber = 10
# emaB = FALSE
# obvB = TRUE
# macdB = TRUE
# rsiB = FALSE
# vmaB = FALSE
# rocB=FALSE
# cmoB=TRUE
# cciB = FALSE
# wprB = FALSE
# ahead = 100
# ker = "radial"
#svmT(windowDays,foldNumber,emaB,obvB,macdB,rsiB,vmaB,rocB,cmoB,cciB,wprB,ahead,ker)

svmT <- function(windowDays,foldNumber,emaB,obvB,macdB,rsiB,vmaB,rocB,cmoB,cciB,wprB,ahead,ker){
  Price_Ema10 <- EMA(SET50$SET50.Open, n = 10, wilder = FALSE, ration = NULL)
  obvData <- OBV(SET50$SET50.Open, SET50$Vol)
  macdtemp = MACD(SET50$SET50.Close, nFast = 12, nSlow = 26, nSig = 9, maType = EMA, percent = TRUE)
  macd = macdtemp[,1]-macdtemp[,2]
  rsi <- RSI(SET50$SET50.Close, n = 14, maType = EMA, SET50$Vol)
  vma <- EVWMA(SET50$SET50.Close, SET50$Vol, ratio = 1)
  roc <- ROC(SET50$SET50.Close,n = 2)
  cmo <- CMO(SET50$SET50.Close,n=14)
  cci <- CCI(SET50$SET50.Close,n=14)
  wpr <- WPR(SET50$SET50.Close,n=14)
  
  features <- cbind(Price_Ema10, obvData, macd, rsi, vma,roc,cmo,cci,wpr)
  #features<-features[34:(nrow(features)-ahead),]
  
  features<-features[34:(nrow(features)-ahead),]
  features <- scale(features)
  
  
  # for (col in 1:ncol(features)){
  #   features[,col] = (features[,col] - min(features[,col]))/(max(features[,col])-min(features[,col]))
  # 
  # }
  
  
  Price1 = SET50$SET50.Close[(34+ahead):length(SET50$SET50.Close)]
  Price2 = SET50$SET50.Close[34:(length(SET50$SET50.Close)-ahead)]
  Price <- Price2-Price1
  #Price <- Price[34:length(Price)]
  #UpDown <- ifelse(Price < 0, 'Up', 'Down')
  UpDown <- ifelse(Price > 0, 1, 0)
  
  closePrice <-SET50$SET50.Close[34:(nrow(SET50)-1)]
  
  
  
  #windowDays = 110
  
  TP = 0
  TN= 0
  FP = 0
  FN = 0
  TP2 = 0
  TN2= 0
  FP2 = 0
  FN2 = 0
  totalTime = 0
  totalTime2 = 0
  
  if(windowDays>0){
  
    featureSliding = c(1:(nrow(features)-windowDays))
  
    
    if(emaB){
      emaLag = CasesSeries(features[,1], c(1:windowDays))
      colnames(emaLag) <- paste("EMA", colnames(emaLag), sep = "_")
      featureSliding = cbind(featureSliding,emaLag)
      print("EMA")
    }
    if(obvB){
      obvLag = CasesSeries(features[,2], c(1:windowDays))
      colnames(obvLag) <- paste("OBV", colnames(obvLag), sep = "_")
      featureSliding = cbind(featureSliding,obvLag)
    }
    if(macdB){
      macdLag = CasesSeries(features[,3], c(1:windowDays))
      colnames(macdLag) <- paste("MACD", colnames(macdLag), sep = "_")
      featureSliding = cbind(featureSliding,macdLag)
    }
    if(vmaB){
    vmaLag = CasesSeries(features[,5], c(1:windowDays))
    colnames(vmaLag) <- paste("VMA", colnames(vmaLag), sep = "_")
    featureSliding = cbind(featureSliding,vmaLag)
    }
    if(rsiB){
    rsiLag = CasesSeries(features[,4], c(1:windowDays))
    colnames(rsiLag) <- paste("RSI", colnames(rsiLag), sep = "_")
    featureSliding = cbind(featureSliding,rsiLag)
    }
    
    if(rocB){
      rocLag = CasesSeries(features[,6], c(1:windowDays))
      colnames(rocLag) <- paste("ROC", colnames(rocLag), sep = "_")
      featureSliding = cbind(featureSliding,rocLag)
    }
    if(cmoB){
      cmoLag = CasesSeries(features[,7], c(1:windowDays))
      colnames(cmoLag) <- paste("CMO", colnames(cmoLag), sep = "_")
      featureSliding = cbind(featureSliding,cmoLag)
    }
    if(cciB){
      cciLag = CasesSeries(features[,8], c(1:windowDays))
      colnames(cciLag) <- paste("CCI", colnames(cciLag), sep = "_")
      featureSliding = cbind(featureSliding,cciLag)
    }
    if(wprB){
      wprLag = CasesSeries(features[,9], c(1:windowDays))
      colnames(wprLag) <- paste("WPR", colnames(wprLag), sep = "_")
      featureSliding = cbind(featureSliding,wprLag)
    }
    
    #featureSliding = cbind(emaLag,obvLag,macdLag,vmaLag,rsiLag,rocLag,cmoLag,cciLag,wprLag)
    #featureSliding = cbind(emaLag,obvLag,macdLag,vmaLag,cmoLag,rsiLag)
    
    #remove feature on last day
    featureSliding[,2:ncol(featureSliding)]
    #add  featureSliding = featureSliding[1:(nrow(featureSliding)-1),]
    
    resultLagTemp  = CasesSeries(UpDown, c(1:windowDays))
     
    #resultLag = resultLagTemp$y
    resultUpDown = resultLagTemp$y
    #resultUpDown = resultLag[(2):(length(resultLag))]
  }else{
    #featureSliding = as.data.frame(sapply(features, as.numeric)) 
    featureSliding = features
    resultUpDown = UpDown
    
  }

  data2 = cbind(featureSliding,resultUpDown)
  
  data <- data2[sample(nrow(data2)),] 
  
  folds <- cut(seq(1,nrow(data)),breaks=foldNumber,labels=FALSE)
  
  
  
  
  for(i in 1:foldNumber){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]
    
    
    featureTrain <- data.frame(trainData[,1:(ncol(trainData)-1)])
    #closePriceTrain <- trainData[,(ncol(trainData)-1)]
    #UpDownC <- trainData[,(ncol(trainData))]
    UpDownC <- ifelse(trainData[,(ncol(trainData))] >0, 'Up', 'Down')
    
    
    featureTest <- data.frame(testData[,1:(ncol(testData)-1)])
   
    #UpDownT <- testData[,(ncol(testData))]
    UpDownT <- ifelse(testData[,(ncol(testData))]> 0, 'Up', 'Down')
    
    start.time <- Sys.time()
    svmModel = svm(x = featureTrain, y = UpDownC, type = "C", kernel = ker)
    #svmModel = svm(x = featureTrain, y = UpDownC, type = "C", kernel = "polynomial",degree = 4, gamma = 3,cost = 1)
    end.time <- Sys.time()
    
    start2.time <- Sys.time()
    dtModel = rpart(UpDownC~.,data = featureTrain)
    end2.time <- Sys.time()
    
    
    
 
    
 
    time.taken <- end.time - start.time
    totalTime = totalTime + time.taken
    
    time2.taken <- end2.time - start2.time
    totalTime2 = totalTime2 + time2.taken
    
    
    fitSVM <- predict(svmModel,featureTest)
    fitTree <- predict(dtModel,featureTest)
    fitResult <- ifelse(fitTree[,1] > fitTree[,2], "Down", "Up")
    
    
    
    tableSVM = table(Actual = UpDownT, Fitted = fitSVM)
    TP = TP+(100*tableSVM[2,2]/(tableSVM[1,1]+tableSVM[1,2]+tableSVM[2,1]+tableSVM[2,2]))
    TN = TN+(100*tableSVM[1,1]/(tableSVM[1,1]+tableSVM[1,2]+tableSVM[2,1]+tableSVM[2,2]))
    FP = FP + (100*tableSVM[1,2]/(tableSVM[1,1]+tableSVM[1,2]+tableSVM[2,1]+tableSVM[2,2]))
    FN = FN + (100*tableSVM[2,1]/(tableSVM[1,1]+tableSVM[1,2]+tableSVM[2,1]+tableSVM[2,2]))
    
    tableTree2 = table(Actual = UpDownT, Fitted = fitResult)
    totalPercent = tryCatch(tableTree2[2,2],error = function(e) 0) + tryCatch(tableTree2[1,1],error = function(e) 0)+tryCatch(tableTree2[2,1],error = function(e) 0)+tryCatch(tableTree2[1,2],error = function(e) 0)
    TP2 = TP2+(100*tryCatch(tableTree2[2,2],error = function(e) 0) /totalPercent)
    TN2 = TN2+(100*tryCatch(tableTree2[1,1],error = function(e) 0)/totalPercent)
    FP2 = FP2 + (100*tryCatch(tableTree2[1,2],error = function(e) 0)/totalPercent)
    FN2 = FN2 + (100*tryCatch(tableTree2[2,1],error = function(e) 0)/totalPercent)
    
    print(tableSVM)
    print(tableTree2)
  }
  
  
  # result <- list("TP"=TP/10,"TN"=TN/10,"FP"=FP/10,"FN"=FN/10,"AvgTime" = totalTime/10)
  # result <- paste("SVM : ",
  #                 "TP ",TP/foldNumber,"TN ",TN/foldNumber,"FP ",FP/foldNumber,"FN ",FN/foldNumber,"AvgTime " , totalTime/foldNumber,
  #                 "Tree : ",
  #                 "TP ",TP2/foldNumber,"TN ",TN2/foldNumber,"FP ",FP2/foldNumber,"FN ",FN2/foldNumber,"AvgTime " , totalTime2/foldNumber)
  
  precisionTree = TP2/(TP2+FP2) *100
  precisionSVM = TP/(TP+FP) *100
  
  recallTree = TP2/(TP2+FN2) *100
  recallSVM = TP/(TP+FN) *100
  accuracyTree = (TP2+TN2)/(TP2+TN2+FP2+FN2) *100
  accuracySVM = (TP+TN)/(TP+TN+FP+FN)* 100
  
  fAccSVM = (2*precisionSVM*recallSVM)/(recallSVM+precisionSVM) 
  fAccTree = (2*precisionTree*recallTree)/(recallTree+precisionTree) 
  
  digit = 4
  result <- paste(round(TP/foldNumber,digits = digit),round(TN/foldNumber,digits = digit),round(FP/foldNumber,digits = digit),round(FN/foldNumber, digits=digit),round(totalTime/foldNumber, digits = digit),
                  round(TP2/foldNumber, digits= digit),round(TN2/foldNumber, digits = digit),round(FP2/foldNumber, digits = digit),round(FN2/foldNumber, digits= digit),round(totalTime2/foldNumber, digits = digit),
                  round(precisionSVM, digits = digit),round(precisionTree, digits = digit),
                  round(recallSVM, digits = digit),round(recallTree, digits = digit),
                  round(accuracySVM, digits = digit),round(accuracyTree, digits = digit),
                  round(fAccSVM, digits = digit),round(fAccTree, digits = digit),
                  sep = ",")
  
  
  
  print(result)
  print("+++++++")
  print(accuracySVM)
  print(accuracyTree)
  
  return(result)
}




