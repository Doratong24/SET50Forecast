library(shiny)

source('SVM.r')



# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  ntext <- eventReactive(input$calculateButton, {
    if(input$emaCheckBox||
       input$obvCheckBox||
       input$macdCheckBox||
       input$rsiCheckBox||
       input$evwmaCheckBox||
       input$rocCheckBox||
       input$cmoCheckBox||
       input$cciCheckBox||
       input$wprCheckBox){
              strsplit(svmT(input$lag,input$fold,
                   input$emaCheckBox,
                   input$obvCheckBox,
                   input$macdCheckBox,
                   input$rsiCheckBox,
                   input$evwmaCheckBox,
                   input$rocCheckBox,
                   input$cmoCheckBox,
                   input$cciCheckBox,
                   input$wprCheckBox,1),",")[[1]]
    
  }else{" "}
    
  })
  
  # output$accuracySVM<-renderText({
  #   ntext()[15]
  # })
  # 
  # output$accuracyTree<-renderText({
  #   ntext()[16]
  # })
  # 
  # output$recallSVM<-renderText({
  #   ntext()[13]
  # })
  # 
  # output$recallTree<-renderText({
  #   ntext()[14]
  # })
  # 
  # output$precisionSVM<-renderText({
  #  ntext()[11]
  # })
  # output$precisionTree<-renderText({
  #   ntext()[12]
  # })
  # 
  output$avgTimeTree<-renderText({
    (ntext()[10])
  })
  
  output$avgTimeSVM<-renderText({
    (ntext()[5])
  })
  
  
  
  output$evalSVM <- renderTable({
    tab = matrix(c(ntext()[11],ntext()[13],ntext()[15], ntext()[17]),ncol=4)
    colnames(tab) = c('Precision','Recall', 'Accuracy', 'F-Accuracy')
    # tab = matrix(c(1,2,3,4),ncol=2)
    
    return(tab)
  })
  
  output$evalTree <- renderTable({
    tab = matrix(c(ntext()[12],ntext()[14],ntext()[16], ntext()[18]),ncol=4)
    colnames(tab) = c('Precision','Recall', 'Accuracy', 'F-Accuracy')
    # tab = matrix(c(1,2,3,4),ncol=2)
    
    return(tab)
  })
  
  
  

  output$matrixSVM <- renderTable({
    tab = matrix(c("Positive Fitted","Negative Fitted",ntext()[1],ntext()[4],ntext()[3],ntext()[2]),ncol=3)
    colnames(tab) = c(' ','Positive Actual','Negative Actual')
   # tab = matrix(c(1,2,3,4),ncol=2)
    
    return(tab)
    })
  
  output$matrixTree <- renderTable({
    tab = matrix(c("Positive Fitted","Negative Fitted",ntext()[6],ntext()[9],ntext()[8],ntext()[7]),ncol=3)
    colnames(tab) = c(' ','Positive Actual','Negative Actual')
    # tab = matrix(c(1,2,3,4),ncol=2)
    
    return(tab)
  })
  
})