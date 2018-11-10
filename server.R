server <- function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  ## Code to read file in and load into variable selections
  data <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df))
    
    
    
    
    return(df)
    
  })
  
  # #Combine the selected variables into a new data frame
  # selectedData <- reactive({
  #   data()[,input$xcol,input$ycol]
  # })
  
  ## Plot output code 
  output$plot <- renderPlot({
    
    par(mar = c(5.1, 4.1, 0, 1))
    
    x <- data()[, c(input$xcol, input$ycol)]
    plot(x,col=input$color,pch = input$type,type = input$line,cex=input$size)
  })
  
  ## Table Data Code
  output$table <- renderTable({
    
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    # datasetInput()
    read.csv(inFile$datapath, header = input$header)
    
  })
  
  ##File Summary Code
  output$summary <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    yowsa<- read.csv(inFile$datapath, header = input$header)
    summary(yowsa)
  })
  
  ## Observation Code
  output$observations <- renderTable({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    tablefile<- read.csv(inFile$datapath, header = input$header)
    head(tablefile, n = input$obs)
    
  })
  
  ##Download Button Code for downloading csv file
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$file, sep='') },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
  ##File structure code
  output$value <- renderPrint({
    # par(height = "400px",width= "100%")
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    print(str(input$file))
  })
  
  # # # Generate a summary of the data
  output$corr <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    corrfile<- read.csv(inFile$datapath, header = input$header)
    corrgram(corrfile,panel = panel.pie)
  })
  
  
  ##Simple Linear Regression Code
  output$summarylm <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    linearmodeldata<- read.csv(inFile$datapath, header = input$header)
    x<- data()[,input$xcol]
    y<-data()[,input$ycol]
    lmone<-lm(y~x,data = linearmodeldata)
    print("X =")
    print(input$xcol)
    print("Y =")
    print(input$ycol)
    summary(lmone)
    
  })
  
  ##Correlation Code
  output$correlation <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    correlation<- data()[,c(input$xcol,input$ycol)]
    cor(correlation)
  })
  
  ##Histogram Code 
  output$hist <- renderPlot({
    
    
    x<-as.numeric(data()[,input$xcol])
    hist(x, col=input$color,breaks = input$bins,xlab = input$xcol,main = input$xcol,border = input$bordercolor,
         freq = input$frequency)
    
    
  })
  
  ###### Random Forest Attempt #####
  
  
  # output$forest<- renderPlot({
  #   
  #   inFile <- input$file
  #   # df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
  #   # 
  #   # y<-data()[,input$ycol]
  #   # tree.dodgers1=tree(y~.,data = df,method = "regression")
  #   # 
  #   # plot(tree.dodgers1)
  #   # text(tree.dodgers1,pretty = 0)
  #   # summary(tree.dodgers1)
  #   # 
  #   # # # Load the party package. It will automatically load other required packages.
  #   # # library(party)
  #   # # library(randomForest)
  #   # # 
  #   # # # Create the forest.
  #   # # output.forest <- randomForest(y ~.,data = df)
  #   # # 
  #   # # # # View the forest results.
  #   # # # print(output.forest) 
  #   # # # 
  #   # # # # Importance of each predictor.
  #   # # # print(importance(fit,type = 2))
  #   # # plot(output.forest)
  #   # # text(output.forest,pretty = 0)
  #   
  #   par(mfrow=c(1,1))
  #   library(tree)
  #   inFile <- input$file
  #   datatree <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
  #   
  #   datatree<-datatree[,]
  #   names(datatree)
  #   library(randomForest)
  #   attach(datatree)
  #   
  #   lstMSEs=numeric()
  #   set.seed(1)
  #   maxnumpreds=ncol(datatree)-1
  #   maxnumtrees=150
  #   
  #   for(numpreds in 1:maxnumpreds){
  #     for(numtrees in 1:maxnumtrees){
  #       
  #       nrow(datatree)
  #       train=sample(1:nrow(datatree),nrow(datatree)/2)
  #       
  #       y<-data()[,input$ycol]
  #       model.bagged=randomForest(y~.,data = datatree,subset = train,mtry=numpreds,ntree=numtrees,importance=TRUE)
  #       
  #       
  #       
  #       pred.vals.bagged=predict(model.bagged,newdata = datatree[-train])
  #       yes<-data()[,input$ycol]
  #       testvals=yes[-train]
  #       mse=mean((pred.vals.bagged - testvals)^2)
  #       lstMSEs=rbind(lstMSEs,mse)
  #       print(paste("     Processed Trees:",numtrees))
  #     }
  #     print(paste("     Processed Predictors:",numpreds))
  #   }
  #   title(main = "Decision Tree Variance Explained
  #         Max Trees = 150",outer = TRUE,line=-1)
  #   matMSEs=matrix(lstMSEs,nrow = maxnumpreds,ncol=maxnumtrees)
  #   
  #   min(lstMSEs)
  #   min(matMSEs)
  #   lstMSEs
  #   
  #   loc=which(matMSEs==min(matMSEs),arr.ind=TRUE)
  #   print(paste("The optimal configuration is",loc[1],"predictors and",loc[2], "trees"))
  #   length(lstMSEs)
  #   print(paste("        Processed Trees:", numtrees))
  #   print(paste("        Processed Predictors:",numpreds))
  #   matMSEs[loc[1],loc[2]]
  #   
  #   
  #   
  #   which(matMSEs==min(matMSEs),arr.ind = TRUE)
  #   importance(model.bagged)
  #   y<-data()[,input$ycol]
  #   tree.data1=tree(y~.,data = datatree)
  #   plot(model.bagged)
  #   plot(tree.data1)
  #   text(tree.data1,pretty = 0)
  #   # varImpPlot(model.bagged)
  #   # model.bagged
  #   # min(lstMSEs)
  #   # summary(tree.data1)
  # })
  
}

# Run the application 
