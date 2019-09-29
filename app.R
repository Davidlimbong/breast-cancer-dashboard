library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr) 
library(caret) # ML
library(ranger)# random forest
library(DT) #data tabel
library(stringr)
library(e1071)

data <- read.csv("data.csv")
data$diagnosis<-sapply(as.character(data$diagnosis), switch, "M"="Malignant","B"="Benign")
data$diagnosis<-as.factor(data$diagnosis)

ui <- dashboardPage(skin = "red",
                    dashboardHeader(
                      title = "Breast Cancer"
                    ),
                    dashboardSidebar(
                      
                      sidebarMenu(
                        #nama yg di show, id, icon
                        menuItem("Summary",tabName = "1",icon= icon("star")),
                        menuItem("Plot",tabName = "2",icon= icon("pencil")),
                        menuItem("Machine Learning",tabName = "3",icon= icon("cog")),
                        menuItem("Data",tabName = "4",icon= icon("list-alt"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        # Panel Summary
                        tabItem(tabName = "1",
                                box(width = 14,
                                    h1("Summary of Data"),
                                    fluidRow( # <div class="row">
                                      
                                      # Dynamic valueBoxes
                                      valueBoxOutput("nrow"),
                                      valueBoxOutput("ncol"),
                                      valueBoxOutput("persen")
                                    ),
                                    selectInput("atribut","Select Atribut",choices =str_to_title(gsub("_"," ",names(data[,-c(1:2)]))) ),
                                    splitLayout(plotlyOutput("histogram"), plotOutput("boxplot"))
                                )
                                
                        ),
                        #Panel Plot
                        tabItem(tabName = "2",
                                box(width = 16,
                                    h1("Interactive Plot"),
                                    tabsetPanel(
                                      tabPanel("Scatter Plots", 
                                               h3("Scatter Plots"),
                                               flowLayout(
                                                 selectInput("plotx","X axis", choices = str_to_title(gsub("_"," ",names(data[,-c(1:2)])))),
                                                 selectInput("ploty","Y axis", choices = str_to_title(gsub("_"," ",names(data[,-c(1:2)]))))
                                               ),
                                               plotlyOutput("point")), 
                                      tabPanel("Heat Map", h3("Heat Map"),
                                               plotlyOutput("heatmap"))
                                      
                                    )
                                )
                                
                        ),
                        tabItem(tabName ="3",
                                selectInput("metode","pilih metode",choices = c("KNN", 
                                                                                "Random Forest",
                                                                                "Decision Tree")),
                                
                                
                                tabsetPanel(
                                  tabPanel("Data",
                                           box(width = 18,
                                               dataTableOutput("result")
                                               )
                                           ),
                                            
                                  tabPanel("Model Performance",
                                           box(width = 18,
                                               verbatimTextOutput("knn")
                                               )
                                           
                                           ),
                                  tabPanel("Description Model",
                                          box(width = 18,
                                              textOutput("des"),
                                              plotOutput("modelp"))  
                                           
                                  )
                                )
                                
                                ),
                        tabItem(tabName = "4",
                                h3("Breast Cancer Wisconsin (Diagnostic) Data Set"),
                                box(width = 16,
                                    dataTableOutput("hehe")
                                    )
                              )
                        
                        
                      )#tabItems
                    )#dashboardBody
                    
                    
)#dashboardpage
server <- function(input, output,session) { 
  
  #Page 1
  output$nrow <- renderValueBox({
    valueBox(
      paste0(nrow(data)),"Rows", icon = icon("align-justify"),
      color = "purple"
    )
  })
  
  output$ncol <- renderValueBox({
    valueBox(
      value= paste0(ncol(data)),
      subtitle = "columns",
      icon = icon("pause"),
      color = "green"
    )
  })
  
  output$persen <- renderValueBox({
    prop <- round(prop.table(table(data$diagnosis)),2)
    valueBox(
     value = paste0(prop[[2]]*100,"%"),
     subtitle = "Malignant Breast Cancer", 
     color = "maroon",
     icon = icon("remove")
    )
  })
  
  output$histogram<-renderPlotly({
    atribut<-gsub(" ","_",str_to_lower(input$atribut))
    #lx<-input$atribut
    plotd<-ggplot(data,
                  aes(x=data[,atribut],
                      fill=diagnosis,
                      text=sprintf(paste("Diagnosis: ",data$diagnosis))))+
      geom_histogram()+
      theme_bw()+
      scale_fill_discrete(name="",
                          breaks=c("b", "m"),
                          labels=c("Benign", "Malignant"))+
      theme(legend.position = "top")+
      labs(x=str_to_title(paste(gsub("_"," ",atribut))), title=paste("Histogram Plot of", str_to_title(paste(gsub("_"," ",atribut))) ))
    
    
    ggplotly(plotd,tooltip=c("text","count"))%>%
      layout(legend = list(
        orientation = "h"
      )
      )
  })
  
  output$boxplot<-renderPlotly({
    atribut<-gsub(" ","_",str_to_lower(input$atribut))
    ggplot(data=data, aes(y=data[,atribut], x=data$diagnosis))+
      geom_boxplot(aes(color=diagnosis), outlier.color = "red")+
      theme_bw()+
      theme(plot.title = element_text(size = 20) )+
      scale_colour_discrete(name = "Fancy Title")+
      labs(y=str_to_title(paste(gsub("_"," ",atribut))),
           x="Diagnosis",
           title= paste("Box Plot of", str_to_title(paste(gsub("_"," ",atribut))) ))
  })
  
  output$hehe<-renderDataTable({
    datatable(data,options = list(scrollX = TRUE))
    
  })
  #
  #Page2
  output$point<-renderPlotly({
    plotx<-gsub(" ","_",str_to_lower(input$plotx))
    ploty<-gsub(" ","_",str_to_lower(input$ploty))
    plotp<- ggplot(data,aes(x=data[,plotx],
                            y=data[,ploty], 
                            color=diagnosis, 
                            text=sprintf(paste(input$plotx,': %s\n',input$ploty,': %s'), data[,plotx], data[,ploty])))+
            geom_point()+
            theme_bw()+
      labs(x=input$plotx ,y=input$ploty)
    ggplotly(plotp,tooltip="text" )
  })
  output$heatmap<-renderPlotly({
    corr<-cor(data[,-c(1:2)])
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    plot_ly(x = colnames(data[,-c(1:2)]),
            y = colnames(data[,-c(1:2)]), 
            z = corr,
            type = "heatmap") %>% 
      layout(xaxis=ax, yaxis=ax)
    
    
  })
  set.seed(1234)
  dataIndex<-createDataPartition(data$diagnosis,p=0.8,list = FALSE)
  train_data <- data[dataIndex,-1]
  test_data <- data[-dataIndex,]
  
  output$knn<-renderPrint({
    load("model_knn.rda")
    
    if(input$metode=="KNN"){
      pred_knn<-predict(model_knn,test_data[,-1])
      perbandingan<-table(pred_knn,test_data$diagnosis)
      
      
      
    }else if(input$metode=="Random Forest"){
      load("model_rf.rda")
      pred_knn<-predict(model_rf,test_data[,-1])
      perbandingan<-table(pred_knn,test_data$diagnosis)
      
      
    }else{
      load("model_dt.rda")
      pred_knn<-predict(model_dt,test_data[,-1])
      perbandingan<-table(pred_knn,test_data$diagnosis)
      
    }
    Recall<-round((perbandingan[2,2]/sum(perbandingan[,2]))*100,2)
    Accuracy<-round((perbandingan[1,1]+perbandingan[2,2])/sum(perbandingan)*100,2)
    Precision<-round((perbandingan[2,2]/sum(perbandingan[2,]))*100,2)
    Specificity<-round((perbandingan[1,1]/sum(perbandingan[,1]))*100,2)
    
    cat('\n Model Performance  :\n\n','Accuracy \t:',Accuracy,'%\n','Recall \t:',Recall,'%\n','Precision \t:',Precision,'%\n','Specificity \t:',Specificity,"%\n")
    
  })
  
  
  output$result<-renderDataTable ({
    if(input$metode=="KNN"){
      load("model_knn.rda")
      pred_knn<-predict(model_knn,test_data[,-1])
      
    }else if (input$metode=="Random Forest"){
      load("model_rf.rda")
      pred_knn<-predict(model_rf,test_data[,-1])
    }else{
      load("model_dt.rda")
      pred_knn<-predict(model_dt,test_data[,-1])
      
    }
    dataBaru<-test_data[,c(1:2)]
    dataBaru$Predict<-pred_knn
    colnames(dataBaru)<-c("Id","Actual","Prediction")
    datatable(dataBaru)
  })
  
  output$des<-renderText ({
    if(input$metode=="KNN"){
      print("k-nearest neighbour classification for test set from training set.
      For each row of the test set, the k nearest (in Euclidean distance) training set
      vectors are found, and the classification is decided by majority vote, with ties 
      broken at random. If there are ties for the kth nearest vector, all candidates are 
    included in the vote.")
      
    }else if(input$metode=="Random Forest"){
      print("Random forests are a combination of tree predictors
such that each tree depends on the values of a random
vector sampled independently and with the same
distribution for all trees in the forest")
    }
    else {
      print("Decision trees and tree-based models are powerful, incredibly versatile 
            and are is quite likely the single most popular choice for machine learning
            tasks. Their output are also a powerful form of representing rules, which 
            has the benefit of being interpretable and adaptable to almost any scenario.")
    }
    
   
  })
  output$modelp<-renderPlot({
    if(input$metode=="KNN"){
      load("model_knn.rda")
      plot(model_knn)
    }else if (input$metode=="Random Forest"){
      load("model_rf.rda")
      plot(model_rf)
    }else {
      load("model_dt.rda")
      plot(model_dt)
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

