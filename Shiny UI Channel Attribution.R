library(shiny)
ui<-navbarPage("Channel Attribution with Markov Model ",
               tabPanel("Data Import",
                        sidebarLayout(sidebarPanel(fileInput("file","Upload your CSV",multiple = FALSE),
                                                   tags$hr()),
                                      mainPanel(h4("Note"),
                                                verbatimTextOutput("Note"),
                                                tags$head(tags$style("#Note{color:Grey; font-size:16px; font-style:bold;max-height: 500px; background: White;}")),
                                                h4("Format"),verbatimTextOutput("Format")))),
               tabPanel("Report",sidebarLayout(sidebarPanel(uiOutput("model_select"),
                                                            downloadButton("Download","Download")),
                                               mainPanel(verbatimTextOutput("Report")))))
library(ChannelAttribution)
library(dplyr)
library(tidyr)
server<-function(input,output) {data <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()} 
  read.csv(file=file1$datapath)
})
output$table<-renderTable({
  if(is.null(data())){return ()}
  data()
})
output$Note <- renderText({
  "You are going to apply 'Markov model' here
  Please Upload your file in following format
  
  'Markov Model'is used identify the important channels for brands to focus on and the ones 
  that can be ignored or discarded.
  
  1) Transition Probability matrix: Model calculates the probability of transition from one 
  particular state in the user journey to a different state in the user journey. 
  For example, if a user is starting the journey from Facebook and then using Organic Search 
  lands on the lead page or final conversion page of a website, then Markov model calculates 
  what are the chances or probability that a specific user is going to take the same path in 
  future based on past data. Using the transition probability matrix, brands can estimate 
  the future cost or spend on each channel
  2) Removal effect: Based on the transition probability, Markov model calculates removal effect
  of each channel in the path which explains the %effect on leads or conversion if we remove a 
  particular channel from the path  
  3) Channel Contribution: Unlike other attribution models, Markov model uses a probabilistic
  approach to give credit of each conversion to each channel. This is usually known as 
  weighted imputation  "
})
output$Format<-renderText({
  "  1: Make Sure you have downloaded CSV file
  2: First column should be Channel Grouping path
  3: Column should not contain any space
  4: Make sure your conversions column is not containing  ',' or any other special character
  5: Happy MARKOVING!"
})
output$model_select<-renderUI({
  selectInput("modelselect","Select Report",choices = c("Removal Effect"="Removal Effect","Transition Probability"="Transition Probability","Removal Effect with Revenue"="Removal Effect with Revenue","Channel Contribution"= "Channel Contribution"))
})
output$Report<-renderPrint({
  
  f<-data()
  mm<-markov_model(Data = f,var_path =colnames(f[1]),var_conv = colnames(f[2]),order = 2,sep = ">",out_more = TRUE,seed = 10)
  
  if(input$modelselect=="Transition Probability")
  {
    mm<-markov_model(Data = f,var_path =colnames(f[1]),var_conv = colnames(f[2]),order = 2,sep = ">",out_more = TRUE,seed = 10)
    mm$transition_matrix<-separate(data = mm$transition_matrix,col = 1,into = c("1","2"),sep = ",")
    mm$transition_matrix<-separate(data = mm$transition_matrix,col = 3,into = c("3","4"),sep = ",")
    mm$transition_matrix[is.na(mm$transition_matrix)]<-" "
    print(mm$transition_matrix)}
  if(input$modelselect=="Removal Effect"){
    mm<-markov_model(Data = f,var_path =colnames(f[1]),var_conv = colnames(f[2]),order = 2,sep = ">",out_more = TRUE,seed = 10)
    print(mm$removal_effects)}
  if(input$modelselect=="Removal Effect with Revenue"){
    mm<-markov_model(Data = f,var_path =colnames(f[1]),var_conv = colnames(f[2]),var_value = colnames(f[3]),order = 2,sep = ">",out_more = TRUE,seed = 10)
    print(mm$removal_effects)}
  if(input$modelselect == "Channel Contribution"){
    mm<-markov_model(Data = f,var_path =colnames(f[1]),var_conv = colnames(f[2]),var_value = colnames(f[3]),order = 2,sep = ">",out_more = TRUE,seed = 10)
    print(mm$result)
    
  }
})
output$Download <- downloadHandler(
  filename = function(){
    paste(input$modelselect,".csv",sep = "")
  },
  content =function(file) {
    if(input$modelselect=="Transition Probability"){
      f<-data()
      mm<-markov_model(Data = f,var_path =colnames(f[1]),var_conv = colnames(f[2]),order = 2,sep = ">",out_more = TRUE,seed = 10)
      mm$transition_matrix<-separate(data = mm$transition_matrix,col = 1,into = c("1","2"),sep = ",")
      mm$transition_matrix<-separate(data = mm$transition_matrix,col = 3,into = c("3","4"),sep = ",")
      mm$transition_matrix[is.na(mm$transition_matrix)]<-" "
      write.csv(mm$transition_matrix,file)
    }
    if (input$modelselect=="Removal Effect") {
      f<-data()
      mm<-markov_model(Data = f,var_path =colnames(f[1]),var_conv = colnames(f[2]),order = 2,sep = ">",out_more = TRUE,seed = 10)
      write.csv(mm$removal_effects,file)
    }
    if (input$modelselect == "Removal Effect with Revenue") {
      f<-data()
      mm<-markov_model(Data = f,var_path =colnames(f[1]),var_conv = colnames(f[2]),var_value = colnames(f[3]),order = 2,sep = ">",out_more = TRUE,seed = 10)
      write.csv(mm$removal_effects,file)
    }
    if(input$modelselect =="Channel Contribution"){
      f<-data()
      mm<-markov_model(Data = f,var_path =colnames(f[1]),var_conv = colnames(f[2]),var_value = colnames(f[3]),order = 2,sep = ">",out_more = TRUE,seed = 10)
      write.csv(mm$result,file)
    }
  })
}
shinyApp(ui=ui,server=server)
