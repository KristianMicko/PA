#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$Tabulka = renderDataTable({
    #spojeneTabulky_1 = readRDS("Data_3.rda")
    spojeneTabulky_1 = readRDS("final_1.rda")
    print.data.frame(spojeneTabulky_1)
    })
  
  output$TabulkaNajpredavanejsichPoloziek = renderPlot({
    sledovane_1 = readRDS("Sledovane_1.rda")
    sledovane = readRDS("Sledovane.rda")
    j = paste(sledovane_1$A, sledovane_1$C, sep = " ")
    j = paste(j, "%", sep = "")
    if(input$VyberGrafu == "Kolacovy"){
      pie(sledovane_1$B, labels = j, col = rainbow(length(sledovane_1$B)), main = "Kolacovy graf najpred. poloziek")
    }
    if(input$VyberGrafu == "Barplot"){
      barplot(sledovane, col = rainbow(length(sledovane)), main = "Barplot najpredavanejsich poloziek")
    }
  })
  
  output$Graf_1 = renderPlot({
    spojeneTabulky_1 = readRDS("Data_3.rda")
    opilun = data.frame(table(spojeneTabulky_1$hms.Hodiny..hour,spojeneTabulky_1$Dni))
    library(lattice)
    najpredavanejsie_casy = table(spojeneTabulky_1$Item,spojeneTabulky_1$hms.Hodiny..hour)
    najpredavanejsie_casy = data.frame(najpredavanejsie_casy)
    xyplot(Freq~ Var1| Var2, data=opilun)
    
  })
  
  output$Graf_2 = renderPlot({
    library(lattice)
    spojeneTabulky_1 = readRDS("Data_3.rda")
    najpredavanejsie_casy = table(spojeneTabulky_1$Item,spojeneTabulky_1$hms.Hodiny..hour)
    najpredavanejsie_casy = data.frame(najpredavanejsie_casy)
    polozky =levels(najpredavanejsie_casy$Var1)
    G = najpredavanejsie_casy[which(najpredavanejsie_casy$Var1 == polozky[input$od : input$do]),1:3]
    G$Var1 = droplevels(G$Var1)
    xyplot(Freq~ Var2| Var1, data=G) 
  })
  #G = najpredavanejsie_casy[which(najpredavanejsie_casy$Freq>33),1:3]
  #G$Var1 = droplevels(G$Var1)
  #xyplot(Freq~ Var2| Var1, data=G)
  
  output$Graf_3 = renderPlot({
    library(arules)
    library(arulesViz)
    selektovaneTransakcie <- read.transactions("BreadBasket_DMS.csv",format="single",cols=c(3,4),sep=",")
    konfidencia = .5
    if(input$VyberHodnoty_1 == .5){
      konfidencia = .5
    }
    if(input$VyberHodnoty_1 == .6){
      konfidencia = .6
    }
    
    if(input$VyberHodnoty_1 == .7){
      konfidencia = .7
    }
    
    pravidla <- apriori(selektovaneTransakcie,parameter=list(support=0.0045,confidence=konfidencia))
    if(input$VyberGrafu2 == "Basic"){
      plot(pravidla, measure=c("support","lift"), shading="confidence")
    }
    if(input$VyberGrafu2 == "two-key plot"){
      plot(pravidla, method = "two-key plot")
    }
    if(input$VyberGrafu2 == "graph"){
      plot(pravidla, method="graph")
    }
    if(input$VyberGrafu2 == "grouped"){
      plot(pravidla, method="grouped")
    }
    if(input$VyberGrafu2 == "paracoord"){
      plot(pravidla, method="paracoord", control=list(reorder=TRUE))
    }
  })
  
})
