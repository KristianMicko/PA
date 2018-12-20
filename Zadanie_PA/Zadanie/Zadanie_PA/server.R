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
   
  output$Model = renderPlot({
    spojeneTabulky_1 = readRDS("Data_3.rda")
    
    x = NULL
    y= NULL
    if(input$Mesiace == "Oktober 2016"){
      x= 10
    }
    if(input$Mesiace == "November 2016"){
      x= 11
    }
    if(input$Mesiace =="December 2016"){
      x=12
    }
    if(input$Mesiace =="Januar 2017"){
      x=1
    }
    if(input$Mesiace =="Februar 2017"){
      x=2
    }
    if(input$Mesiace =="Marec 2017"){
      x=3
    }  
    if(input$Mesiace =="April 2017"){
      x=4
    }     
    
    if(input$Mesiace1 == "Oktober 2016"){
      y= 10
    }
    if(input$Mesiace1 == "November 2016"){
      y= 11
    }
    if(input$Mesiace1 =="December 2016"){
      y=12
    }
    if(input$Mesiace1 =="Januar 2017"){
      y=1
    }
    if(input$Mesiace1 =="Februar 2017"){
      y=2
    }
    if(input$Mesiace1 =="Marec 2017"){
      y=3
    }  
    if(input$Mesiace1 =="April 2017"){
      y=4
    }
    
    Alpha = spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == x),"Celkovy_Index"]
    Beta = spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == x),"Transaction"]
    Alpha_pred=spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == y),"Celkovy_Index"]
    #Beta_pred=
    #Beta_skut=spojeneTabulky_1 =spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 12),"Transaction"]
    spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == y),"Celkovy_Index"]
    Napilon = data.frame(Alpha,Beta)
    model_1 = lm(Beta~Alpha,data=Napilon)
    model_1
    Napilon_2 = data.frame(Alpha = Alpha_pred) 
    predpoved = predict(model_1,newdata = Napilon_2)
    #View(predpoved)
    dlzka = length(Napilon$Alpha)
    dlzka_1 =length(Napilon_2$Alpha)
    style = c(rep(1,dlzka),rep(2,dlzka_1))
    
    Napilon_2 = data.frame(Alpha = Alpha_pred,Beta_pred=spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == y),"Transaction"],predpoved)
    plot(x=c(Napilon$Alpha,Napilon_2$Alpha),y= c(Napilon$Beta,predpoved),pch=style,col=style,main="Modelovanie predaja v 12.m 2016 roku na zaklade predaja 11.m 2016 roku", xlab="Polozky",ylab="Transakcie")
    output$Skutocny = renderPlot({
      plot(x=c(Napilon$Alpha,Napilon_2$Alpha),y= c(Napilon$Beta,Napilon_2$Beta_pred),col=style,main="Skutocne data ziskane v Nov. a Dec. v 2016",xlab="Polozky",ylab="Transakcie")
      legend(x="bottomright", legend = c(input$Mesiace,input$Mesiace1),pch = style, col = c("black","red")) 
    })
    #plot(x=c(Napilon$Alpha,Napilon_2$Alpha),y= c(Napilon$Beta,Napilon_2$Beta_pred),col=style)
    legend(x="bottomright", legend = c(input$Mesiace,input$Mesiace1),pch = style, col = c("black","red"))
    chyba = c(NULL)
    for (jndex in 1:length(predpoved)) {
      chyba[jndex] = predpoved[jndex]-spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 12),"Transaction"][jndex]
    }
    priemerna_chyba_modelu = mean(chyba)
    output$VystupChyby = renderText(priemerna_chyba_modelu)
    
    
  })
  
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
  
})
