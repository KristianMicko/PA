#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Zakladny popis",
          h1("Toto su data predaja peciva v pekarni"),
          p("Tieto data som ziskal z portalu www.kaggle.com. Je tam uvedeny cas transakcie, polozky a datum")
    ),
    
    tabPanel("Tabulka", verticalLayout(
      
                            h1("Toto je tabulka zo sledovanej datovej vzorky"),
                            p("Su tam data povodne aj data odvodene"),
                            dataTableOutput("Tabulka")  
                            
                        )
             
             
             ),
    tabPanel("Grafy",verticalLayout(
                            selectInput("VyberGrafu", "Vyber typ grafu pre najpredavanejsie polozky", c("Kolacovy", "Barplot")),
                            plotOutput("TabulkaNajpredavanejsichPoloziek"),
                            h3("Plot predaja pocas tyzdna"),
                            h6("Priemerny najvynosnejsi cas predaja pocas tyzdna"),
                            plotOutput("Graf_1"),
                            plotOutput("Graf_2"),
                            numericInput("od", "rozsah od", 1, min = 1, max = 95),
                            numericInput("do", "rozsah do", 2, min = 1, max = 95)
                        )
             
             
             
             
             ),
    
    tabPanel("Modelovanie", verticalLayout(
          h1("Modelovanie"),
          p("Predikovany graf"),
          plotOutput("Model"),
          selectInput("Mesiace","Zvol vstupny mesiac na predikciu", c("Oktober 2016", "November 2016", "December 2016",
                                                               "Januar 2017", "Februar 2017", "Marec 2017", "April 2017")),
          selectInput("Mesiace1","Zvol predikovany mesiac", c("Oktober 2016", "November 2016", "December 2016",
                                                               "Januar 2017", "Februar 2017", "Marec 2017", "April 2017")),
          p("Skutocne namerane hodnoty"),
          plotOutput("Skutocny"),
          p("Priemerna chyba"),
          textOutput("VystupChyby")
        )
    
    )
    
  )
  
  
))
