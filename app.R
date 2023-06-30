library(shiny)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

Trasplantes <- read.csv("Trasplantes.csv")

ui <- fluidPage(
  titlePanel("Trasplantes de Órganos y Tejidos (2007 - 2020)"),
  
    navbarPage(
    tabsetPanel(
      tabPanel("DATOS",DT::dataTableOutput("tabla")),
      tabPanel("RESUMEN",
               sidebarPanel(
                 selectizeInput("Entidades",label = h4("Seleccionar entidades"), 
                                choices = Trasplantes$ENTIDAD, multiple=TRUE)),
                 verbatimTextOutput("Resumen")),
      tabPanel("TRASPLANTES",
                 sidebarPanel(
                   selectizeInput("Entidades",label = h4("Seleccionar entidades"), 
                                  choices = Trasplantes$ENTIDAD, multiple=TRUE),
                   sliderInput( "ylms", label = h4("Años contemplados"), min = 2007, 
                                max = 2020, value = c(2007, 2020))),
               plotOutput("Total")),
      tabPanel("ÓRGANOS", 
               sidebarPanel(
                 selectizeInput("Entidades",label = h4("Seleccionar entidades"), 
                                choices = Trasplantes$ENTIDAD, multiple=TRUE),
                 sliderInput( "ylms", label = h4("Años contemplados"), min = 2007, 
                            max = 2020, value = c(2007, 2020))),
               plotOutput("Organos")),
      tabPanel("TIPO DE TRASPLANTE",
               sidebarPanel(
                 selectizeInput("Entidades",label = h4("Seleccionar entidades"), 
                                choices = Trasplantes$ENTIDAD, multiple=TRUE),
                 sliderInput( "ylms", label = h4("Años contemplados"), min = 2007, 
                              max = 2020, value = c(2007, 2020))),
               plotOutput("Tipo"))
    )
  )
)


server <- function(input, output) {
  
  TrasplantesSub <- reactive({ Trasplantes %>%
      filter(ENTIDAD %in% input$Entidades)
  })
 
    output$tabla <- DT::renderDataTable(DT::datatable({Trasplantes},
                options=list(lengthMenu=list(c(5,10,100),c("5","10","100"))
                ), filter = "top", selection = "multiple", style = "bootstrap" 
                ))
    
    
    output$Resumen <- renderPrint({summary(TrasplantesSub())})
    
    
  fecha <- as.Date(Trasplantes$FECHATRASPLANTE,format="%d/%m/%Y")
    
    
    
    output$Total<-renderPlot ({TrasplantesSub() %>%
      ggplot(Trasplantes, aes(fecha, fill = group)) +   
        geom_histogram(alpha = 0.5, position = "identity")
      
    })
      
}


shinyApp(ui = ui, server = server)