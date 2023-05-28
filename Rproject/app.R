library(shiny)
library(readr)
prediction <- read_csv("C:../../MTAD_Francisco_Ferreira/datasets//Prediction_drf_50_20_0.4_bc.csv")
 
# Define the user interface

ui <- fluidPage(
  titlePanel (h2("Sistema de Apoio a decisao")),
  sidebarLayout(
    sidebarPanel(style = "margin-top: 50px;",#mover side panel para baixo 
      # br(), #espacamento
      h4("Parametros de Analise", align = "center"),
      selectInput("Genero", "Genero:", c("0", "1")),
      helpText("0 - Homem 1 - Mulher"),
      selectInput("Cartao", "Categoria de Cartao:", c("0","1","2","3")),
      helpText("0 - Blue 1 - Silver"),
      helpText("2 - Gold 3 - Platinum"),
      selectInput("Estado_Civil","Estado Civil:", c("Married","Unknown","Divorced","Single")),
      submitButton("Aplicar parametros", icon("redo"),)
      ),
    
    mainPanel(
      br(),br(),
      h3("Probabilidade de Incumprimento", align = "Center"),
      br(),
      plotOutput("barplot")
      )
  )
  )


# Define the server logic
server <- function(input, output) {
  
  output$barplot <- renderPlot({
    
    agg <- aggregate(AC  ~  Gender + Card_Category + Marital_Status, data = prediction, FUN = mean)
    
    dados <- agg[agg$Gender == input$Genero & agg$Card_Category == input$Cartao & agg$Marital_Status == input$Estado_Civil,]
    
    if (nrow(dados) == 0) {
      shiny::showModal(modalDialog(
        title = "Error",
        "No data matches the selected inputs. Please try again with different inputs."
      ))
    } else {
      barplot(dados$AC,names.arg = "Attrited Customer",
              ylim = c(0,1),
              xlab = "Probabilidade tendo em conta Genero, Tipo de Cartao e Estado Civil",
              ylab = "Probabilidade",
              col = "red",
              width = 1,)
      text(dados$AC, labels = format(dados$AC, digits=2),pos=3, cex =2)
    }
    
  })
}  
# Run the app
shinyApp(ui = ui, server = server)


