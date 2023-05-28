library(readr)
library(dplyr)
library(ggplot2)
library(h2o)

#Load dataset com a coluna das predicts
prediction <- read_csv("C:../../MTAD_Francisco_Ferreira/datasets//Prediction_drf_50_20_0.4_bc.csv")

h2o.init(ip = "localhost", port = 54321, startH2O = T)

modelo <- h2o.loadModel("C:../../MTAD_Francisco_Ferreira/Modelos/drf_50_20_0,4_bc")

#listar variáveis do modelo 
str(modelo)

#Fazer, em R, plot das curvas de aprenizagem
x <- modelo@model$scoring_history$number_of_trees
y1 <- modelo@model$scoring_history$training_logloss
y2 <- modelo@model$scoring_history$validation_logloss
plot(x,y1, type = "l",col="blue",
     main = "Training vs Validation",
     ylim = c(0.1,3),
     xlab = "Trees",
     ylab = "LogLoss")
lines.default(x,y2,col="orange")
legend(x = "topright",          # Position
       legend = c("Training", "Validation"),  # Legend texts
       col = c("blue", "orange"),           # Line colors
       lwd = 2)       

#Confusion Matrix 
modelo@model$validation_metrics@metrics$cm$table



# test shiny 
# agg <- aggregate(EC ~ GrupoIdade + Months_on_book, data = prediction, FUN = mean)
# 
# dados <- agg[agg$GrupoIdade == input$Idade & agg$Months_on_book == input$Meses_Banco,]
# 
# 
# levels(bankag$Income_Category)
# unique(predictionshiny$Marital_Status)


