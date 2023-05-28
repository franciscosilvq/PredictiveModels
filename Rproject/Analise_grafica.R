library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)

#upload dataset previamente alterado para h2o
bankag <- read.csv("C:../../MTAD_Francisco_Ferreira/datasets//bankag")


#Mudanças ao dataset para analise grafica
#ver linhas dataset
nrow(bank)
#variaveis para factor
bankag$Attrition_Flag <- as.factor(bankag$Attrition_Flag)
bankag$Attrition_Flag <- as.integer(bankag$Attrition_Flag)
#quando AC e EC -> 1 e 2 
levels(bankag$Attrition_Flag)[levels(bankag$Attrition_Flag) == "AC"] <- "1"
levels(bankag$Attrition_Flag)[levels(bankag$Attrition_Flag) == "EC"] <- "2"
#passar a 0 (nao paga) e 1 (paga)
#variáveis para factor
bankag$Marital_Status <- as.factor(bankag$Marital_Status)
bankag$Marital_Status <- as.numeric(bankag$Marital_Status)

#passar todas as variaveis para numeric se forem int
bankag <- bankag %>%
  mutate_if(is.integer, as.numeric)

# escrever csv com as alterações para a analise grafica e posterior em MARKDOWN
#write.csv(bankag,"C:../../MTAD_Francisco_Ferreira/datasets/\\Bank_markdown", row.names = FALSE)

# Matriz de Correlecao
corr <- cor(bankag[,1:ncol(bankag)])
corrplot(corr, type = "upper", method="color", order="hclust", addCoef.col = "black", tl.col="black", tl.srt=45, tl.cex=0.7, number.cex=0.5, number.digits=2)

#Relação entre o limite de crédito com os meses de permanencia no banco bem como o genero

# 0<-Male 1<-Female
#bankag$Gender <- as.factor(bankag$Gender)
ggplot(bankag, aes(x=bankag$Months_on_book, y=bankag$Credit_Limit, colour=factor(bankag$Gender))) +
  geom_point()+ 
  labs(title = "Relação entre os meses de permanência e o crédito obtido bem como a classificação de género", 
       x="Meses de Permanencia no banco", 
       y="Limite de Credito",
       colour= "Genero",
       caption= "0 = Homem
       1 = Mulher")


predict <- read.csv("C:../../MTAD_Francisco_Ferreira/datasets/Prediction_drf_50_20_0.4_bc.csv")

# Cliente Status vs Income Category
counts <- table(bankag$Attrition_Flag,bankag$Income_Category)
barplot(counts,names.arg = c("Unknown","Less than $40K","$40K - $60K","$60K - $80K","$80K - $120K","$120K +"),cex.names = 0.7 ,
        main="Situação de cliente face ao Rendimento",
        xlab="Classe de Rendimento",
        ylim = c(0,3000),
        col=c("red","blue"),
        legend =rownames(counts),
        beside=TRUE)

View(counts)

