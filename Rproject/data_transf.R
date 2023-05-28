library(readr)
library(dplyr)
library(ggplot2)

bank <- read.csv("C:../../MTAD_Francisco_Ferreira/datasets/BankChurners.csv")

unique(bank$Income_Category)
#Eliminar as dual últimas colunas 
bank$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1 = NULL
bank$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2 = NULL
bank$CLIENTNUM = NULL

colnames(bank)
nrow(bank)
#Número de linhas EC
df_EC <- bank %>% 
  filter(bank$Attrition_Flag == "Existing Customer")
nrow(df_EC)
#Número de linhas AC
df_AC <- bank %>% 
  filter(bank$Attrition_Flag == "Attrited Customer")
nrow(df_AC)


summary(bank)

#variaveis -< as.factor
bank$Attrition_Flag <- as.factor(bank$Attrition_Flag)
bank$Gender <- as.factor(bank$Gender)
bank$Education_Level <- as.factor(bank$Education_Level)
bank$Income_Category <- as.factor(bank$Income_Category)
bank$Card_Category <- as.factor(bank$Card_Category)

# 
levels(bank$Attrition_Flag)[levels(bank$Attrition_Flag) == "Attrited Customer"] <- "AC"
levels(bank$Attrition_Flag)[levels(bank$Attrition_Flag) == "Existing Customer"] <- "EC"
levels(bank$Attrition_Flag)

# Valores de $Gender para numerico
# Men = 0, Female = 1 
levels(bank$Gender)[levels(bank$Gender) == "M"] <- "0"
levels(bank$Gender)[levels(bank$Gender) == "F"] <- "1"


#countar valores da coluna - table(bank$Income_Category[])
#ver niveis da coluna - levels(bank$Income_Category)
#ver os resultados na consola - bank[bank$Income_Category %in% ('Less than $40K'),]

# Valores de $Income_Category para numerico
levels(bank$Income_Category)[levels(bank$Income_Category) == "Unknown"] <- 0 #
levels(bank$Income_Category)[levels(bank$Income_Category) == "Less than $40K"] <- 1 #
levels(bank$Income_Category)[levels(bank$Income_Category) == "$40K - $60K"] <- 2#
levels(bank$Income_Category)[levels(bank$Income_Category) == "$60K - $80K"] <- 3 #
levels(bank$Income_Category)[levels(bank$Income_Category) == "$80K - $120K"] <- 4 #
levels(bank$Income_Category)[levels(bank$Income_Category) == "Less than $40K"] <- 5
levels(bank$Income_Category)[levels(bank$Income_Category) == "$120K +"] <- 6 #


#Card Category para num por ordem de prestigio
levels(bank$Card_Category)[levels(bank$Card_Category) == "Blue"] <- 0
levels(bank$Card_Category)[levels(bank$Card_Category) == "Silver"] <- 1
levels(bank$Card_Category)[levels(bank$Card_Category) == "Gold"] <- 2
levels(bank$Card_Category)[levels(bank$Card_Category) == "Platinum"] <- 3

#Education Level para num por ordem de escolaridade
#levels(bank$Education_Level)
levels(bank$Education_Level)[levels(bank$Education_Level) == "Unknown"  ] <-0
levels(bank$Education_Level)[levels(bank$Education_Level) == "Uneducated"  ] <-1
levels(bank$Education_Level)[levels(bank$Education_Level) == "High School"  ] <-2 
levels(bank$Education_Level)[levels(bank$Education_Level) == "College"  ] <-3 
levels(bank$Education_Level)[levels(bank$Education_Level) == "Graduate"  ] <-4
levels(bank$Education_Level)[levels(bank$Education_Level) == "Post-Graduate"  ] <-5 
levels(bank$Education_Level)[levels(bank$Education_Level) == "Doctorate"  ] <-6 

#ver nulos no dataset
sum(is.na(bank))

#banka analise grafica
#write.csv(bank,"C:../../MTAD_Francisco_Ferreira/datasets/\\bankag", row.names = FALSE)

