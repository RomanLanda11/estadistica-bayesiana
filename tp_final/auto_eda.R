library(readr)
df <- read_csv("tp_final/cerebros.csv")

#install.packages("DataExplorer")
library(DataExplorer)
create_report(df)


#install.packages("SmartEDA")
library(SmartEDA)
ExpReport(df)


# Instalar y cargar la librería
install.packages("explore")
library(explore)
explore(df)
