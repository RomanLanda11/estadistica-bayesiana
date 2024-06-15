library(readr)
df <- read_csv("data/cerebros.csv")
df$diag <- factor(df$diag, levels = c("HC", "MCI", "AD"))
levels(df$diag)

#install.packages("DataExplorer")
library(DataExplorer)
create_report(df)


#install.packages("SmartEDA")
library(SmartEDA)
ExpReport(df)


# Instalar y cargar la librería
#install.packages("explore")
library(explore)
explore(df)
