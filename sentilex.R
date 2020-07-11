# Análise de Sentimentos em R

setwd("/Users/fernando/Google Drive/DSA/BigDataRAzure/Projetos/Analise_de_Sentimentos")
getwd()

library(stringr)
library(dplyr)
library(tidyr)

#Lendo o arquivo SentiLex-PT02.

sentilex <- read.delim("./SentiLex-PT02/SentiLex-lem-PT02.txt", header = FALSE, sep = ";")
View(sentilex)

#Substituindo células vaziar por NA
sentilex$V4[sentilex$V4 == ""] <- NA

#Excluindo células com valores NA
sentilex <- na.omit(sentilex)

sentilex <- sentilex %>%
  separate(V1, c("Word", "trash"), ".PoS")

sentilex <- sentilex %>%
  separate(V3, c("trash1","Feeling"), "POL:N0=")

sentilex <- sentilex[, c("Word","Feeling")]

write.csv(sentilex, file = 'sentilex.csv')
