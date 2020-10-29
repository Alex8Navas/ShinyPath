# Análisis del marco de datos sobre los Estados de USA.
library(tidyverse)
library(maps)
library(mapproj)
library(ggplot2)
source("data/helpers.R")


states <- readRDS("data/counties.rds")
head(states)
colnames(states) <- c("name", "total.pop", "Human", "Elf", "Orc", "Wizard")
head(states)


var <- states$Human
color<- "steelblue"
legend.title <- "Porcentaje de Humanos"
min = 0
max = 100

percent_map(var, color, legend.title, min, max)
