library(shiny)
library(shiny.router)
library(shinydashboard)
library(shinybusy)
library(shinythemes)
library(shinyWidgets)
library(readxl)
library(tidyverse)
library(DT)
library(highcharter)
library(lubridate, warn.conflicts = FALSE) 
library(shinyjs)
library(shinycssloaders) 
library(rintrojs)
library(officer)
library(flextable)
library(glue)
library(stringi)
library(plyr)


setwd("M:/Commun/Projet Diffusion/chiffres_cles/Applications/Repertoire_App")

base=read_excel("base_new_derniere_version.xlsx")

Repertoire<-"M:/Commun/Projet Diffusion/chiffres_cles/Applications/Repertoire_App/data/"

base$value<-as.numeric(base$value)

base$Indicateurs<-as.character(base$Indicateurs)

source("M:/Commun/Projet Diffusion/chiffres_cles/Applications/Repertoire_App/Fonctions.R",encoding="UTF-8")




load("data/taux_plainte_cambriolages.RData")








taux_plainte_cambriolages %>% filter(Indicateurs==sort(unique(taux_plainte_cambriolages$Indicateurs))[2])






































































