#-----------------------------------------------------------------------------#
#                                                                             #
#                      Problem Set 1: Predicting Income                       #
#                                                                             #
#-----------------------------------------------------------------------------#

#   Autores: -                                                   
#            -   
#            - 
#            - 
#
#  Fecha: 0#/09/2023 
#
#  Objetivo: Este código cargar los datos, para proceder a limpiarlos y 
#  realizar las gráficas descriptivas de la base final.
#-----------------------------------------------------------------------------#

# - Limpiar el environment y el panel

rm(list = ls())
cat("\014")

# - Librerias

library(pacman)
p_load(rvest, tidyverse, ggplot2)

# - 1. Importar Datos

GEIH <-  data.frame()

for (j in 1:10) {
  url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
  pagina <- paste0(url,j,".html")
  Data_GitHub <- read_html(pagina)
  temp <-  Data_GitHub %>%
    html_table()
  GEIH <- bind_rows(GEIH,temp)
}

# - 2. Limpieza de la base de datos

