#-----------------------------------------------------------------------------#
#                                                                             #
#                      Problem Set 1: Predicting Income                       #
#                                                                             #
#-----------------------------------------------------------------------------#
#   Autores: - Tania Reina 202015300                                                    
#            -   
#            - 
#            - 
#
#  Fecha: 18/09/2023 
#
#  Objetivo: Este código busca cargar los datos, para así proceder a limpiarlos
#  y realizar las gráficas descriptivas de la base final.
#-----------------------------------------------------------------------------#

# Limpiar el environment y el panel

rm(list = ls())
cat("\014")

# Librerias

library(pacman)
p_load(rvest, tidyverse, writexl, ggplot2, openxlsx)

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

# 2. Limpieza de la base de datos
# Edad -Sólo mayores de 18 años-

GEIH <- rename(GEIH, c("edad" = "age"))
GEIH <- GEIH[GEIH$edad >= 18, ]

# Años de educación

GEIH$edad_2 <- GEIH$edad^2

# Ingresos:

  #Total

GEIH <- rename(GEIH, c("ingreso_total" = "ingtot"))

  #Mensual

GEIH <- rename(GEIH, c("salario_mensual" = "y_ingLab_m"))

# Género

GEIH$mujer <- ifelse(GEIH$sex == 0, 1, 0)
GEIH$mujer[GEIH$sex == 1] <- 0

# Estudiante

GEIH$estudiante <- ifelse(GEIH$p6240 == 3, 1, 0)
GEIH$estudiante[GEIH$p6240 != 3] <- 0
GEIH$estudiante[GEIH$p6240 == "."] <- NA

# Busca trabajo

GEIH$busca_trabajo <- ifelse(GEIH$p6240 == 2, 1, 0)
GEIH$busca_trabajo[GEIH$p6240 != 2] <- 0
GEIH$busca_trabajo[GEIH$p6240 == "."] <- NA

# Amo(a) de casa

GEIH$amo_casa <- ifelse(GEIH$p6240 == 4, 1, 0)
GEIH$amo_casa[GEIH$p6240 != 4] <- 0
GEIH$amo_casa[GEIH$p6240 == "."] <- NA

# Hijos en el hogar

GEIH$hijos_hogar <- ifelse(GEIH$p6050 == 3, 1, 0)
GEIH$hijos_hogar[GEIH$p6050 != 3] <- 0
GEIH$hijos_hogar[GEIH$p6050 == "."] <- NA

# Interacciones
GEIH$mujer_hijos_hogar<- GEIH$mujer*GEIH$hijos_hogar
GEIH$mujer_amo_casa<- GEIH$mujer*GEIH$amo_casa
# Primaria

GEIH$primaria <- ifelse(GEIH$p6210 == 1, 1, 0)
GEIH$primaria[GEIH$p6210 == "."] <- NA

# Secundaria

GEIH$secundaria <- ifelse(GEIH$p6210 == 4, 1, 0)
GEIH$secundaria[GEIH$p6210 == "."] <- NA

# Media

GEIH$media <- ifelse(GEIH$p6210 == 5, 1, 0)
GEIH$media[GEIH$p6210 == "."] <- NA

# Superior

GEIH$superior <- ifelse(GEIH$p6210 == 6, 1, 0)
GEIH$superior[GEIH$p6210 == "."] <- NA

# Mantener Ocupados

GEIH <- GEIH[GEIH$oc == 1, ]


# Experiencia trabajo actual

GEIH <- rename(GEIH, c("exp_trab_actual" = "p6426"))
GEIH$exp_trab_actual_2 <- GEIH$exp_trab_actual^2

# Estrato

GEIH <- rename(GEIH, c("estrato" = "estrato1"))

# Cabecera

GEIH$cabecera <- ifelse(GEIH$clase == 1, 1, 0)

# Horas de trabajo a la semana

GEIH <- rename(GEIH, c("horas_trab_usual" = "hoursWorkUsual"))

# Logaritmo del Salario

GEIH$log_salario_m <- log(GEIH$salario_mensual)

# Ciudad

GEIH <- rename(GEIH, c("ciudad" = "dominio"))


# Mantener variables

GEIH <- subset(GEIH, select = c("directorio", "secuencia_p", "orden",
                                "mes", "edad", "edad_2", "mujer", 
                                "estudiante", "busca_trabajo", "amo_casa",
                                "hijos_hogar", "primaria", "secundaria",
                                "media", "superior", "salario_mensual",
                                "ingreso_total", "exp_trab_actual",
                                "estrato", "cabecera", "horas_trab_usual",
                                "oficio", "log_salario_m", "informal",
                                "ciudad"))

# Limpieza de valores faltantes 

# Se eliminan las observaciones que tienen valores faltantes en el
# salario nominal mensual

GEIH <- GEIH %>% filter(!is.na(salario_mensual))

# Estadísticas Descriptivas

Descriptivas <- GEIH[c("mujer","edad", "amo_casa", "hijos_hogar",
                            "estrato", "estudiante", "primaria", 
                            "secundaria", "media", "superior", 
                            "salario_mensual", "ingreso_total", 
                            "exp_trab_actual", "horas_trab_usual", 
                            "informal")]

estadisticas_todos <- data.frame(sapply(Descriptivas, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_todos, file = "C:/Users/Erick/Desktop/Problem_Set_1/views/Descriptivas.xlsx")



ggplot(GEIH, aes(x = salario_mensual)) +
  geom_histogram(bins = 280, fill = "darkorchid4", alpha = 0.5) +
  labs(x = "Salario nominal mensual", y = "Frecuencia") +
  theme_minimal() +
  scale_x_continuous(
    labels = function(x) paste0("$", format(x, big.mark = ".", scientific = FALSE)),
    limits = c(0, 13000000),
    expand = c(0,0),
    breaks = seq(0, 13000000, 1000000)
  ) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = "."),
    limits = c(0, 1000),
    expand = c(0,0),
    breaks = seq(0, 1000, 100)
  ) +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid = element_blank()
    )
ggsave("C:/Users/Erick/Desktop/Problem_Set_1/views/grafico1.png", width = 4.5, height = 2.5, dpi = 600)

