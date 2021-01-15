# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

#Propósitos de DC_Pert: 
# Calcular componentes y construir índice que sirve como indicador de 
# pertinencia del derecho a cuidar.

# Fecha de actualización: 27 de diciembre 2020
# Preparado por Natalia Achicanoy, Damián Lugo, Diana Laura Ramírez,
# Rodrigo Salas, Natalia Torres.
# Contacto: Natalia Torres, correo: nataliato94@gmail.com

######################## Preparar área de trabajo ##############################

# 1. Remover notación científica
options(scipen = 999)

# 2. Caracteres usados en el idioma español 
Sys.setlocale("LC_ALL", "Spanish")# Establecer notación para español (Windows)
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Establecer notación para español (Mac)

# 3. Eliminar variables precargadas 
rm(list = ls())

# 4. Cargar paquetes a utilizar
library(pacman)
p_load(tidyverse, readxl, dplyr, tidyr, janitor, writexl, foreign)

# 5. Establecer directorio de trabajo
setwd("C:/R/cuidados/")

################### Seleccionar estado de interés ##############################

# 1. Revisar este documento para obtener el código del estado de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en el siguiente objeto
estado <- "06"

############################# Cargar bases #####################################

# 1. Cargar base ENDIREH
base <- read.dta("input/ENDIREH/BD_MUJERES_ENDIREH2016_SitioINEGI.dta")

# 2. Seleccionar solo variables para construcción del índice
base <- base %>%
  select(id_muj, cve_ent, fac_muj,p7_8_4, p7_8_5, p7_8_6,
         p12_1_1_1, p12_1_1_3, p12_1_1_15, p12_1_1_16, p12_1_1_22,
         p14_1ab_1,p14_1ab_3,
         p14_1c_1, p14_1c_2,p14_1c_6,
         p15_1_1,p15_1_4,p15_1_7)

# 3. Filtrar para base_est de interés
base_est <- base %>%
  filter(cve_ent == estado)

# 4. Generar una nueva base 
base_i <- base_est["id_muj"] 

###################### Preparar variables para indicador #######################

# Asignar valores dicotómicos a las respuestas de las encuestadas: 1 y 0
# Será == 1 cuando la respuesta esté relacionada con un entorno en donde NO
# prevalezcan las creencias tradicionales de género

### 1. Recursos a su disposición / Discriminación laboral

# p7_8_4, p7_8_5 y p7_8_6 son 1 si respondió que NO tuvo problemas en su trabajo por cuestiones de maternidad 
# (el código para "No" en la ENDIREH es 2)
# 0 si respondió que SÍ tuvo problemas:
base_i$P1 <- base_est$p7_8_4 == 2
base_i$P2 <- base_est$p7_8_5 == 2
base_i$P3 <- base_est$p7_8_6 == 2

### 2. Relaciones de pareja (enojo hombre a mujer)

# p12_1_1_1, p12_1_1_3, p12_1_1_15, p12_1_1_16 y p12_1_1_22 son 1 si 
# respondió que su (ex)pareja NO se enoja con ella
# (el código para "No" en la ENDIREH es "2")
# 0 si respondió que SÍ se enoja con ella:
base_i$P4 <- base_est$p12_1_1_1 == "2"
base_i$P5 <- base_est$p12_1_1_3 == "2"
base_i$P6 <- base_est$p12_1_1_15 == "2"
base_i$P7 <- base_est$p12_1_1_16 == "2"
base_i$P8 <- base_est$p12_1_1_22 == "2"

### 3. Toma de decisiones en el hogar

# A p14_1ab_1, p14_1c_1, p14_1c_2 se les asignó una ponderación dependiendo del 
# grado de influencia que tiene la encuestada en la toma de decisión. 
# Entre más cercano a 1, ella tiene mayor libertad. Entre más cercano a 0, tiene menos libertad. 

base_i$P9_a <- case_when(
  base_est$p14_1ab_1 == 1 ~ 1, 
  base_est$p14_1ab_1 == 2 ~ 0,
  base_est$p14_1ab_1 == 3 ~ 0.25, 
  base_est$p14_1ab_1 == 4 ~ 0.75,
  base_est$p14_1ab_1 == 5 ~ 0.50,
  base_est$p14_1ab_1 == 6 ~ 0
)

base_i$P9_b <- case_when(
  base_est$p14_1c_1 == 1 ~ 1,
  base_est$p14_1c_1 == 2 ~ 0,
  base_est$p14_1c_1 == 3 ~ 0, 
  base_est$p14_1c_1 == 4 ~ 0, 
  base_est$p14_1c_1 == 5 ~ 0,
  base_est$p14_1c_1 == 6 ~ 0,
  base_est$p14_1c_1 == 7 ~ 0.50, 
  base_est$p14_1c_1 == 8 ~ 0
)

base_i$P9_c <- case_when(
  base_est$p14_1c_2 == 1 ~ 1, 
  base_est$p14_1c_2 == 2 ~ 0,
  base_est$p14_1c_2 == 3 ~ 0,
  base_est$p14_1c_2 == 4 ~ 0,
  base_est$p14_1c_2 == 5 ~ 0,
  base_est$p14_1c_2 == 6 ~ 0,
  base_est$p14_1c_2 == 7 ~ 0.50,
  base_est$p14_1c_2 == 8 ~ 0
)

# (NOTA: Por las características de las preguntas P9_b (p14_1c_1) y P9_c (p14_1c_2)
# es necesario promediar su ponderación):

base_i$P9_bc <- case_when(
  !is.na(base_i$P9_b) & !is.na(base_i$P9_c) ~ (base_i$P9_b + base_i$P9_c)/2,
  !is.na(base_i$P9_b) & is.na(base_i$P9_c) ~ base_i$P9_b,
  is.na(base_i$P9_b) & !is.na(base_i$P9_c) ~ base_i$P9_c
)

# Se hizo la misma ponderación para p14_1ab_3 y p14_1c_6.
# Entre más cercano a 1, ella tiene mayor libertad. 
# Entre más cercano a 0, tiene menos libertad.
base_i$P10 <- case_when(
  base_est$p14_1ab_3 == "1" ~ 1,
  base_est$p14_1ab_3 == "2" ~ 0,
  base_est$p14_1ab_3 == "3" ~ 0.25,
  base_est$p14_1ab_3 == "4" ~ 0.75,
  base_est$p14_1ab_3 == "5" ~ 0.50,
  base_est$p14_1ab_3 == "6" ~ 0,
  base_est$p14_1c_6 == "1" ~ 1,
  base_est$p14_1c_6 == "2" ~ 0,
  base_est$p14_1c_6 == "3" ~ 0,
  base_est$p14_1c_6 == "4" ~ 0,
  base_est$p14_1c_6 == "5" ~ 0,
  base_est$p14_1c_6 == "6" ~ 0,
  base_est$p14_1c_6 == "7" ~ 0.50,
  base_est$p14_1c_6 == "8" ~ 0,
)

### 4. Creencia en roles de género

# p15_1_1 y p15_1_7 son 1 si la entrevistada opina que NO.
# 0 si opina que SÍ:
base_i$P11 <- base_est$p15_1_1 == 2
base_i$P12 <- base_est$p15_1_7 == 2

# p15_1_4 es 1 si la entrevistada opina que SÍ...
# 0 si opina que NO: 
base_i$P13 <- base_est$p15_1_4 == 1

### 5. Limpieza adicional de variables

# Convertir las respuestas "3" o "7" No aplica, "9" o "99" No especificado, etc. en NAs:
# (Se convierten en NAs para que no cuenten ni como 1 ni como 0).

base_i$P1[base_est$p7_8_4 == 3 | base_est$p7_8_6 == 9] <- NA
base_i$P2[base_est$p7_8_5 == 3 | base_est$p7_8_6 == 9] <- NA
base_i$P3[base_est$p7_8_6 == 3 | base_est$p7_8_6 == 9] <- NA

base_i$P4[base_est$p12_1_1_1 == "9"] <- NA
base_i$P5[base_est$p12_1_1_3 == "9"] <- NA
base_i$P6[base_est$p12_1_1_15 == "9"] <- NA
base_i$P7[base_est$p12_1_1_16 == "9"] <- NA
base_i$P8[base_est$p12_1_1_22 == "9"] <- NA

base_i$P9_a[base_est$p14_1ab_1 == "7" | base_est$p14_1ab_1 == "9"] <- NA 
base_i$P9_b[base_est$p14_1c_1 == "7" | base_est$p14_1c_1 == "9"] <- NA
base_i$P9_c[base_est$p14_1c_2 == "7" | base_est$p14_1c_2 == "9"] <- NA
base_i$P10[base_est$p14_1ab_3 == "7" | base_est$p14_1ab_3 == "9" | base_est$p14_1c_6 == "7" | base_est$p14_1c_6 == "9"] <- NA

base_i$P11[base_est$p15_1_1 == "9"] <- NA
base_i$P12[base_est$p15_1_7 == "9"] <- NA
base_i$P13[base_est$p15_1_4 == "9"] <- NA

################################ Crear índice ##################################

# Para crear el índice (por cada observación/renglón calcular el promedio):
indice <- apply(base_i[c("P1","P2","P3","P4","P5",
                         "P6","P7","P8","P9_a","P9_bc","P10", "P11",
                         "P12","P13")]
                , FUN = function(x) mean(x, na.rm = TRUE),
                MARGIN = 1)

# Se genera otra columna que incluya el factor de expansión:
base_i$factor <- base_est$fac_muj

# Y se genera una nueva columna para el índice: 
base_i$indice <- indice

# El resultado del índice es:
sum(base_i$indice*base_i$factor)/sum(base_i$factor)

# Para guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel):
Indice_PertinenciaDerechoCuidar <- sum(base_i$indice*base_i$factor)/sum(base_i$factor)
Indice_PertinenciaDerechoCuidar <- as.data.frame(Indice_PertinenciaDerechoCuidar)
write_xlsx(Indice_PertinenciaDerechoCuidar,"output/DC_Pert.xlsx")

############################# Resultados extras ################################

# Para pasar la base de datos del base_est a formato csv:
write.csv(base_i, "output/DC_Pert_DatosAdicionales.csv")

# Análisis por pregunta:
indice_preguntas <- apply(base_i[c("P1","P2","P3","P4","P5",
                                   "P6","P7","P8","P9_a","P9_bc","P10",
                                   "P11","P12","P13")],
                          FUN = function(x) weighted.mean(x, base_i$factor, 
                                                          na.rm = TRUE),MARGIN = 2)

# Para ver gráficamente el análisis por pregunta:
barplot(indice_preguntas, ylim = c(0,1))

# Para conocer el número de personas que contestaron P9 y P10: 
universo <- function(p,n){
  sum(base_est[p] == n, na.rm = TRUE)
}

sum(sapply(c(1:9,99,""), function(n) universo(p = "p14_1c_6", n)))

# Creación de dos índices, uno para medir el contexto y otro para medir creencias:
# Para contexto se tomarán de P1 a P10:
base_i$contexto <- apply(base_i[c("P1","P2","P3","P4","P5",
                                  "P6","P7","P8","P9_a", "P9_bc","P10")]
                         , FUN = function(x) mean(x, na.rm = TRUE),
                         MARGIN = 1)

weighted.mean(base_i$contexto,base_i$factor, na.rm = TRUE)

# Para creencias se tomarán P11, P12 y P13:
base_i$creencias <- apply(base_i[c("P11","P12","P13")]
                          , FUN = function(x) mean(x, na.rm = TRUE),
                          MARGIN = 1)

weighted.mean(base_i$creencias,base_i$factor, na.rm = TRUE)

