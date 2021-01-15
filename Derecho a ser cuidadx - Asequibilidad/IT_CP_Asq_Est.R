# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de IT_CP_Asq_Est: 
# Calcular el indicador de asequibilidad de cuidados personales
# para infancia temprana desde el Estado.

# Fecha de actualización: 27 de diciembre 2020
# Preparado por Natalia Achicanoy, Damián Lugo, Diana Laura Ramírez,
# Rodrigo Salas, Natalia Torres.
# Contacto: Natalia Torres, correo: nataliato94@gmail.com

######################## Preparar área de trabajo ##############################

# 1. Remover notación científica
options(scipen = 999)

# 2. Carácteres usados en el idioma español 
Sys.setlocale("LC_ALL", "Spanish") #PC
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac

# 3. Eliminar variables precargadas 
rm(list = ls())

# 4. Cargar paquetes a utilizar
library(pacman)
p_load(tidyverse, readxl, dplyr, tidyr, janitor, writexl, foreign)

# 5. Establecer directorio de trabajo
setwd("C:/R/cuidados/")

########################### Cargar bases ####################################

# 1. Cargar bases
alojamientos_caas <- read_excel("input/CAAS/Alojamientos de asistencia social.xls")
usuarios_caas <- read_excel("input/CAAS/Poblacion usuaria residente.xls")

# 2. Limpiar nombres de bases de datos
alojamientos_caas <- clean_names(alojamientos_caas)
usuarios_caas <- clean_names(usuarios_caas)

# 3. Eliminar filas sin datos 
alojamientos_caas <- alojamientos_caas[-1, ]
usuarios_caas <- usuarios_caas[-1, ]

############### Seleccionar municipio y estado de interés ###############

# 1. Revisar este documento para obtener el código del municipio y estado de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en los siguientes objetos
entidad <- "06"
municipio <- "007"

# 3. Filtrar bases por municipio de interés
alojamientos_caas <- alojamientos_caas %>% 
  filter(entidad == entidad, mun == municipio)
usuarios_caas <- usuarios_caas %>% 
  filter(entidad == entidad, mun == municipio) 

##################### Extraer información para indicador #######################

# 1. Juntar bases de datos
caas <- alojamientos_caas %>%
  left_join(usuarios_caas, by = "id_aloja")

# 2. Obtener el total de menores cuidados en casas hogares públicas
menores_total <- caas %>%
  filter(clasealoja.x == 1, fig_juri == 5) %>% 
  summarise(total_it_cp_asq_est = sum(r_t_0_4, na.rm = TRUE))

# 3. Obtener el total de menores cuidados en casas hogares públicas que no pagan cuotas
menores_nocuotas <- caas %>% 
  filter(clasealoja.x == 1, fig_juri == 5, 
                     cuotas == 2, gobierno == 1) %>% 
  summarise(total_infnocuotas = sum(r_t_0_4, na.rm = TRUE))

# 4. Resultado del indicador
porcentaje <- menores_nocuotas/menores_total*100
IT_CP_Asq_Est <- ifelse(menores_total != 0, porcentaje, 0)
IT_CP_Asq_Est
# Si el denominador y numerador son 0, el indicador saldrá como NaN, lo cual 
# significa que, en el municipio de interés, no existen estos servicios

# 5. Guardar los resultados del indicador en la carpeta de output como archivo .xlsx de Excel
IT_CP_Asq_Est <- as.data.frame(IT_CP_Asq_Est)
write_xlsx(IT_CP_Asq_Est, "output/IT_CP_Asq_Est.xlsx")
