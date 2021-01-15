# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósito de IT_CDI_Asq_Merc:
# Calcular el indicador de asequibilidad de cuidados para el
# desarrollo integral de infancia temprana desde el mercado

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

############### Seleccionar estado de interés ##################################

# 1. Revisar este documento para obtener el código del estado de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en el siguiente objeto
estado <- 6

########################### Cargar bases #######################################

# 1. Cargar y preparar bases
enoe <- read.csv("input/ENOE/sdemt317.CSV")
eness <- read.csv("input/ENESS/eness2017_cuestionario_basico.csv") 

# 2. Arreglar nombres variables
names(eness) <- tolower(names(eness))

# 3. Agrupar datos de ENOE por hogar
enoe_hog <- enoe %>%
  filter(r_def == 00) %>% #Solo entrevistas realizadas
  group_by(cd_a, ent, con, v_sel, n_hog, h_mud) %>% #Identificación única del hogar
  summarise(ingmensual_hog = sum(ingocup)) %>% # Suma de los ingresos laborales mensuales de integrantes del hogar
  select(cd_a, ent, con, v_sel, n_hog, h_mud, ingmensual_hog) #Mantener solo esas variables

# 4. Unir con información de ENESS
base <- left_join(eness,enoe_hog, by=c("cd_a", "ent", "con", "v_sel", "n_hog", "h_mud"))

# 5. Filtrarla solo para estado de interés
base <- base %>%
  filter(ent == estado)

##################### Extraer información para indicador #######################

# 1. Calcular denominador
menores5 <- base[base$p1_eda>=0 & base$p1_eda<=5,]
menores5_priv <- menores5[menores5$p23==6 | menores5$p23==8 | (menores5$p23==12 & menores5$p25!=0),]
menores5_priv <- menores5_priv %>%
  filter(ingmensual_hog!=0) %>% #No contar a los hogares que no reportan ingresos porque no se puede dividir entre 0
  filter(p25!=99999) #Tiene información sobre gastos
denominador <- sum(menores5_priv$factor, na.rm = TRUE) #Total de menores de 5 cuidados en el sector privado con información de ingresos mensuales

# 2. Calcular numerador
menores5_priv_si <- menores5_priv %>%
  group_by(cd_a, ent, con, v_sel, n_hog, h_mud, ingmensual_hog) %>% #Agrupar por hogares
  summarise(gasto_total_cuid_priv = sum(p25)) %>% #Suma del gasto de cada hogar en cuidado privado
  mutate(pct_gasto_cuidados = gasto_total_cuid_priv/ingmensual_hog) %>% #Porcentaje de ingresos del hogar que representa el gasto
  mutate(pct_menor26_sino = case_when(pct_gasto_cuidados <= 0.26 ~ 1,
                                      pct_gasto_cuidados > 0.26 ~ 0)) 

gasto_menor26 <- inner_join(menores5_priv, menores5_priv_si, by=c("cd_a", "ent", "con", "v_sel", "n_hog", "h_mud"))

gasto_menor26 <- gasto_menor26[gasto_menor26$pct_menor26_sino == 1,]
numerador <- sum(gasto_menor26$factor, na.rm = TRUE) #Total de menores de 5 cuyos hogares gastan 26 % o menos en cuidados infantiles privados

# 3. Calcular indicador
IT_CDI_Asq_Merc <- (numerador/denominador)*100
IT_CDI_Asq_Merc

# 4. Guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel)
IT_CDI_Asq_Merc <- as_data_frame(IT_CDI_Asq_Merc)
write_xlsx(IT_CDI_Asq_Merc, "output/IT_CDI_Asq_Merc.xlsx")
