# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósito de IT_CDI_Asq_Est:
# Calcular el indicador de asequibilidad de cuidados para el
# desarrollo integral de infancia temprana desde el Estado

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
menores5_pub <- menores5[(menores5$p23>=1 & menores5$p23<=5) | menores5$p23==7,]
menores5_pub <- menores5_pub %>%
  filter(ingmensual_hog!=0) %>% #No contar a los hogares que no reportan ingresos porque no se puede dividir entre 0
  filter(p25!=99999) #Tiene información sobre gastos
denominador <- sum(menores5_pub$factor, na.rm = TRUE) #Total de menores de 5 cuidados en el sector público con información de ingresos mensuales

# 2. Calcular numerador
menores5_pub_si <- menores5_pub %>%
  group_by(cd_a, ent, con, v_sel, n_hog, h_mud, ingmensual_hog) %>% #Agrupar por hogares
  summarise(gasto_total_cuid_pub = sum(p25)) %>% #Suma del gasto de cada hogar en cuidado público
  mutate(pct_gasto_cuidados = gasto_total_cuid_pub/ingmensual_hog) %>% #Porcentaje de ingresos del hogar que representa el gasto
  mutate(pct_menor18_sino = case_when(pct_gasto_cuidados <= 0.18 ~ 1,
                                      pct_gasto_cuidados > 0.18 ~ 0)) 

gasto_menor18 <- inner_join(menores5_pub, menores5_pub_si, by=c("cd_a", "ent", "con", "v_sel", "n_hog", "h_mud"))

gasto_menor18 <- gasto_menor18[gasto_menor18$pct_menor18_sino == 1,]
numerador <- sum(gasto_menor18$factor, na.rm = TRUE) #Total de menores de 5 cuyos hogares gastan 18 % o menos en cuidados infantiles públicos

# 3. Calcular indicador
IT_CDI_Asq_Est <- (numerador/denominador)*100
IT_CDI_Asq_Est

# 4. Guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel)
IT_CDI_Asq_Est <- as.data.frame(IT_CDI_Asq_Est)
write_xlsx(IT_CDI_Asq_Est, "output/IT_CDI_Asq_Est.xlsx")
