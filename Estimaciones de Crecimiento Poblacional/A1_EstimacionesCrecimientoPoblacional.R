# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósito de A1_EstimacionesCrecimientoPoblacional: 
# Calcular el crecimiento poblacional de infancia temprana y adultos mayores.

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

############### Seleccionar municipio y estado de interés ######################

#1. Reemplazar aquí por nombre del municipio y estado de interés (incluyendo acentos)
municipio <- "Manzanillo"
estado <- "Colima"

########################### Cargar bases ####################################

# 1. Cargar bases
conapo <- read.csv("input/CONAPO/base_municipios_final_datos_01.csv", fileEncoding = "ISO-8859-1")
# Nota: Corroborar que se esté utilizando la base correcta 
# (terminación datos_01 o datos_02 según el estado de interés)

# 2. Filtrarlas solo para municipios de interés
conapo_mun <- filter(conapo, MUN==municipio)

################ A. Calcular crecimiento poblacional ###########################

# 1. Grupo de 0-4 años
p04 <- conapo_mun %>%
  filter(conapo_mun$EDAD_QUIN=="pobm_00_04")

  #2015 (Intercensal)
  p04_15 <- p04 %>%
    filter(p04$AÑO==2015) 
    it_15 <- sum(p04_15$POB)

  #2020
  p04_20 <- p04 %>%
    filter(p04$AÑO==2020) 
    pct_cambio1_04 <- (sum(p04_20$POB)/sum(p04_15$POB))
    pct_cambio1_04
    it_20 <- it_15*(1+pct_cambio1_04/100)

  #2025
  p04_25 <- p04 %>%
    filter(p04$AÑO==2025) 
    pct_cambio2_04 <- (sum(p04_25$POB)/sum(p04_20$POB))
    pct_cambio2_04
    it_25 <- it_20*(1+pct_cambio2_04/100)
    
  #2030
  p04_30 <- p04 %>%
    filter(p04$AÑO==2030) 
    pct_cambio3_04 <- (sum(p04_30$POB)/sum(p04_25$POB))
    pct_cambio3_04
    it_30 <- it_25*(1+pct_cambio3_04/100)
    

# 2. Grupo de 65 o más
p65 <- conapo_mun %>%
    filter(conapo_mun$EDAD_QUIN=="pobm_65_mm")
    
    #2015 (Intercensal)
    p65_15 <- p65 %>%
      filter(p65$AÑO==2015) 
    am_15 <- sum(p65_15$POB)
    
    #2020
    p65_20 <- p65 %>%
      filter(p65$AÑO==2020) 
    pct_cambio1_65 <- (sum(p65_20$POB)/sum(p65_15$POB))
    am_20 <- am_15*(1+pct_cambio1_65/100)
    
    #2025
    p65_25 <- p65 %>%
      filter(p65$AÑO==2025) 
    pct_cambio2_65 <- (sum(p65_25$POB)/sum(p65_20$POB))
    am_25 <- am_20*(1+pct_cambio2_65/100)
    
    #2030
    p65_30 <- p65 %>%
      filter(p65$AÑO==2030) 
    pct_cambio3_65 <- (sum(p65_30$POB)/sum(p65_25$POB))
    am_30 <- am_25*(1+pct_cambio3_65/100)

# 3. Guardar los resultados en la carpeta de output (como archivo .xlsx de Excel):
  # Crear vectores
    grupo_et<- c("0-4 años","65 o más años")
    quince <- c(it_15,am_15)
    veinte <- c(it_20,am_20)
    veinticinco <- c(it_25,am_25)
    treinta <- c(it_30,am_30)
    
  # Crear data frame y renombrar
    est_crecimiento_pob <- data.frame(grupo_et,quince,veinte,veinticinco,treinta)
    est_crecimiento_pob <- rename(est_crecimiento_pob, "Grupo etario" = grupo_et,
                                  "2015" = quince, "2020" = veinte, "2025" = veinticinco,
                                  "2030" = treinta)

    write_xlsx(est_crecimiento_pob,"output/A1_EstimacionesCrecimientoPoblacional.xlsx")   

################ Resultados extras ###########################
# Data frame en tasas: 
  # Crear vectores faltantes
    t_veinte <- c(pct_cambio1_04,pct_cambio1_65)
    t_veinticinco <- c(pct_cambio2_04,pct_cambio2_65)
    t_treinta <- c(pct_cambio3_04,pct_cambio3_65)
    
  # Crear data frame y renombrar
    est_crecimiento_pob_tasa <- data.frame(grupo_et,quince,t_veinte,t_veinticinco,t_treinta)
    est_crecimiento_pob_tasa <- rename(est_crecimiento_pob_tasa,"Grupo etario" = grupo_et, "2015" = quince,
                                  "2020" = t_veinte, "2025" = t_veinticinco, "2030" = t_treinta)
    
    write_xlsx(est_crecimiento_pob_tasa,"output/A1_EstimacionesCrecimientoPoblacional_Tasas.xlsx")
