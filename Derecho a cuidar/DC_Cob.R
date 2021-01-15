# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósito de DC_Cob: 
# Calcular indicadores del atributo de cobertura del derecho a cuidar.

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
p_load(tidyverse, readxl, dplyr, tidyr, janitor, writexl, foreign, naniar)

# 5. Establecer directorio de trabajo
setwd("C:/R/cuidados/")

################### Seleccionar municipio de interés ###########################

# 1. Revisar este documento para obtener el código del municipio de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en el siguiente objeto
municipio <- 7

############################## Cargar bases ####################################

# 1. Cargar bases
intercensal <- read.csv("input/Intercensal/TR_PERSONA06.csv")

# 2. Filtrarla solo para municipio de interés
inter_per_mun <- intercensal %>%
  filter(MUN==municipio) 

# 3. Generar bases por género 
mujeres <- inter_per_mun %>%
  filter(SEXO==3)
hombres <- inter_per_mun %>%
  filter(SEXO==1)

################## Calcular indicadores cobertura Pt. 1 ########################

#1. Porcentaje de personas que dedican >=1 hr al trabajo doméstico
  #Mujeres
  mujeres$trabdom_sino <- (mujeres$ACTI_SIN_PAGO6>0 & mujeres$ACTI_SIN_PAGO6<999 | 
                           mujeres$ACTI_SIN_PAGO7>0 & mujeres$ACTI_SIN_PAGO7<999 |
                           mujeres$ACTI_SIN_PAGO8>0 & mujeres$ACTI_SIN_PAGO8<999)
  pct_trabdom_m <- (weighted.mean(mujeres$trabdom_sino, mujeres$FACTOR, na.rm = TRUE))*100

  #Hombres
  hombres$trabdom_sino <- (hombres$ACTI_SIN_PAGO6>0 & hombres$ACTI_SIN_PAGO6<999 | 
                           hombres$ACTI_SIN_PAGO7>0 & hombres$ACTI_SIN_PAGO7<999 |
                           hombres$ACTI_SIN_PAGO8>0 & hombres$ACTI_SIN_PAGO8<999)
  pct_trabdom_h <- (weighted.mean(hombres$trabdom_sino, hombres$FACTOR, na.rm = TRUE))*100

  #Diferencia
  DC_Cob_Ind_1.1 <- pct_trabdom_m - pct_trabdom_h
  DC_Cob_Ind_1.1

#2. Porcentaje de personas que dedican >=1 hr al trabajo de cuidados
  #Mujeres
  mujeres$cuidados_sino <- (mujeres$ACTI_SIN_PAGO1>0 & mujeres$ACTI_SIN_PAGO1<999 | 
                            mujeres$ACTI_SIN_PAGO2>0 & mujeres$ACTI_SIN_PAGO2<999 |
                            mujeres$ACTI_SIN_PAGO3>0 & mujeres$ACTI_SIN_PAGO3<999 | 
                            mujeres$ACTI_SIN_PAGO4>0 & mujeres$ACTI_SIN_PAGO4<999 |
                            mujeres$ACTI_SIN_PAGO5>0 & mujeres$ACTI_SIN_PAGO5<999)
  pct_cuidados_m <- (weighted.mean(mujeres$cuidados_sino, mujeres$FACTOR, na.rm = TRUE))*100

  #Hombres
  hombres$cuidados_sino <- (hombres$ACTI_SIN_PAGO1>0 & hombres$ACTI_SIN_PAGO1<999 | 
                            hombres$ACTI_SIN_PAGO2>0 & hombres$ACTI_SIN_PAGO2<999 |
                            hombres$ACTI_SIN_PAGO3>0 & hombres$ACTI_SIN_PAGO3<999 | 
                            hombres$ACTI_SIN_PAGO4>0 & hombres$ACTI_SIN_PAGO4<999 |
                            hombres$ACTI_SIN_PAGO5>0 & hombres$ACTI_SIN_PAGO5<999)
  pct_cuidados_h <- (weighted.mean(hombres$cuidados_sino, hombres$FACTOR, na.rm = TRUE))*100

  #Diferencia
  DC_Cob_Ind_1.2 <- pct_cuidados_m - pct_cuidados_h
  DC_Cob_Ind_1.2 
  
################## Calcular indicadores cobertura Pt. 2 ########################
  
#1. Reemplazar 999 (No especifica) y 0 con NA 
#Esto significa que solo contamos a quienes reportan hacer la actividad
inter_per_mun <- inter_per_mun %>%
    replace_with_na_at(.vars = c("ACTI_SIN_PAGO1", "ACTI_SIN_PAGO2",
                                 "ACTI_SIN_PAGO3", "ACTI_SIN_PAGO4",
                                 "ACTI_SIN_PAGO5", "ACTI_SIN_PAGO6",
                                 "ACTI_SIN_PAGO7", "ACTI_SIN_PAGO8"),
                     condition ~.x == 999) %>%
    replace_with_na_at(.vars = c("ACTI_SIN_PAGO1", "ACTI_SIN_PAGO2",
                                 "ACTI_SIN_PAGO3", "ACTI_SIN_PAGO4",
                                 "ACTI_SIN_PAGO5", "ACTI_SIN_PAGO6",
                                 "ACTI_SIN_PAGO7", "ACTI_SIN_PAGO8"),
                       condition ~.x == 0) 
    
#2. Volver a generar bases de género
mujeres <- inter_per_mun %>%
  filter(SEXO==3)
hombres <- inter_per_mun %>%
  filter(SEXO==1)

#3. Calcular promedio de horas dedicadas al trabajo doméstico
  #Mujeres
  hrs_trabdom_m <- weighted.mean(mujeres$ACTI_SIN_PAGO6, mujeres$FACTOR, na.rm = TRUE) +
                   weighted.mean(mujeres$ACTI_SIN_PAGO7, mujeres$FACTOR, na.rm = TRUE) +
                   weighted.mean(mujeres$ACTI_SIN_PAGO8, mujeres$FACTOR, na.rm = TRUE)

  #Hombres
  hrs_trabdom_h <- weighted.mean(hombres$ACTI_SIN_PAGO6, hombres$FACTOR, na.rm = TRUE) +
                   weighted.mean(hombres$ACTI_SIN_PAGO7, hombres$FACTOR, na.rm = TRUE) +
                   weighted.mean(hombres$ACTI_SIN_PAGO8, hombres$FACTOR, na.rm = TRUE)

  #Diferencia
  DC_Cob_Ind_2.1 <- hrs_trabdom_m - hrs_trabdom_h
  DC_Cob_Ind_2.1
  
# 4. Calcular promedio de horas dedicadas al trabajo de cuidados
  #Mujeres
  hrs_cuidados_m <- weighted.mean(mujeres$ACTI_SIN_PAGO1, mujeres$FACTOR, na.rm = TRUE) +
                    weighted.mean(mujeres$ACTI_SIN_PAGO2, mujeres$FACTOR, na.rm = TRUE) +
                    weighted.mean(mujeres$ACTI_SIN_PAGO3, mujeres$FACTOR, na.rm = TRUE) +
                    weighted.mean(mujeres$ACTI_SIN_PAGO4, mujeres$FACTOR, na.rm = TRUE) +
                    weighted.mean(mujeres$ACTI_SIN_PAGO5, mujeres$FACTOR, na.rm = TRUE) 

  #Hombres
  hrs_cuidados_h <- weighted.mean(hombres$ACTI_SIN_PAGO1, hombres$FACTOR, na.rm = TRUE) +
                    weighted.mean(hombres$ACTI_SIN_PAGO2, hombres$FACTOR, na.rm = TRUE) +
                    weighted.mean(hombres$ACTI_SIN_PAGO3, hombres$FACTOR, na.rm = TRUE) +
                    weighted.mean(hombres$ACTI_SIN_PAGO4, hombres$FACTOR, na.rm = TRUE) +
                    weighted.mean(hombres$ACTI_SIN_PAGO5, hombres$FACTOR, na.rm = TRUE) 

  #Diferencia (Indicador)
  DC_Cob_Ind_2.2 <- hrs_cuidados_m - hrs_cuidados_h
  DC_Cob_Ind_2.2


# 5. Guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel)
  DC_Cob_Ind_1.1 <- as.data.frame(DC_Cob_Ind_1.1)
  DC_Cob_Ind_1.2 <- as.data.frame(DC_Cob_Ind_1.2)
  DC_Cob_Ind_2.1 <- as.data.frame(DC_Cob_Ind_2.1)
  DC_Cob_Ind_2.2 <- as.data.frame(DC_Cob_Ind_2.2)
  
  write_xlsx(list(DC_Cob_Ind_1.1, DC_Cob_Ind_1.2, DC_Cob_Ind_2.1, DC_Cob_Ind_2.2), "output/DC_Cob.xlsx")
  