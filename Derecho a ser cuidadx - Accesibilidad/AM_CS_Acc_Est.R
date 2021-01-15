# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de AM_CS_Acc_Est: 
# Calcular el indicador de accesibilidad de cuidados de salud
# para personas adultas mayores desde el Estado.

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

############################### Cargar bases ###################################

#1. Cargar base
intercensal <- read.csv("input/Intercensal/TR_PERSONA06.CSV")

#2. Limpiar nombres variables
intercensal <- clean_names(intercensal)

################### Seleccionar municipio de interés ###########################

# 1. Revisar este documento para obtener el código del municipio de interés 
# (https://www.inegi.org.mx/app/ageeml/). 
# Nótese que no es necesario seleccionar el estado porque la base descarga ya
# corresponde al estado de interés para el análisis.

# 2. Sustituir código en el siguiente objeto
municipio <- 7

# 3. Filtrar base por municipio de interés
intercensal <- intercensal %>% 
  filter(mun == municipio)  

##################### Extraer información para indicadores #####################

# 1. Calcular denominadores por institución pública
  #Población total
  personas_adultas_mayores <- intercensal %>%
    filter(edad>=65 & edad<=110)
  #Seguro Popular
  seg_pop <- personas_adultas_mayores[personas_adultas_mayores$dhsersal1==1,]
  denominador.1 <- sum(seg_pop$factor, na.rm = TRUE)
  #IMSS
  imss <- personas_adultas_mayores[(personas_adultas_mayores$dhsersal1==2 | personas_adultas_mayores$dhsersal2==2),]
  denominador.2 <- sum(imss$factor, na.rm = TRUE)
  #ISSSTE
  issste <- personas_adultas_mayores[(personas_adultas_mayores$dhsersal1==3 | personas_adultas_mayores$dhsersal2==3),]
  denominador.3 <- sum(issste$factor, na.rm = TRUE)
  #Institutos de seguridad pública estatales
  issste_est <- personas_adultas_mayores[(personas_adultas_mayores$dhsersal1==4 | personas_adultas_mayores$dhsersal2==4),]
  denominador.4 <- sum(issste_est$factor, na.rm = TRUE)
  #Pemex, Defensa, Marina
  pemex <- personas_adultas_mayores[(personas_adultas_mayores$dhsersal1==5 | personas_adultas_mayores$dhsersal2==5),]
  denominador.5 <- sum(pemex$factor, na.rm = TRUE)
  #Otra institución
  otra_inst <- personas_adultas_mayores[(personas_adultas_mayores$dhsersal1==7 | personas_adultas_mayores$dhsersal2==7),]
  denominador.6 <- sum(otra_inst$factor, na.rm = TRUE)
  #Denominador total (una o más de las instituciones públicas)
  seg_publico <- personas_adultas_mayores[((personas_adultas_mayores$dhsersal1>=1 & personas_adultas_mayores$dhsersal1<=5) | personas_adultas_mayores$dhsersal1==7) |
                                  ((personas_adultas_mayores$dhsersal2>=2 & personas_adultas_mayores$dhsersal2<=5) | personas_adultas_mayores$dhsersal2==7),]
  denominador <- sum(seg_publico$factor, na.rm = TRUE)

# 2. Calcular numeradores por institución pública
  #Población total
  personas_adultas_mayores <- intercensal %>%
    filter(edad>=65 & edad<=110)
  #Seguro Popular
  seg_pop <- personas_adultas_mayores[personas_adultas_mayores$dhsersal1==1 & personas_adultas_mayores$sersal==5,]
  numerador.1 <- sum(seg_pop$factor, na.rm = TRUE)
  numerador.1
  #IMSS
  imss <- personas_adultas_mayores[(personas_adultas_mayores$dhsersal1==2 | personas_adultas_mayores$dhsersal2==2) & personas_adultas_mayores$sersal==1,]
  numerador.2 <- sum(imss$factor, na.rm = TRUE)
  #ISSSTE
  issste <- personas_adultas_mayores[(personas_adultas_mayores$dhsersal1==3 | personas_adultas_mayores$dhsersal2==3) & personas_adultas_mayores$sersal==2,]
  numerador.3 <- sum(issste$factor, na.rm = TRUE)
  #Institutos de seguridad pública estatales
  issste_est <- personas_adultas_mayores[(personas_adultas_mayores$dhsersal1==4 | personas_adultas_mayores$dhsersal2==4) & personas_adultas_mayores$sersal==3,]
  numerador.4 <- sum(issste_est$factor, na.rm = TRUE)
  #Pemex, Defensa, Marina
  pemex <- personas_adultas_mayores[(personas_adultas_mayores$dhsersal1==5 | personas_adultas_mayores$dhsersal2==5) & personas_adultas_mayores$sersal==4,]
  numerador.5 <- sum(pemex$factor, na.rm = TRUE)
  #Otra institución
  otra_inst <- personas_adultas_mayores[(personas_adultas_mayores$dhsersal1==7 | personas_adultas_mayores$dhsersal2==7) & personas_adultas_mayores$sersal==8,]
  numerador.6 <- sum(otra_inst$factor, na.rm = TRUE)
  #Nominador total 
  seg_publico <- personas_adultas_mayores[(personas_adultas_mayores$dhsersal1==1 & personas_adultas_mayores$sersal==5) |
                                  ((personas_adultas_mayores$dhsersal1==2 | personas_adultas_mayores$dhsersal2==2) & personas_adultas_mayores$sersal==1) |
                                  ((personas_adultas_mayores$dhsersal1==3 | personas_adultas_mayores$dhsersal2==3) & personas_adultas_mayores$sersal==2) |
                                  ((personas_adultas_mayores$dhsersal1==4 | personas_adultas_mayores$dhsersal2==4) & personas_adultas_mayores$sersal==3) |
                                  ((personas_adultas_mayores$dhsersal1==5 | personas_adultas_mayores$dhsersal2==5) & personas_adultas_mayores$sersal==4) | 
                                  ((personas_adultas_mayores$dhsersal1==5 | personas_adultas_mayores$dhsersal2==5) & personas_adultas_mayores$sersal==4) |
                                  ((personas_adultas_mayores$dhsersal1==7 | personas_adultas_mayores$dhsersal2==7) & personas_adultas_mayores$sersal==8),]
  numerador <- sum(seg_publico$factor, na.rm = TRUE)
  numerador

# 3. Resultado del indicador 
AM_CS_Acc_Est <- (numerador/denominador)*100
AM_CS_Acc_Est

# 4. Guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel)
AM_CS_Acc_Est <- as.data.frame(AM_CS_Acc_Est) 
write_xlsx(AM_CS_Acc_Est, "output/AM_CS_Acc_Est.xlsx")
