# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de PD_CP_Acc_Merc: 
# Calcular el indicador de accesibilidad de cuidados personales
# para personas con discapacidad desde el mercado.

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

##################### Seleccionar estado de interés ############################

# 1. Revisar este documento para obtener el código del estado de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en el siguiente objeto
estado <- 6

########################### Cargar bases #######################################

# 1. Cargar bases ENOE
coe1 <- read.csv("input/ENOE/COE1T120.csv")
coe2 <- read.csv("input/ENOE/COE2T12.csv")
sdem <- read.csv("input/ENOE/SDEMT120.csv")

# Corregir nombre variables - este paso solo es necesario cuando la computadora importa la primera columna con el prefijo "ï.."
coe1 <- coe1 %>%
  rename(r_def = ï..r_def)
coe2 <- coe2 %>%
  rename(r_def = ï..r_def)
sdem <- sdem %>%
  rename(r_def = ï..r_def)

# 2. Mantener solo encuestas respondidas SDEM
sdem <- sdem %>%
  filter(r_def == 00 & c_res != 2)

# 3. Unir bases del cuestionario sobre empleo y ocupación
coe <- inner_join(coe1, coe2)

# 4. Filtar COE a solo entrevistas realizadas
coe <- coe %>%
  filter(r_def == 00)

# 5. Juntar SDEM y COE
enoe <- left_join(coe, sdem) 
#NOTA: Nos quedamos solo con los datos sociodemográficos de personas con respuestas en COE

# 6. Cargar base Censo
censo <- read.csv("input/Censo/iter_06_cpv2010.csv")

# 7. Filtrar bases solo para estado de interés
enoe <- enoe %>%
  filter(ent == estado)
censo <- censo %>%
  filter(ï..entidad == estado)

# 8. Filtrar base PEA ocupada
pea <- enoe %>%
  filter(clase1 == 1) %>%
  filter(clase2 == 1)

###################### Extraer informacion para indicador #######################

# 1. Calcular denominador
denominador <- as.numeric(censo[1, "pcon_lim"])

# 2. Calcular numerador
personas_cuidadoras <- pea[pea$p3 %in% c(5221,5222,9611) & ! is.na(pea$p3),] 
numerador <- sum(personas_cuidadoras$fac)

# 3. Resultados indicador 
PD_CP_Acc_Merc <- (numerador/denominador)
PD_CP_Acc_Merc

# 4. Guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel)
PD_CP_Acc_Merc <- as.data.frame(PD_CP_Acc_Merc)
write_xlsx(PD_CP_Acc_Merc, "output/PD_CP_Acc_Merc.xlsx")
