# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósito de DC_Asq: 
# Calcular indicadores del atributo de asequibilidad del derecho a cuidar

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

######################## Seleccionar estado de interés #########################

# 1. Revisar este documento para obtener el código del estado de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en el siguiente objeto
estado <- 6

############################# Cargar bases #####################################

# 1. Cargar bases
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

# 6. Filtrar solo para el estado y edades de interés
entidad <- enoe %>% 
  filter(ent == estado) %>%
  filter(eda>=15 & eda!=99)

########################### Calcular indicadores ###############################

# 1. Filtrar para poblaciones relevantes
  #Poblacion ocupada
  pea_ocup <- entidad %>% 
    filter(clase1 == 1) %>%
    filter(clase2 == 1) 

# 2. Acceso a tiempo para cuidados paternos o maternos 
# Solo 1er trabajo, la pregunta no se realiza para 2do trabajo
# El calculo se realiza sobre la poblacion ocupada, porque nos importa el total no 
# solo los trabajadores subordinados y remunerados
t_cuid_matpat <- sum(pea_ocup$fac[pea_ocup$p3m3 == 3], na.rm = TRUE)/sum(pea_ocup$fac, na.rm = TRUE) #Total
cuid_mat <- sum(pea_ocup$fac[pea_ocup$p3m3 == 3 & pea_ocup$sex==2], na.rm = TRUE)/sum(pea_ocup$fac[pea_ocup$sex==2], na.rm = TRUE) #Mujeres
cuid_pat <- sum(pea_ocup$fac[pea_ocup$p3m3 == 3 & pea_ocup$sex==1], na.rm = TRUE)/sum(pea_ocup$fac[pea_ocup$sex==1], na.rm = TRUE) #Hombres

# 3. Acceso a guarderías (solo 1er trabajo, la pregunta no se realiza para 2do trabajo)
t_guard <- sum(pea_ocup$fac[pea_ocup$p3m2 == 2], na.rm = TRUE)/sum(pea_ocup$fac, na.rm = TRUE) #Total
guard_muj <- sum(pea_ocup$fac[pea_ocup$p3m2 == 2 & pea_ocup$sex==2], na.rm = TRUE)/sum(pea_ocup$fac[pea_ocup$sex==2], na.rm = TRUE) #Mujeres
guard_hom <-sum(pea_ocup$fac[pea_ocup$p3m2 == 2 & pea_ocup$sex==1], na.rm = TRUE)/sum(pea_ocup$fac[pea_ocup$sex==1], na.rm = TRUE) #Hombres

# 4. Una de las dos prestaciones, por género
prest_muj <- sum(pea_ocup$fac[(pea_ocup$p3m2 == 2 | pea_ocup$p3m3 == 3) & pea_ocup$sex==2], na.rm = TRUE)/sum(pea_ocup$fac[pea_ocup$sex == 2], na.rm = TRUE) #Mujeres
prest_hom <- sum(pea_ocup$fac[(pea_ocup$p3m2 == 2 | pea_ocup$p3m3 == 3) & pea_ocup$sex==1], na.rm = TRUE)/sum(pea_ocup$fac[pea_ocup$sex == 1], na.rm = TRUE) #Hombres

# 5. Indicadores finales
DC_Asq_1.1 <- t_cuid_matpat*100
DC_Asq_1.1
DC_Asq_1.2 <- t_guard*100
DC_Asq_1.2

# 6. Guardar el resultado en la carpeta de output (como archivo .xlsx de Excel)
DC_Asq_1.1 <- as.data.frame(DC_Asq_1.1)
DC_Asq_1.2 <- as.data.frame(DC_Asq_1.2)

write_xlsx(list(DC_Asq_1.1, DC_Asq_1.2), "output/DC_Asq.xlsx")
