# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de IT_CDI_Pert_Est: 
# Calcularla información complementaria de cuidados para el desarrollo
# integral para infancia temprana desde el Estado.

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

########################### Cargar bases ######################################

# 1. Cargar base
datos <- read_xlsx("input/ENCAL/2019_nov_encalG_nacional.xlsx")

# 2. Limpiar nombres columnas
datos <- clean_names(datos)
#NOTA: De las siguientes dos líneas, solo se aplicará la que corresponda a cómo
# su computadora importa los datos.
datos <- rename(datos,"codigo_ent" = deleg, "nombre_ent" = delegacio_n)
datos <- rename(datos,"codigo_ent" = deleg, "nombre_ent" = delegaci_un)

############### Seleccionar estado o región de interés #########################

# 1. Para conocer el código correspondiente al estado/región de interés
print.data.frame(unique(datos[c("codigo_ent","nombre_ent")]))

# 2. Reemplazar aquí por el código del estado/región de interés
estado <- 8

# 3. Filtrar la base solo para el estado o región de interés
datos_estado <- datos %>%
  filter(codigo_ent == estado)

################## Calcular información complementaria #########################

# 1. Denominador
denominador <- dim(datos_estado)[1] # Total de personas encuestadas

# 2. Numerador
numerador <- sum(datos_estado$sat1 == 1 | datos_estado$sat1 == 2) # Los que dicen estar "Muy Satisfechos" o "Satisfechos"

# 3. Resultado del indicador
IT_CDI_Pert_Est_Comp <- (numerador/denominador)*100
IT_CDI_Pert_Est_Comp

# Guardar el resultado en la carpeta de output (como archivo .xlsx de Excel)
IT_CDI_Pert_Est_Comp  <- as.data.frame(IT_CDI_Pert_Est_Comp)
write_xlsx(IT_CDI_Pert_Est_Comp, "output/IT_CDI_Pert_Est.xlsx")
