# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de IT_CDI_Cob_Hog: 
# Calcular el indicador de cobertura de cuidados para el desarrollo integral
# para infancia temprana desde el hogar.

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

############################### Cargar bases ####################################

# 1. Cargar bases
#Bases para información sobre casas hogares
usuarios_caas <- read_excel("input/CAAS/Poblacion usuaria residente.xls")
alojamientos_caas <- read_excel("input/CAAS/Alojamientos de asistencia social.xls")
intercensal <- read.csv("input/Intercensal/TR_PERSONA06.CSV")

#Bases para información sobre personas cuidadoras remuneradas
eness <- read.csv("input/ENESS/eness2017_cuestionario_basico.csv")

#Bases para información sobre centros educativos
siged_inicial <- read_excel("input/SIGED/Inicial.xlsx")
siged_preescolar <- read_excel("input/SIGED/Preescolar.xlsx")

# 2. Limpiar nombres variables
siged_inicial <- clean_names(siged_inicial)
siged_preescolar <- clean_names(siged_preescolar)
intercensal <- clean_names(intercensal)
usuarios_caas <- clean_names(usuarios_caas)
intercensal <- clean_names(intercensal)
alojamientos_caas <- clean_names(alojamientos_caas)
eness <- clean_names(eness)

#NOTA: De las siguientes dos líneas, solo se aplicará la que corresponda a cómo
# su computadora importa los datos.
siged_inicial <- rename(siged_inicial,
                        "clave_del_municipio_o_delegacion" = clave_del_municipio_o_delegaci_th,
                        "nombre_del_control_publico_o_privado" = nombre_del_control_pblico_o_privado)
siged_inicial <- rename(siged_inicial,
                        "nombre_del_control_publico_o_privado" = nombre_del_control_publico_o_privado)

siged_preescolar <- rename(siged_preescolar,
                           "clave_del_municipio_o_delegacion" = clave_del_municipio_o_delegaci_th,
                           "nombre_del_control_publico_o_privado" = nombre_del_control_pblico_o_privado)
siged_preescolar <- rename(siged_preescolar,
                           "nombre_del_control_publico_o_privado" = nombre_del_control_publico_o_privado)

# 3. Eliminar filas sin datos
usuarios_caas <- usuarios_caas[-1, ]
alojamientos_caas <- alojamientos_caas[-1, ]

# 4. Limpiar bases guarderías y escuelas
siged_inicial <- siged_inicial %>%
  filter(alumnos_total != "NA", #Sin información sobre número de alumnos
  servicio_educativo != "INICIAL NO ESCOLARIZADA") #No escolarizadas

siged_preescolar <- siged_preescolar %>%
  filter(alumnos_total != "NA", #Sin información sobre número de alumnos
         servicio_educativo != "INICIAL NO ESCOLARIZADA") #No escolarizadas

############### Seleccionar municipio y estado de interés ######################

# 1. Revisar este documento para obtener el código del estado y municipio de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en los siguientes objetos:
estado1 <- "06"
estado2 <- 6
municipio1 <- "007"
municipio2 <- 7

# 3. Filtrar bases por estado y municipio de interés
siged_inicial <- siged_inicial %>%
  filter(clave_de_la_entidad_federativa == estado2,
         clave_del_municipio_o_delegacion == municipio2)
siged_preescolar <- siged_preescolar %>%
  filter(clave_de_la_entidad_federativa == estado2,
         clave_del_municipio_o_delegacion == municipio2)
usuarios_caas <- usuarios_caas %>% 
  filter(entidad == estado1, 
         mun == municipio1)  
alojamientos_caas <- alojamientos_caas %>% 
  filter(entidad == estado1, 
         mun == municipio1)
intercensal <- intercensal %>% 
  filter(mun == municipio2)  
eness <- eness %>%
  filter(ent == estado2)

##################### Extraer información para indicador #########################

# 1. Cálculo del indicador
niños <- eness[eness$p1_eda %in% c(0:5) & !is.na(eness$p23),] #0-5 años
p23_hogar_adultos <- niños[niños$p23 %in% c(0, 9, 10),] #Cuidado por su mamá, su papá, su abuelo/a

# 2. Resultado del indicador
IT_CDI_Cob_Hog <- sum(p23_hogar_adultos$factor)/sum(niños$factor)*100
IT_CDI_Cob_Hog

#################### Cálculo de información complementaria #####################

# 1. Proporción 1 
  #1.1 Preescolares privados
  #Denominador
  primera_infancia <- intercensal %>%
    filter(edad>=0 & edad<=5)
  denominador.1 <- sum(primera_infancia$factor)
  #Numerador
  alumnos_priv <- siged_preescolar %>%
    filter(nombre_del_control_publico_o_privado == "PRIVADO")
  numerador.1 <- sum(alumnos_priv$alumnos_total)
  #Indicador
  prop1.1 <- (numerador.1/denominador.1)

  #1.2 Guarderías privadas
  #Numerador
  alumnos_priv <- siged_inicial %>%
    filter(nombre_del_control_publico_o_privado == "PRIVADO")
  numerador.2 <- sum(alumnos_priv$alumnos_total)
  #Indicador
  prop1.2 <- (numerador.2/denominador.1)

  #1.3. Preescolares públicos
  #Numerador
  alumnos_pub <- siged_preescolar %>%
    filter(nombre_del_control_publico_o_privado == "PÚBLICO")
  numerador.3 <- sum(alumnos_pub$alumnos_total)
  #Indicador
  prop1.3 <- (numerador.3/denominador.1)

  #1.4. Guarderías públicas
  #Numerador
  alumnos_pub <- siged_inicial %>%
    filter(nombre_del_control_publico_o_privado == "PÚBLICO")
  numerador.4 <- sum(alumnos_pub$alumnos_total)
  #Indicador
  prop1.4 <- (numerador.4/denominador.1)

  #Proporción 1
  prop1 <- prop1.1 + prop1.2 + prop1.3 + prop1.4

# 2. Porporción 2 - cuidados por personas remuneradas en el hogar
  #Denominador
  total <- eness[eness$p1_eda %in% c(0:5) & !is.na(eness$p23),] #0-5 años
  denominador.2 <- sum(total$factor)
  #Numerador
  cuid_remun <- total[total$p23==12 & (total$p25>0 & total$p25<99999),]
  numerador.5 <- sum(cuid_remun$factor)
  #Proporción 2
  prop2 <- (numerador.5/denominador.2)

  # 3. Proporción 3 - Casas hogares
  # 3.1 Casa hogar privada
  #Denominador
  poblacion_primera_infancia <- intercensal %>%
    filter(edad>=0 & edad<=4) #Se utiliza 0-4 porque ese es el rango utilizado en el CAAS
  denominador.3 <- sum(poblacion_primera_infancia$factor)
  #Numerador
  usuarios_caas <- usuarios_caas %>% 
    subset(select = c("id_aloja", "r_t_0_4")) #Población usuario de 0-4 años
  CAAS <- left_join(alojamientos_caas, usuarios_caas, by="id_aloja") 
  CAAS <- CAAS %>%
    filter(clasealoja == 1, #Casa hogar para menores de edad
           fig_juri!=5 & fig_juri!=7) #Privada
  numerador.6 <- sum(CAAS$r_t_0_4)
  #Indicador
  prop3.1 <- (numerador.6/denominador.3)

  # 3.2 Casa hogar pública
  #Numerador
  usuarios_caas <- usuarios_caas %>% 
    subset(select = c("id_aloja", "r_t_0_4")) #Población usuario de 0-4 años
  CAAS <- left_join(alojamientos_caas, usuarios_caas, by="id_aloja") 
  CAAS <- CAAS %>%
    filter(clasealoja == 1, #Casa hogar para menores de edad
           fig_juri == 5) #Pública
  numerador.7 <- sum(CAAS$r_t_0_4)
  #Indicador
  prop3.2 <- (numerador.7/denominador.3)

  #Total proporción 3
  prop3 <- prop3.1 + prop3.2
  
# 4. Proporción de niños y niñas que están en situación de abandono - No está disponible
  
# 5. Resultado del indicador complementario
  IT_CDI_Cob_Hog_Comp <- (1 - (prop1 + prop2 + prop3))*100
  IT_CDI_Cob_Hog_Comp

# 6. Guardar ambos resultados en la carpeta de output (como archivo .xlsx de Excel)
  IT_CDI_Cob_Hog <- as.data.frame(IT_CDI_Cob_Hog) 
  IT_CDI_Cob_Hog_Comp <- as.data.frame(IT_CDI_Cob_Hog_Comp) 
  write_xlsx(list(IT_CDI_Cob_Hog,IT_CDI_Cob_Hog_Comp), "output/IT_CDI_Cob_Hog.xlsx")
