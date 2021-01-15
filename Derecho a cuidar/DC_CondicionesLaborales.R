# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósito de DC_CondicionesLaborales:
# Calcular indicadores de las condiciones laborales de sectores remunerados
# dedicados a los cuidados.

# Fecha de actualización: 5 de enero 2021
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

############### Seleccionar municipio y entidad de interés #####################

# 1. Revisar este documento para obtener el código del estado de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en el siguiente objeto
estado <- 6

############################## Cargar bases ####################################

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

###################### Crear y cuantificar grupos de interés ##################

#1. Filtrar solo para la PEA ocupada
pea <- entidad %>%
  filter(clase1 == 1) %>%
  filter(clase2 == 1)

#2. Ajustar valores de algunas variables para su uso posterior
pea <- pea %>%
  replace_with_na_at(.vars = c("ingocup", "ing_x_hrs", "hrsocup"),
                     condition ~.x == 0) 

#3. Generar grupos laborales

#3.1 - Trabajadores domésticos
#Incluye las siguientes categorías: trabajadores domésticos, lavanderos y planchadores
#domésticos, y cocineros domésticos
trab_dom <- pea[pea$p3 %in% c(9611, 9643, 5113) & ! is.na(pea$p3),]
  #Número total
  sum(trab_dom$fac)
  #Proporción mujeres
  sum(trab_dom$fac[trab_dom$sex == 2])/sum(trab_dom$fac)

#3.2 - Cuidadores en casas particulares
#Incluye la categoría: cuidadores de niños, personas con discapacidad y ancianos en casas particulares
cuid_part <- pea[pea$p3 == 5222 & ! is.na(pea$p3),]
  #Número total
  sum(cuid_part$fac)
  #Proporción mujeres
  sum(cuid_part$fac[cuid_part$sex == 2])/sum(cuid_part$fac)

#3.3 - Personas que trabajan como cuidadores en establecimientos
#Incluye la categoría: cuidadores de niños, personas con discapacidad y ancianos en establecimientos
cuid_est <- pea[pea$p3 == 5221 & ! is.na(pea$p3),]
  #Número total
  sum(cuid_est$fac)
  #Proporción mujeres
  sum(cuid_est$fac[cuid_est$sex == 2])/ sum(cuid_est$fac)

#3.4 - Profesores de enseñanza preescolar
#Incluye la categoría profesores de enseñanza preescolar
prof_pre <- pea[pea$p3 == 2335 & ! is.na(pea$p3),]
  #Número total
  sum(prof_pre$fac)
  #Proporción mujeres
  sum(prof_pre$fac[prof_pre$sex == 2])/sum(prof_pre$fac)

#3.5 - Profesores de enseñanza a personas con alguna limitación
#Incluye las categorías: profesores de personas con problemas de audición y lenguaje,
#profesores de personas con problemas de aprendizaje, y otros profesores de enseñanza especial
prof_lim <-  pea[pea$p3 %in% c(2341, 2342, 2343) & ! is.na(pea$p3),]
  #Número total
  sum(prof_lim$fac)
  #Proporción mujeres
  sum(prof_lim$fac[prof_lim$sex == 2])/sum(prof_lim$fac)

#3.6 - Médicos y especialistas
#Incluye las categorías médicos generales y familiares, dentistas, y médicos especialistas
#(anestesiólogos, cirujanos, ginecólogos y obstretas, médicos internos, patólogos, pediatras,
#psiquiatras, radiólogos, y salud médicos especialistas)
medicos <- pea[pea$p3 %in% 
                 c(2411, 2412, 2421, 2422, 2423,
                   2424, 2425, 2426, 2427, 2428, 2429) & ! is.na(pea$p3),]
  #Número total
  sum(medicos$fac)
  #Proporción mujeres
  sum(medicos$fac[medicos$sex == 2])/sum(medicos$fac)

#3.7 - Otros trabajadores en salud
#Incluye las categorías enfermeras y paramédicos profesionales, fisioterapeutas y logopedas,
#auxiliares en enfermería y paramédicos, auxiliares y asistentes dentales,
#auxiliares hospitalarios y de medicina y parteras)
otros_salud <- pea[pea$p3 %in% c(2436, 2437, 2821, 2822, 2823, 2824) & ! is.na(pea$p3),]
  #Número total
  sum(otros_salud$fac)
  #Proporción mujeres
  sum(otros_salud$fac[otros_salud$sex == 2])/sum(otros_salud$fac)

#3.8 - Poblacion economicamente activa, ocupada
  #Número total
  sum(pea$fac)
  #Proporción mujeres
  sum(pea$fac[pea$sex == 2])/sum(pea$fac)

################### Condiciones laborales, por grupo  #########################

# 1. Mediana de ingreso mensual (para quienes reciben ingresos)
ing_mens <- c(median(rep(trab_dom$ingocup, times=trab_dom$fac), na.rm = TRUE),
              median(rep(cuid_part$ingocup, times=cuid_part$fac), na.rm = TRUE),
              median(rep(cuid_est$ingocup, times=cuid_est$fac), na.rm = TRUE),
              median(rep(prof_pre$ingocup, times=prof_pre$fac), na.rm = TRUE),
              median(rep(prof_lim$ingocup, times=prof_lim$fac), na.rm = TRUE),
              median(rep(medicos$ingocup, times=medicos$fac), na.rm = TRUE),
              median(rep(otros_salud$ingocup, times=otros_salud$fac), na.rm = TRUE),
              median(rep(pea$ingocup, times=pea$fac), na.rm = TRUE))

# 2. Mediana de ingreso por hora (para quienes reciben ingresos)
ing_hrs <- c(median(rep(trab_dom$ing_x_hrs, times=trab_dom$fac), na.rm = TRUE),
             median(rep(cuid_part$ing_x_hrs, times=cuid_part$fac), na.rm = TRUE),
             median(rep(cuid_est$ing_x_hrs, times=cuid_est$fac), na.rm = TRUE),
             median(rep(prof_pre$ing_x_hrs, times=prof_pre$fac), na.rm = TRUE),
             median(rep(prof_lim$ing_x_hrs, times=prof_lim$fac), na.rm = TRUE),
             median(rep(medicos$ing_x_hrs, times=medicos$fac), na.rm = TRUE),
             median(rep(otros_salud$ing_x_hrs, times=otros_salud$fac), na.rm = TRUE),
             median(rep(pea$ing_x_hrs, times=pea$fac), na.rm = TRUE))

# 3. Duración jornada laboral
jornada <- c(median(rep(trab_dom$hrsocup, times=trab_dom$fac), na.rm = TRUE),
             median(rep(cuid_part$hrsocup, times=cuid_part$fac), na.rm = TRUE),
             median(rep(cuid_est$hrsocup, times=cuid_est$fac), na.rm = TRUE),
             median(rep(prof_pre$hrsocup, times=prof_pre$fac), na.rm = TRUE),
             median(rep(prof_lim$hrsocup, times=prof_lim$fac), na.rm = TRUE),
             median(rep(medicos$hrsocup, times=medicos$fac), na.rm = TRUE),
             median(rep(otros_salud$hrsocup, times=otros_salud$fac), na.rm = TRUE),
             median(rep(pea$hrsocup, times=pea$fac), na.rm = TRUE))

# 4. Aguinaldo
aguinaldo <- c(sum(trab_dom$fac[trab_dom$p3l1 == 1], na.rm = TRUE)/sum(trab_dom$fac, na.rm = TRUE)*100,
               sum(cuid_part$fac[cuid_part$p3l1 == 1], na.rm = TRUE)/sum(cuid_part$fac, na.rm = TRUE)*100,
               sum(cuid_est$fac[cuid_est$p3l1 == 1], na.rm = TRUE)/sum(cuid_est$fac, na.rm = TRUE)*100,
               sum(prof_pre$fac[prof_pre$p3l1 == 1], na.rm = TRUE)/sum(prof_pre$fac, na.rm = TRUE)*100,
               sum(prof_lim$fac[prof_lim$p3l1 == 1], na.rm = TRUE)/sum(prof_lim$fac, na.rm = TRUE)*100,
               sum(medicos$fac[medicos$p3l1 == 1], na.rm = TRUE)/sum(medicos$fac, na.rm = TRUE)*100,
               sum(otros_salud$fac[otros_salud$p3l1 == 1], na.rm = TRUE)/sum(otros_salud$fac, na.rm = TRUE)*100,
               sum(pea$fac[pea$p3l1 == 1], na.rm = TRUE)/sum(pea$fac, na.rm = TRUE)*100)

# 5. Vacaciones con goce de sueldo
vacaciones <- c(sum(trab_dom$fac[trab_dom$p3l2 == 2], na.rm = TRUE)/sum(trab_dom$fac, na.rm = TRUE)*100,
                sum(cuid_part$fac[cuid_part$p3l2 == 2], na.rm = TRUE)/sum(cuid_part$fac, na.rm = TRUE)*100,
                sum(cuid_est$fac[cuid_est$p3l2 == 2], na.rm = TRUE)/sum(cuid_est$fac, na.rm = TRUE)*100,
                sum(prof_pre$fac[prof_pre$p3l2 == 2], na.rm = TRUE)/sum(prof_pre$fac, na.rm = TRUE)*100,
                sum(prof_lim$fac[prof_lim$p3l2 == 2], na.rm = TRUE)/sum(prof_lim$fac, na.rm = TRUE)*100,
                sum(medicos$fac[medicos$p3l2 == 2], na.rm = TRUE)/sum(medicos$fac, na.rm = TRUE)*100,
                sum(otros_salud$fac[otros_salud$p3l2 == 2], na.rm = TRUE)/sum(otros_salud$fac, na.rm = TRUE)*100,
                sum(pea$fac[pea$p3l2 == 2], na.rm = TRUE)/sum(pea$fac, na.rm = TRUE)*100)

# 6. Crédito para vivienda
vivienda <- c(sum(trab_dom$fac[trab_dom$p3m1 == 1], na.rm = TRUE)/sum(trab_dom$fac, na.rm = TRUE)*100,
              sum(cuid_part$fac[cuid_part$p3m1 == 1], na.rm = TRUE)/sum(cuid_part$fac, na.rm = TRUE)*100,
              sum(cuid_est$fac[cuid_est$p3m1 == 1], na.rm = TRUE)/sum(cuid_est$fac, na.rm = TRUE)*100,
              sum(prof_pre$fac[prof_pre$p3m1 == 1], na.rm = TRUE)/sum(prof_pre$fac, na.rm = TRUE)*100,
              sum(prof_lim$fac[prof_lim$p3m1 == 1], na.rm = TRUE)/sum(prof_lim$fac, na.rm = TRUE)*100,
              sum(medicos$fac[medicos$p3m1 == 1], na.rm = TRUE)/sum(medicos$fac, na.rm = TRUE)*100,
              sum(otros_salud$fac[otros_salud$p3m1 == 1], na.rm = TRUE)/sum(otros_salud$fac, na.rm = TRUE)*100,
              sum(pea$fac[pea$p3m1 == 1], na.rm = TRUE)/sum(pea$fac, na.rm = TRUE)*100)

# 7. Guardería
guarderia <- c(sum(trab_dom$fac[trab_dom$p3m2 == 2], na.rm = TRUE)/sum(trab_dom$fac, na.rm = TRUE)*100,
               sum(cuid_part$fac[cuid_part$p3m2 == 2], na.rm = TRUE)/sum(cuid_part$fac, na.rm = TRUE)*100,
               sum(cuid_est$fac[cuid_est$p3m2 == 2], na.rm = TRUE)/sum(cuid_est$fac, na.rm = TRUE)*100,
               sum(prof_pre$fac[prof_pre$p3m2 == 2], na.rm = TRUE)/sum(prof_pre$fac, na.rm = TRUE)*100,
               sum(prof_lim$fac[prof_lim$p3m2 == 2], na.rm = TRUE)/sum(prof_lim$fac, na.rm = TRUE)*100,
               sum(medicos$fac[medicos$p3m2 == 2], na.rm = TRUE)/sum(medicos$fac, na.rm = TRUE)*100,
               sum(otros_salud$fac[otros_salud$p3m2 == 2], na.rm = TRUE)/sum(otros_salud$fac, na.rm = TRUE)*100,
               sum(pea$fac[pea$p3m2 == 2], na.rm = TRUE)/sum(pea$fac, na.rm = TRUE)*100)

# 8. Tiempo para cuidados maternos o paternos
cuidados <- c(sum(trab_dom$fac[trab_dom$p3m3 == 3], na.rm = TRUE)/sum(trab_dom$fac, na.rm = TRUE)*100,
              sum(cuid_part$fac[cuid_part$p3m3 == 3], na.rm = TRUE)/sum(cuid_part$fac, na.rm = TRUE)*100,
              sum(cuid_est$fac[cuid_est$p3m3 == 3], na.rm = TRUE)/sum(cuid_est$fac, na.rm = TRUE)*100,
              sum(prof_pre$fac[prof_pre$p3m3 == 3], na.rm = TRUE)/sum(prof_pre$fac, na.rm = TRUE)*100,
              sum(prof_lim$fac[prof_lim$p3m3 == 3], na.rm = TRUE)/sum(prof_lim$fac, na.rm = TRUE)*100,
              sum(medicos$fac[medicos$p3m3 == 3], na.rm = TRUE)/sum(medicos$fac, na.rm = TRUE)*100,
              sum(otros_salud$fac[otros_salud$p3m3 == 3], na.rm = TRUE)/sum(otros_salud$fac, na.rm = TRUE)*100,
              sum(pea$fac[pea$p3m3 == 3], na.rm = TRUE)/sum(pea$fac, na.rm = TRUE)*100)

# 9. Seguro médico privado
seg_privado <- c(sum(trab_dom$fac[trab_dom$p3m6 == 6], na.rm = TRUE)/sum(trab_dom$fac, na.rm = TRUE)*100,
                 sum(cuid_part$fac[cuid_part$p3m6 == 6], na.rm = TRUE)/sum(cuid_part$fac, na.rm = TRUE)*100,
                 sum(cuid_est$fac[cuid_est$p3m6 == 6], na.rm = TRUE)/sum(cuid_est$fac, na.rm = TRUE)*100,
                 sum(prof_pre$fac[prof_pre$p3m6 == 6], na.rm = TRUE)/sum(prof_pre$fac, na.rm = TRUE)*100,
                 sum(prof_lim$fac[prof_lim$p3m6 == 6], na.rm = TRUE)/sum(prof_lim$fac, na.rm = TRUE)*100,
                 sum(medicos$fac[medicos$p3m6 == 6], na.rm = TRUE)/sum(medicos$fac, na.rm = TRUE)*100,
                 sum(otros_salud$fac[otros_salud$p3m6 == 6], na.rm = TRUE)/sum(otros_salud$fac, na.rm = TRUE)*100,
                 sum(pea$fac[pea$p3m6 == 6], na.rm = TRUE)/sum(pea$fac, na.rm = TRUE)*100)

# 10. IMSS
imss <- c(sum(trab_dom$fac[trab_dom$p6d==1], na.rm = TRUE)/sum(trab_dom$fac, na.rm = TRUE)*100,
          sum(cuid_part$fac[cuid_part$p6d==1], na.rm = TRUE)/sum(cuid_part$fac, na.rm = TRUE)*100,
          sum(cuid_est$fac[cuid_est$p6d==1], na.rm = TRUE)/sum(cuid_est$fac, na.rm = TRUE)*100,
          sum(prof_pre$fac[prof_pre$p6d==1], na.rm = TRUE)/sum(prof_pre$fac, na.rm = TRUE)*100,
          sum(prof_lim$fac[prof_lim$p6d==1], na.rm = TRUE)/sum(prof_lim$fac, na.rm = TRUE)*100,
          sum(medicos$fac[medicos$p6d==1], na.rm = TRUE)/sum(medicos$fac, na.rm = TRUE)*100,
          sum(otros_salud$fac[otros_salud$p6d==1], na.rm = TRUE)/sum(otros_salud$fac, na.rm = TRUE)*100,
          sum(pea$fac[pea$p6d==1], na.rm = TRUE)/sum(pea$fac, na.rm = TRUE)*100)

# 11. Otra institución de seguridad social
otra_ss <- c(sum(trab_dom$fac[(trab_dom$p6d>=2 & trab_dom$p6d<=4)], na.rm = TRUE)/sum(trab_dom$fac, na.rm = TRUE)*100,
             sum(cuid_part$fac[(cuid_part$p6d>=2 & cuid_part$p6d<=4)], na.rm = TRUE)/sum(cuid_part$fac, na.rm = TRUE)*100,
             sum(cuid_est$fac[(cuid_est$p6d>=2 & cuid_est$p6d<=4)], na.rm = TRUE)/sum(cuid_est$fac, na.rm = TRUE)*100,
             sum(prof_pre$fac[(prof_pre$p6d>=2 & prof_pre$p6d<=4)], na.rm = TRUE)/sum(prof_pre$fac, na.rm = TRUE)*100,
             sum(prof_lim$fac[(prof_lim$p6d>=2 & prof_lim$p6d<=4)], na.rm = TRUE)/sum(prof_lim$fac, na.rm = TRUE)*100,
             sum(medicos$fac[(medicos$p6d>=2 & medicos$p6d<=4)], na.rm = TRUE)/sum(medicos$fac, na.rm = TRUE)*100,
             sum(otros_salud$fac[(otros_salud$p6d>=2 & otros_salud$p6d<=4)], na.rm = TRUE)/sum(otros_salud$fac, na.rm = TRUE)*100,
             sum(pea$fac[(pea$p6d>=2 & pea$p6d<=4)], na.rm = TRUE)/sum(pea$fac, na.rm = TRUE)*100)

# 12. Guardar los resultados en la carpeta de output (como archivo .xlsx de Excel):
# Crear vectores faltantes
grupo <- c("Trabajadores domésticos","Cuidadores en casas particulares","Cuidadores en establecimientos",
           "Profesores de enseñanza preescolar","Profesores de enseñanza a personas con alguna limitación",
           "Médicos y especialistas","Otros trabajadores en salud", "PEA Ocupada")
totales <- c(sum(trab_dom$fac),sum(cuid_part$fac),sum(cuid_est$fac),sum(prof_pre$fac),
             sum(prof_lim$fac),sum(medicos$fac),sum(otros_salud$fac),sum(pea$fac))
mujeres <- c(sum(trab_dom$fac[trab_dom$sex == 2])/sum(trab_dom$fac),
             sum(cuid_part$fac[cuid_part$sex == 2])/sum(cuid_part$fac),
             sum(cuid_est$fac[cuid_est$sex == 2])/ sum(cuid_est$fac),
             sum(prof_pre$fac[prof_pre$sex == 2])/sum(prof_pre$fac),
             sum(prof_lim$fac[prof_lim$sex == 2])/sum(prof_lim$fac),
             sum(medicos$fac[medicos$sex == 2])/sum(medicos$fac),
             sum(otros_salud$fac[otros_salud$sex == 2])/sum(otros_salud$fac),
             sum(pea$fac[pea$sex == 2])/sum(pea$fac))

# Crear data frame y renombrar
DC_CondicionesLab <- data.frame(grupo,totales,mujeres,ing_mens,ing_hrs,jornada,aguinaldo,
                                vacaciones,vivienda,guarderia,cuidados,seg_privado,
                                imss,otra_ss)
DC_CondicionesLab <- rename(DC_CondicionesLab, "Sector" = grupo, "Total de personas" = totales,
                            "Proporción de mujeres" = mujeres, "Ingreso promedio mensual" = ing_mens, 
                            "Ingreso promedio por hora" = ing_hrs, "Horas promedio de jornada laboral" = jornada,
                            "% que recibe aguinaldo" = aguinaldo, "% con derecho a vacaciones con goce de sueldo" = vacaciones,
                            "% con acceso a crédito para vivienda" = vivienda, "% con acceso a guarderías" = guarderia,
                            "% con acceso a tiempo para cuidados maternos/paternos" = cuidados, "% con seguro privado" = seg_privado,
                            "% con acceso al IMSS" = imss, "% con acceso a otra institución de seguridad social" = otra_ss)

write_xlsx(DC_CondicionesLab,"output/DC_Condiciones Laborales.xlsx")

