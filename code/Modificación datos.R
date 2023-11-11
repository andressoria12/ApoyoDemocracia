# El Quantificador

# Preliminares --------------------------------------------------------------------------------------------
#Importamos librerías
library(dplyr)

# Leer los archivos CSV
ab_04_19 <- read.csv("data/ab_04_19.csv", sep = ",")
ecu2021  <- read.csv("data/ecu2021.csv", sep = ",")

# Identificar las columnas que tienen el mismo nombre en ambos datasets
columnas_comunes <- intersect(names(ecu2021), names(ab_04_19))

# Mostrar las columnas comunes
if (length(columnas_comunes) > 0) {
  cat("Columnas con el mismo nombre en ambos datasets:\n")
  print(columnas_comunes)
} else {
  cat("No hay columnas con el mismo nombre en ambos datasets.")
}

# Desarrollar variables de interés para el análisi en ambos datasets para juntarlos
#Si no existen diferencias dejamos igual para juntar

#------------------------------Género-------------------------------
#Verificamos si son la misma clase 
class(ab_04_19$q1)
class(ecu2021$q1tb)

#Renombramos la columna para cuadar los dos datsets
ecu2021<-rename(ecu2021, 'q1'=q1tb)

#-----------------------------Edad----------------------------------
class(ab_04_19$age)
class(ecu2021$q2)
#Renombramos para coincidir los dos datasets y categoría
ecu2021<-rename(ecu2021, 'age'=q2)
ecu2021$age<-as.integer(ecu2021$age) 

#-------------------------Upm-------------------------
class(ab_04_19$upm)
class(ecu2021$upm)
# Cambiar el tipo de dato de la variable "upm" a "numeric"
ecu2021$upm <- as.numeric(ecu2021$upm)

#------------------------Estratopri--------------------------------
class(ab_04_19$estratopri)
class(ecu2021$estratopri)
#Modificamos para coicidir los Relabels necesarios
ecu2021$region<- ifelse(ecu2021$estratopri == 901, 'Costa',
                   ifelse ( ecu2021$estratopri == 902, 'Sierra', 'Oriente'))
#Verificamos la igualdad
class(ab_04_19$region)
class(ecu2021$region)

#------------------------------Weight1500-------------------------
class(ab_04_19$weight1500)
class(ecu2021$weight1500)

#------------------------------Strata------------------------
class(ab_04_19$strata)
class(ecu2021$strata)
#---------------------------Año----------------------
class(ab_04_19$year)
class(ecu2021$year)

#---------------------------Idnum-------------------
class(ab_04_19$idnum)
class(ecu2021$idnum)
ecu2021$idnum <- as.character(ecu2021$idnum)

#--------------------------Corrupción------------------------------
#Corrupción y los cambios para que valores coicidan
class(ab_04_19$corr_pol)
class(ecu2021$exc2)

class(ab_04_19$corr_pub)
class(ecu2021$exc6)

class(ab_04_19$exc7new)
class(ecu2021$exc7new)

#Modificaciones para coincidir ambos datasets
#Renombrar variables iguales
ecu2021<-rename(ecu2021, 'corr_pol'= exc2, 'corr_pub' = exc6)

#Creación de variable dummie para coincidir el dataset ab_04_19
ecu2021$corrper_new<-ifelse(ecu2021$exc7new >2, 1, 0)

#Creación de variable de corprob
ecu2021$corprob<-ifelse(ecu2021$a4 == 13, 1, 0)

#-----------------------Mercado de trabajo------------------------------
class(ab_04_19$ocup4a)
class(ecu2021$ocup4a)

#Creamos variables necesarias para coincidir los datasets

#Configuramos la variable como factor
ecu2021$ocup4a <- as.integer(ecu2021$ocup4a)

# Variable si es que la persona está empleada
ecu2021$em_4a<-ifelse(ecu2021$ocup4a == 1 | ecu2021$ocup4a == 2 ,1,0)

# Variable si es que la persona está en busca de trabajo
ecu2021$unem_4a<-ifelse(ecu2021$ocup4a == 3,1,0 ) %>% as.factor()

# Variable si es que está desempleada y busca o no trabajo
ecu2021$unem_total<-ifelse(ecu2021$ocup4a == 3 | ecu2021$ocup4a == 7, 1, 0)

ecu2021$unem_4a <- as.integer(ecu2021$unem_4a)
ecu2021$em_4a <- as.integer(ecu2021$em_4a)


# Crear una nueva variable del mercado laboral, con las siguientes categorías

# Trabaja (ya sea trabajando ahora o tiene un trabajo pero no trabajando ahora)
# Desempleados, de cualquier tipo (buscando o no mirando)
# No en la fuerza laboral (Estudiante, Jubilado, Trabaja en casa)

ecu2021$work_2a<-ifelse(ecu2021$ocup4a == 1 | ecu2021$ocup4a == 2, 'Employed',
                   ifelse(ecu2021$ocup4a == 3 | ecu2021$ocup4a == 7, 'Unemployed','Not WF')) %>% as.factor()

# Renivelar para que la referencia no esté en la fuerza laboral
ecu2021$work_2a<-relevel(ecu2021$work_2a, 'Not WF')

#Crear una dummy que indique si la persona está o no en la población económicamente activa (trabaja o puede trabajar)
ecu2021$eap<-ifelse(ecu2021$ocup4a == 1 | ecu2021$ocup4a == 2 | ecu2021$ocup4a == 3 | ecu2021$ocup4a == 7, 'EAP', 'Not EAP')


#---------------------Variables ligadas al artículo de corrupción------------------------
#Se harán los cambios si es que son necesarios
#Variables semejantes en ambos datasets

#Pregunta del modelo de la democracia
class(ab_04_19$ing4)
class(ecu2021$ing4)

#Pregunta del golpe de estado
class(ab_04_19$jc13)
class(ecu2021$jc13)

#Pregunta Gobierno sin Asamblea
class(ab_04_19$jc13)
class(ecu2021$jc13)

#Pregunta satisfacción democracia
class(ab_04_19$pn4)
class(ecu2021$pn4)

#Aprobación presidencial
class(ab_04_19$pres_aprov)
class(ecu2021$m1)
ecu2021<-rename(ecu2021, 'pres_aprov' = m1)

# Juntar ambos conjuntos de datos por filas
ab_04_21 <- dplyr::bind_rows(ab_04_19, ecu2021)

# Guardar el conjunto de datos combinado en un archivo CSV dentro de la carpeta 'output'
write.csv(ab_04_21, "output/ab_04_21.csv", row.names = FALSE)














