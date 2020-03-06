library(tidyverse)
library(datos)

##5.2 Encuentra los vuelos que:
##1. Tuvieron un retraso de llegada de dos o más horas
filter(vuelos, llegada_programada >= 120)

##2. Volaron a Houston (IAH oHOU)
filter(vuelos, destino %in% c("IAH", "HOU"))

##3. Fueron operados por United, American o Delta
filter(vuelos, aerolinea %in% c("UA", "AA","DL"))

##4. Partieron en invierno (julio, agosto y septiembre)
filter(vuelos, mes %in% c(7, 8, 9))

##5. Llegaron más de dos horas tarde, pero no salieron tarde
filter(vuelos, atraso_salida > 120 & atraso_llegada <= 0)

##6. Se retrasaron por lo menos una hora, pero repusieron más de 30 minutos en vuelo
filter(vuelos, atraso_salida >= 60, atraso_salida - atraso_llegada > 30)

##7. Partieron entre la medianoche y las 6 a.m. (incluyente)
summary(vuelos$horario_salida)
filter(vuelos, horario_salida  <= 600 | horario_salida == 2400)

##8. Otra función de dplyr que es útil para usar filtros es between(). ¿Qué hace?
## ¿Puedes usarla para simplificar el código necesario para responder a los desafíos anteriores?
filter(vuelos, between(mes, 7, 9))

## 9. ¿Cuántos vuelos tienen datos faltantes en horario_salida? ¿Qué otras variables tienen valores faltantes? ¿Qué representan estas filas?
sum(is.na(vuelos$horario_salida))
vuelos_NA_salida <- filter(vuelos, is.na(horario_salida))

##10. Las variables horario_llegada, atraso_salida, atraso_llegada, codigo_cola, tiempo_vuelo también
##tienen datos faltantes. Representan vuelos cancelados. 

##11. ¿Por qué NA ^ 0 no es faltante? ¿Por qué NA | TRUE no es faltante? ¿Por qué FALSE & NA no es faltante? ¿Puedes descubrir la regla general? (¡NA * 0 es un contraejemplo complicado!)

## a. En R NA^0 es igual a 1. Esto es porque si reemplazamos NA por cualquier número, dará 1.
## b. En este caso NA representa un valor lógico, es decir, TRUE o FALSE. En cualquiera de los casos el resultado será TRUE.
## F | T = T , T | T = T.
## c. De manera similar, FALSE & NA, seleccionará aquellos casos que cumplan la condición FALSE. 

## 5.3 Arrange 
##1. ¿Cómo podrías usar arrange() para ordenar todos los valores faltantes al comienzo? (Sugerencia: usa is.na()).
arrange(filter(vuelos, is.na(horario_salida)), horario_salida)

##2. Ordena vuelos para encontrar los vuelos más retrasados. Encuentra los vuelos que salieron más temprano.
arrange(vuelos, atraso_salida) ##vuelos que salieron más temprano
arrange(vuelos, desc(atraso_salida)) ##vuelos más retrasados

##3. Ordena vuelos para encontrar los vuelos más rápidos.

vuelos_velocidad <- mutate(vuelos, velocidad = distancia/(tiempo_vuelo*60))
vuelos_velocidad <- arrange(vuelos_velocidad, velocidad)
vuelos_velocidad <- select(vuelos_velocidad, velocidad, distancia, tiempo_vuelo, vuelo, origen, 
                           destino, anio, mes, dia)

##4. ¿Cuáles vuelos viajaron más tiempo? ¿Cuál viajó menos tiempo?
arrange(vuelos, distancia) ##Los que viajaron menos tiempo con respecto a la distancia
arrange(vuelos, desc(distancia)) ##Los que viajaron más con respecto a la distancia

arrange(vuelos, tiempo_vuelo) ##Los que viajaron menos tiempo con respecto a horas de vuelo
arrange(vuelos, desc(tiempo_vuelo)) ##Los que viajaron más tiempo con respecto a horas de vuelo

## 5.4 Select

## 1. Haz una lluvia de ideas sobre tantas maneras como sea posible para seleccionar 
##horario_salida,atraso_salida,horario_llegada, yatraso_llegada de vuelos.

select(vuelos, horario_salida, atraso_salida, horario_llegada, atraso_llegada)
select(vuelos, starts_with("horario"), starts_with("atraso"))
select(vuelos, ends_with("salida"), ends_with("llegada"))
select(vuelos, contains("horar"), contains("atr"))
variables_seleccionadas <- c("horario_salida", "horario_llegada", "atraso_salida", "atraso_llegada")
select(vuelos, one_of(variables_seleccionadas))
##select(vuelos, matches("horario$|atraso$"))

##2. ¿Qué sucede si incluyes el nombre de una variable varias veces en una llamada a select()?
select(vuelos, horario_salida, horario_salida)
##No sucede nada, solo se muestra una vez la variable repetida. 

##3. ¿Qué hace la función one_of()? ¡¿Por qué podría ser útil en conjunto con este vector?
##one_of permite seleccionar las variables que combinan con los nombres de cierto vector de caracteres. Puede ser útil
## almacenar las variables que queremos en n vector, para agregar o quitar en el vector. También es útil
## utilizar on_of ya que si escribimos mal una variable nos avisará que esa variable no existe.

select(vuelos, contains("SALIDA"))

##Pensé que contains distinguiría entre mayúsculas y minúsculas. Este comportamiento se
## puede cambiar con el siguiente comando, el cual trata cada nomre de varible de manera literal. 

select(vuelos, contains("SALIDA", ignore.case = FALSE))

##5.5.2
##Las variables horario_salida y salida_programada tienen un formato conveniente para leer, pero es difícil 
##realizar cualquier cálculo con ellas porque no son realmente números continuos. Transfórmalas hacia un
##formato más conveniente como número de minutos desde la medianoche.

horas_a_minutos <- function (x){
  x %/% 100 * 60 + x %% 100
}

mutate(vuelos, 
       salida_programada = horas_a_minutos(salida_programada),
       horario_salida = horas_a_minutos(horario_salida))

##Compara tiempo_vuelo con horario_llegada - horario_salida. ¿Qué esperas ver? ¿Qué ves? 
##¿Qué necesitas hacer para arreglarlo?

select(vuelos1, tiempo_vuelo, horario_llegada - horario_salida)

##
