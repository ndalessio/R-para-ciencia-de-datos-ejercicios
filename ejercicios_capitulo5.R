
mutate(vuelos, 
       salida_programada = horas_a_minutos(salida_programada),
       horario_salida = horas_a_minutos(horario_salida))

##Compara tiempo_vuelo con horario_llegada - horario_salida. ¿Qué esperas ver? ¿Qué ves? 
##¿Qué necesitas hacer para arreglarlo?

select(vuelos1, tiempo_vuelo, horario_llegada - horario_salida)

vuelos1 <- vuelos %>%
  mutate(horario_salida = (horario_salida %/% 100) * 60 + horario_salida %% 100,
         horario_llegada = (horario_llegada %/% 100) * 60 + horario_llegada %% 100,
         tiempo_vuelo2 = horario_llegada - horario_salida)
