library(climaemet)
library(tibble)
library(tidyverse)

# Obtención de las Tª diaras por estaciones aemet (NO volver a ejecutar):
observaciones_21 <-aemet_daily_period_all(start = 2021, end = 2021)

observaciones_22 <-aemet_daily_period_all(start = 2022, end = 2022)

# Explicación Antonio: 
observaciones_diarias <-aemet_daily_period_all(start = 2021, end = 2022)
# nos ha dicho q a partir d estos datos usemoss una de las funciones de la url para separar por años https://lubridate.tidyverse.org/reference/index.html 
View(observaciones_diarias)

temp_provincias <-  observaciones_diarias %>%
  mutate(years=lubridate::year(fecha))%>%
  group_by (years,provincia) %>%
  summarise(
    tmedia = mean(tmed, na.rm = TRUE)
  )
view(temp_provincias)
  # select(nombre, tmed) %>%
  #summarise(
    #tmed22 = mean(tmed, na.rm = TRUE)
  #)



# Desde aquí ya tenemos nuestros datos de interés: tenemos q modificar en base a lo q ha dicho el profe

# Temperatura 2021


temp_2021 <-  observaciones_21 %>%
  group_by (provincia) %>%
  # select(nombre, tmed) %>%
  summarise(
    tmed21 = mean(tmed, na.rm = TRUE)
  )


# Temperatura 2022

temp_2022 <-  observaciones_22 %>%
  group_by (provincia) %>%
  # select(nombre, tmed) %>%
  summarise(
    tmed22 = mean(tmed, na.rm = TRUE)
  )

# Unión de las tablas de temperatura: según lo del profe esto ya no haría falta
temp_21_22 <- full_join(x = temp_2021, y = temp_2022)

# Nuestra tabla de interés es:
temp_21_22

# Explicación del profe para obtener las provincias asociadas a las CCAA: usar cause when para juntar las provincias a las CCAA

# Explicación para obtener las tablas de interés de forma correcta: 
#library(DT)
#datatable(iris) iris sería nuestro conjunto de datos de interés


