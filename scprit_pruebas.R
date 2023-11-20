
library(tibble)
library(tidyverse)

# Obtención de las Tª diaras por estaciones aemet (NO volver a ejecutar):
observaciones_21 <-aemet_daily_period_all(start = 2021, end = 2021)

observaciones_22 <-aemet_daily_period_all(start = 2022, end = 2022)


# Desde aquí ya tenemos nuestros datos de interés: 

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

# Unión de las tablas de temperatura:
temp_21_22 <- full_join(x = temp_2021, y = temp_2022)

# Nuestra tabla de interés es:
temp_21_22


