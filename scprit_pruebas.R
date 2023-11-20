
library(tibble)


library(tidyverse)

# Temperatura 2021
observaciones_21 <-aemet_daily_period_all(start = 2021, end = 2021)

temp_2021 <-  observaciones_21 %>%
  group_by (provincia) %>%
  # select(nombre, tmed) %>%
  summarise(
    tmed21 = mean(tmed, na.rm = TRUE)
  )

# Temperatura 2022
observaciones_22 <-aemet_daily_period_all(start = 2022, end = 2022)

temp_2022 <-  observaciones_22 %>%
  group_by (provincia) %>%
  # select(nombre, tmed) %>%
  summarise(
    tmed22 = mean(tmed, na.rm = TRUE)
  )

# Unión de las tablas de temperatura
#union_tmed <- 
