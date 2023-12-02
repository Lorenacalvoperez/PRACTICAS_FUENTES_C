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
  temp_con_CCAA <- temp_provincias %>%
  mutate(CCAA = case_when(
    provincia %in% c("BURGOS", "AVILA", "LEON", "SALAMANCA", "SEGOVIA", "SORIA", "VALLADOLID" ,"PALENCIA" ,"ZAMORA") ~ "CASTILLA Y LEON",
    provincia %in% c("ALBACETE", "CIUDAD REAL", "CUENCA", "GUADALAJARA", "TOLEDO") ~ "CASTILLA-LA MANCHA",
    provincia %in% c("ALMERIA", "CADIZ", "CORDOBA", "GRANADA", "HUELVA", "JAEN", "MALAGA", "SEVILLA") ~ "ANDALUCIA",
    provincia %in% c("HUESCA", "ZARAGOZA", "TERUEL") ~ "ARAGON",
    provincia == "ASTURIAS" ~ "ASTURIAS",
    provincia == "CANTABRIA" ~ "CANTABRIA",
    provincia == "ILLES BALEARS" ~ "BALEARES",
    provincia %in% c("STA. CRUZ DE TENERIFE", "LAS PALMAS") ~ "CANARIAS",
    provincia == "LA RIOJA" ~ "LA RIOJA",
    provincia %in% c("ARABA/ALAVA", "GIPUZKOA", "BIZKAIA") ~ "PAIS VASCO",
    provincia %in% c("BARCELONA", "GIRONA", "LLEIDA", "TARRAGONA") ~ "CATALUÑA", 
    provincia %in% c("ALICANTE", "CASTELLON", "VALENCIA") ~ "COMUNIDAD VALENCIANA",
    provincia %in% c("BADAJOZ", "CACERES") ~ "EXTREMADURA",
    provincia %in% c("A CORUÑA", "LUGO", "OURENSE", "PONTEVEDRA") ~ "GALICIA", 
    provincia == "MADRID" ~ "COMUNIDAD DE MADRID",
    provincia == "MURCIA" ~ "MURCIA",
    provincia == "NAVARRA" ~ "NAVARRA",
    provincia == "CEUTA" ~ "CEUTA",
    provincia == "MELILLA" ~ "MELILLA",
    
  ))
  view(temp_con_CCAA)
 #TABLA FINAL 
tmed_CCAA<- temp_con_CCAA %>% 
  group_by(CCAA) %>% 
  summarise(
  tmedia = mean(tmedia, na.rm = TRUE)
)
  
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


