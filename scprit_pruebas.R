library(climaemet)
library(tibble)
library(tidyverse)
library(rjson)
library(tidyjson)
# Obtención de las Tª diaras por estaciones aemet (NO volver a ejecutar):

observaciones_diarias <-aemet_daily_period_all(start = 2021, end = 2022)
# nos ha dicho q a partir d estos datos usemoss una de las funciones de la url para separar por años https://lubridate.tidyverse.org/reference/index.html 
View(observaciones_diarias)
#temperatura de las provinias
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
#creacion de una columna con las comunidades autonomas
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
  
#library(jsonlite)
# Unión de las tablas de 
psicologos_2021 <- fromJSON("file=DATA/psicologos_CA_2021.json")
data.json <- data.frame(data.json,row.names = NULL)
psicologos_2021 <- fromJSON(txt = readLines(file=DATA/psicologos_CA_2022.json, warn = FALSE))

columnas_deseadas <- psicologos_2021 %>%
  select(Nombre, Data)
View(psicologos_2021)
View(columnas_deseadas)

labels(factor(psicologos_2021$Nombre))

head(psicologos_2021)

psicologos_2021 %>% 
  spread_all() %>% 
  gather_object() %>% 
  json_types() %>% 
  count(name, type)

psicologos_21<- psicologos_2021 %>% 
  enter_object(Data) %>% 
  gather_array() %>% 
  spread_all() %>%
  select(-document.id, - array.index)
View(psicologos_21)

psicologos_2022<- fromJSON(file = "DATA/psicologos_CA_2022.json")
psicologos_2021<- fromJSON(file = "DATA/psicologos_CA_2021.json")








# Explicación para obtener las tablas de interés de forma correcta: 
#library(DT)
#datatable(iris) iris sería nuestro conjunto de datos de interés

ruta_json <- "DATA/psicologos_CA_2021.json"

# Cargar el contenido del JSON en un objeto de R
psicologos_json <- fromJSON(txt = readLines(ruta_json, warn = FALSE))
View(psicologos_json)
# Visualizar la estructura de los datos
str(psicologos_json)