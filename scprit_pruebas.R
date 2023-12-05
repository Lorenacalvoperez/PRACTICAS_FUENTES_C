library(climaemet)
library(tibble)
library(tidyverse)
library(rjson)
library(tidyjson)
library(readr)
# Obtención de las Tª diaras por estaciones aemet (NO volver a ejecutar):

observaciones_diarias <-aemet_daily_period_all(start = 2021, end = 2022)
# nos ha dicho q a partir d estos datos usemoss una de las funciones de la url para separar por años https://lubridate.tidyverse.org/reference/index.html 
View(observaciones_diarias)

#temperatura de las provincias
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
  
  
 #TABLA FINAL Tª
tmed_CCAA<- temp_con_CCAA %>% 
  group_by(CCAA,years) %>% 
  summarise(
  tmedia = mean(tmedia, na.rm = TRUE)
)
  


# Carga csv de psicologos

library(readr)
psicologos_2021 <- read_delim("DATA/psicologos_2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

levels(factor(psicologos_2021$`Comunidades y Ciudades Autónomas`))



psicologos_2022 <- read_delim("DATA/psicologos_2022.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

levels(factor(psicologos_2022$`Comunidades y Ciudades Autónomas`))

# Psicologos completos

psicologos <- psicologos_2021 %>%
  mutate(years = 2021) %>% 
  full_join(., 
            psicologos_2022 %>%
              mutate(years = 2022) ) %>% 
  drop_na() %>% 
  filter(`Situación laboral`== "Colegiados no jubilados" & Sexo == "Total") %>% 
  group_by(`Comunidades y Ciudades Autónomas`, years) %>% 
  select(Total_ps)
# La columan de Total_ps está en formato numérico  

# Es necesario aplicar un mutate sobre psicologos_2021 para que tenga los mismos levels que psicologos 2022!!!


head(psicologos)
View(psicologos)
View(psicologos_2021)
View(psicologos_2022)



# Carga csv de visitas
library(readr)
visitas_2021 <- read_delim("DATA/visitas_2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

visitas_2022 <- read_delim("DATA/visitas_2022.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Conversión de la columna Total a formato numérico: La llamaremos Total_v
visitas_2021 <- visitas_2021 %>% 
  mutate(Total_v = as.numeric(gsub(',', '.', gsub('\\.', '', .$Total))))

visitas_2022 <- visitas_2022 %>% 
  mutate(Total_v = as.numeric(gsub(',', '.', gsub('\\.', '', .$Total))))


#VISITAS COMPLETAS

visitas<- visitas_2021%>%
  mutate(years = 2021) %>% 
  full_join(., 
            visitas_2022%>%
              mutate(years = 2022) ) %>% 
  drop_na() %>% 
  filter(`Tipo de profesional` == "Psicólogo, psicoterapeuta o psiquiatra" & 
           `Sí o no` == "Sí") %>% 
  group_by(`Comunidades y Ciudades Autónomas`, years) %>% 
  select(Total_v)



# ESTÁ CORRECTO
head(visitas)
View(visitas)
View(visitas_2022)
View(visitas_2021)

#carga de datos Población.csv
library(readr)
poblacion <- read_delim("DATA/poblacion_CA_años.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Conversión de la columna Total a formato numérico: La llamaremos Total_pob
poblacion <- poblacion %>% 
  mutate(Total_pob = as.numeric(gsub(',', '.', gsub('\\.', '', .$Total))))

poblacion_final <- poblacion %>%
  drop_na() %>% 
  rename(years=Periodo)%>% 
  filter(Sexo == "Ambos sexos" & 
           Nacionalidad == "Española" &
           years %in% c(2021,2022) &
           `Comunidades y Ciudades Autónomas` != "Total Nacional" ) %>% 
  group_by(`Comunidades y Ciudades Autónomas`,years) %>% 
  select(Total_pob) %>% 
  arrange(years)

head(poblacion_final)
View(poblacion_final)


#Carga de datos de la población json no coindiden los atributos 

poblacion <- fromJSON(file ="DATA/poblacion.json")

poblacion %>% 
  spread_all() %>% 
  gather_object() %>% 
  json_types() %>% 
  count(name, type)

poblacion_Data<- poblacion %>% 
  enter_object(Data) %>% 
  gather_array() %>% 
  spread_all() 
 

poblacion_MetaData<- poblacion %>% 
  enter_object(MetaData) %>% 
  gather_array() %>% 
  spread_all()
  

View(poblacion_MetaData)


# Carga de datos de salarios .json:
# No coinciden los atributos para hacer la unión

salarios <- fromJSON(file ="DATA/salario_CCAA_años.json")

salarios %>% 
  spread_all() %>% 
  gather_object() %>% 
  json_types() %>% 
  count(name, type)

salarios_Data<- salarios %>% 
  enter_object(Data) %>% 
  gather_array() %>% 
  spread_all() 


salarios_Metadata<- salarios %>% 
  enter_object(MetaData) %>% 
  gather_array() %>% 
  spread_all() 
  

View(salarios_Metadata)
View(salarios_Data)

# Carga de datos de salarios .csv:
library(readr)
salarios <- read_delim("DATA/salarios_CCAA.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

View(salarios)

salarios_final <- salarios %>%
  drop_na() %>% 
  rename(years = Periodo)%>% 
  filter(`Tipo de jornada` == "Total" & 
           Decil == "Total decil" &
           years %in% c(2021,2022) &
           `Comunidades y Ciudades Autonómas` != "Total Nacional" ) %>% 
  group_by(`Comunidades y Ciudades Autonómas`, years) %>% 
  select(Total_num) %>% 
  arrange(years)

View(salarios_final)



# TABLAS DE INTERÉS: 

# temp_con_CCAA
head(tmed_CCAA)
View(tmed_CCAA)

# psicologos: NO ESTÁ BIEN DEL TODO!!! MODIFICAR PARA OBTENER MISMAS CCAA!!!
head(psicologos)
View(psicologos)

# Visitas al psicologo:
head(visitas)
View(visitas)

# Población
head(poblacion_final)
View(poblacion_final)

# Salarios
head(salarios_final)
View(salarios_final)



# JULIA AL FINAL QUE HACEMOS CON ESTO???

#CARGA de datos psicologos.json 

psicologos_json_2021 <- fromJSON(file ="DATA/psicologos_2021.json")


#Identificacion de arrays
psicologos_json_2021 %>% 
  spread_all() %>% 
  gather_object() %>% 
  json_types() %>% 
  count(name, type)
#obtenemos la columna data
psicologos_json_2021_Data<- psicologos_json_2021 %>% 
  enter_object(Data) %>% 
  gather_array() %>% 
  spread_all() %>%
  select(Valor)
#obtenemos la columna metadata
psicologos_json_2021_Metadata<- psicologos_json_2021 %>% 
  enter_object(MetaData) %>% 
  gather_array() %>% 
  spread_all() %>%
  select(-document.id, - array.index)
View(visitas_Data)
#union de columnas
psicologos_json_2021_union<- cbind(psicologos_json_2021_Metadata,psicologos_json_2021_Data)
head(psicologos_json_2021_union)
View(psicologos_json_2021_union)









# Explicación para obtener las tablas de interés de forma correcta: 
#library(DT)
#datatable(iris) iris sería nuestro conjunto de datos de interés

