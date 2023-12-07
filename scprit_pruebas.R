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
  tmedia = mean(tmedia, na.rm = TRUE)) %>% 
  arrange(years)

View(tmed_CCAA)

levels(factor(tmed_CCAA$CCAA))
labels(factor(tmed_CCAA$CCAA))

# Carga csv de psicologos

library(readr)
psicologos_2021 <- read_delim("DATA/psicologos2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

levels(factor(psicologos_2021$`Comunidades y Ciudades Autónomas`))
labels(factor(psicologos_2021$`Comunidades y Ciudades Autónomas`))



psicologos_2022 <- read_delim("DATA/psicologos2022.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

levels(factor(psicologos_2022$`Comunidades y Ciudades Autónomas`))
labels(factor(psicologos_2021$`Comunidades y Ciudades Autónomas`))
 

# Es necesario aplicar un mutate sobre psicologos_2021 para que tenga los mismos levels que psicologos_2022!!!
psicologos_2021 <- psicologos_2021 %>% 
  mutate(CCAA = factor(`Comunidades y Ciudades Autónomas`, 
                                                     levels = c("01 Andalucía", "02 Aragón", 
                                                                "03 Asturias, Principado de", "04 Balears, Illes", 
                                                                "05 Canarias", 
                                                                "06 Cantabria", 
                                                                "07 Castilla y León", "08 Castilla - La Mancha",
                                                                "09 Cataluña", "10 Comunitat Valenciana",
                                                                "11 Extremadura", "12 Galicia", 
                                                                "13 Madrid, Comunidad de", "14 Murcia, Región de", 
                                                                "15 Navarra, Comunidad Foral de",
                                                                "16 País Vasco", "17 Rioja, La", "18 Ceuta", 
                                                                "19 Melilla"), 
         labels = c("ANDALUCIA", "ARAGON", "ASTURIAS",
                    "BALEARES", "CANARIAS", "CANTABRIA", 
                    "CASTILLA Y LEON", "CASTILLA-LA MANCHA", "CATALUÑA", 
                    "COMUNIDAD VALENCIANA", "EXTREMADURA",
                    "GALICIA", "COMUNIDAD DE MADRID", "MURCIA",
                    "NAVARRA", "PAIS VASCO", "LA RIOJA", "CEUTA", "MELILLA")))

levels(factor(psicologos_2021$CCAA))

psicologos_2022 <- psicologos_2022 %>% 
  mutate(CCAA = factor(`Comunidades y Ciudades Autónomas`, 
                                                     levels = c("Andalucía", "Aragón", 
                                                                "Asturias, Principado de", "Balears, Illes", 
                                                                "Canarias", 
                                                                "Cantabria", 
                                                                "Castilla-La Mancha", "Castilla y León",
                                                                "Cataluña", "Ceuta", "Comunidad Valenciana",
                                                                "Extremadura", "Galicia", 
                                                                "Madrid, Comunidad de", "Melilla", "Murcia, Región de",
                                                                "Navarra, Comunidad Foral de",
                                                                "País Vasco", "Rioja, La" 
                                                                ), 
                                                     labels = c("ANDALUCIA", "ARAGON", "ASTURIAS",
                                                                "BALEARES", "CANARIAS", "CANTABRIA", 
                                                                "CASTILLA-LA MANCHA", "CASTILLA Y LEON", "CATALUÑA", 
                                                                "CEUTA", "COMUNIDAD VALENCIANA", "EXTREMADURA",
                                                                "GALICIA", "COMUNIDAD DE MADRID", "MELILLA", "MURCIA",
                                                                "NAVARRA", "PAIS VASCO", "LA RIOJA")))

levels(factor(psicologos_2022$CCAA))

# Psicologos completos

psicologos <- psicologos_2021 %>%
  mutate(years = 2021) %>% 
  full_join(., 
            psicologos_2022 %>%
              mutate(years = 2022) ) %>% 
  drop_na() %>% 
  filter(`Situación laboral`== "Colegiados no jubilados") %>% 
  group_by(CCAA, years) %>%
  mutate(Total_ind_ps=Total/10^5) %>%
  select(Total_ind_ps)
# La columan de Total_ps está en formato numérico 

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


levels(factor(visitas_2021$`Comunidades y Ciudades Autónomas`))
levels(factor(visitas_2022$`Comunidades y Ciudades Autónomas`))

visitas_2021 <- visitas_2021 %>% 
  mutate(CCAA = factor(`Comunidades y Ciudades Autónomas`, 
                                                     levels = c("Andalucía", "Aragón", 
                                                                "Asturias (Principado de)", "Balears (Illes)", 
                                                                "Canarias", 
                                                                "Cantabria", 
                                                                "Castilla-La Mancha", "Castilla y León",
                                                                "Cataluña", "Ceuta (Ciudad Autónoma de)", 
                                                                "Comunitat Valenciana",
                                                                "Extremadura", "Galicia", 
                                                                "Madrid (Comunidad de)", 
                                                                "Melilla (Ciudad Autónoma de)", "Murcia (Región de)",
                                                                "Navarra (Comunidad Foral de)",
                                                                "País Vasco", "Rioja (La)" 
                                                     ), 
                                                     labels = c("ANDALUCIA", "ARAGON", "ASTURIAS",
                                                                "BALEARES", "CANARIAS", "CANTABRIA", 
                                                                "CASTILLA-LA MANCHA", "CASTILLA Y LEON", "CATALUÑA", 
                                                                "CEUTA", "COMUNIDAD VALENCIANA", "EXTREMADURA",
                                                                "GALICIA", "COMUNIDAD DE MADRID", "MELILLA", "MURCIA",
                                                                "NAVARRA", "PAIS VASCO", "LA RIOJA")))

levels(factor(visitas_2021$CCAA))

levels(factor(visitas_2022$`Comunidades y Ciudades Autónomas`))

# Es necesario aplicar un mutate sobre visitas_2022 para que tenga los mismos levels que visitas_2021!!!
visitas_2022 <- visitas_2022 %>% 
  mutate(CCAA = factor(`Comunidades y Ciudades Autónomas`, 
                             levels = c("01 Andalucía", "02 Aragón", 
                                        "03 Asturias, Principado de", "04 Balears, Illes", 
                                        "05 Canarias", 
                                        "06 Cantabria", 
                                        "07 Castilla y León", "08 Castilla - La Mancha",
                                        "09 Cataluña", "10 Comunitat Valenciana",
                                        "11 Extremadura", "12 Galicia", 
                                        "13 Madrid, Comunidad de", "14 Murcia, Región de", 
                                        "15 Navarra, Comunidad Foral de",
                                        "16 País Vasco", "17 Rioja, La", "18 Ceuta", 
                                        "19 Melilla"), 
                             labels = c("ANDALUCIA", "ARAGON", "ASTURIAS",
                                        "BALEARES", "CANARIAS", "CANTABRIA", 
                                        "CASTILLA Y LEON", "CASTILLA-LA MANCHA", "CATALUÑA", 
                                        "COMUNIDAD VALENCIANA", "EXTREMADURA",
                                        "GALICIA", "COMUNIDAD DE MADRID", "MURCIA",
                                        "NAVARRA", "PAIS VASCO", "LA RIOJA", "CEUTA", "MELILLA")))

View(visitas_2022)
levels(factor(visitas_2022$CCAA))

#VISITAS COMPLETAS

visitas<- visitas_2021%>%
  mutate(years = 2021) %>% 
  full_join(., 
            visitas_2022%>%
              mutate(years = 2022) ) %>% 
  drop_na() %>% 
  filter(`Tipo de profesional` == "Psicólogo, psicoterapeuta o psiquiatra" & 
           `Sí o no` == "Sí" & Sexo == "Ambos sexos") %>% 
  group_by(CCAA, years) %>% 
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

levels(factor(poblacion$`Comunidades y Ciudades Autónomas`))

# Es necesario aplicar un mutate sobre poblacion para que tenga los mismos levels que el resto de tablas: 
poblacion <- poblacion %>% 
  mutate(CCAA = factor(`Comunidades y Ciudades Autónomas`, 
                                               levels = c("01 Andalucía", "02 Aragón", 
                                                          "03 Asturias, Principado de", "04 Balears, Illes", 
                                                          "05 Canarias", 
                                                          "06 Cantabria", 
                                                          "07 Castilla y León", "08 Castilla - La Mancha",
                                                          "09 Cataluña", "10 Comunitat Valenciana",
                                                          "11 Extremadura", "12 Galicia", 
                                                          "13 Madrid, Comunidad de", "14 Murcia, Región de", 
                                                          "15 Navarra, Comunidad Foral de",
                                                          "16 País Vasco", "17 Rioja, La", "18 Ceuta", 
                                                          "19 Melilla", "Total Nacional"), 
                                               labels = c("ANDALUCIA", "ARAGON", "ASTURIAS",
                                                          "BALEARES", "CANARIAS", "CANTABRIA", 
                                                          "CASTILLA Y LEON", "CASTILLA-LA MANCHA", "CATALUÑA", 
                                                          "COMUNIDAD VALENCIANA", "EXTREMADURA",
                                                          "GALICIA", "COMUNIDAD DE MADRID", "MURCIA",
                                                          "NAVARRA", "PAIS VASCO", "LA RIOJA", "CEUTA", "MELILLA",
                                                          "TOTAL NACIONAL")))

levels(factor(poblacion$CCAA))


poblacion_final <- poblacion %>%
  drop_na() %>% 
  rename(years=Periodo)%>% 
  filter(Sexo == "Ambos sexos" & 
           Nacionalidad == "Total" &
           years %in% c(2021,2022) &
           CCAA != "TOTAL NACIONAL" ) %>% 
  group_by(CCAA,years) %>% 
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

levels(factor(salarios$`Comunidades y Ciudades Autonómas`))

# Es necesario aplicar un mutate sobre salarios para que tenga los mismos levels que el resto de tablas
salarios <- salarios %>% 
  mutate(CCAA = factor(`Comunidades y Ciudades Autonómas`, 
                                                 levels = c("01 Andalucía", "02 Aragón", 
                                                            "03 Asturias, Principado de", "04 Balears, Illes", 
                                                            "05 Canarias", 
                                                            "06 Cantabria", 
                                                            "07 Castilla y León", "08 Castilla - La Mancha",
                                                            "09 Cataluña", "10 Comunitat Valenciana",
                                                            "11 Extremadura", "12 Galicia", 
                                                            "13 Madrid, Comunidad de", "14 Murcia, Región de", 
                                                            "15 Navarra, Comunidad Foral de",
                                                            "16 País Vasco", "17 Rioja, La", "18 Ceuta", 
                                                            "19 Melilla", "Total Nacional"), 
                                                 labels = c("ANDALUCIA", "ARAGON", "ASTURIAS",
                                                            "BALEARES", "CANARIAS", "CANTABRIA", 
                                                            "CASTILLA Y LEON", "CASTILLA-LA MANCHA", "CATALUÑA", 
                                                            "COMUNIDAD VALENCIANA", "EXTREMADURA",
                                                            "GALICIA", "COMUNIDAD DE MADRID", "MURCIA",
                                                            "NAVARRA", "PAIS VASCO", "LA RIOJA", "CEUTA", "MELILLA",
                                                            "TOTAL NACIONAL")))

levels(factor(salarios$CCAA))

# SALARIOS FINAL

salarios_final <- salarios %>%
  drop_na() %>% 
  rename(years = Periodo)%>% 
  filter(`Tipo de jornada` == "Total" & 
           Decil == "Total decil" &
           years %in% c(2021,2022) &
           CCAA != "TOTAL NACIONAL" ) %>% 
  group_by(CCAA, years) %>% 
  select(Total_num) %>% 
  arrange(years)

View(salarios_final)



# TABLAS DE INTERÉS: 

# temp_con_CCAA
head(tmed_CCAA)
View(tmed_CCAA)

# psicologos: 
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

# OBTENCIÓN DE LA TABLA FINAL:
tabla_final<- tmed_CCAA %>% 
              full_join(.,salarios_final) %>% 
              full_join(.,psicologos) %>% 
              full_join(.,poblacion_final) %>% 
          mutate(.,Total_ps=Total_ind_ps*(Total_pob*10^3)) %>%
              full_join(.,visitas) %>% 
          mutate(.,NAT=as.integer(Total_v*10^6/(Total_ps*(Total_pob*10^3))*10^5))
#se extrepresan los vslores en millones para que los indice salgan valores con mayor sentido.
View(tabla_final)

# OBTENCIÓN DE GRÁFICOS:
## GRÁFICO DE DISPERSIÓN:
#NAT vs Temperatura media
library(ggplot2)
tabla_final %>%
          filter(.,NAT<50000) %>% 
          ggplot(.,mapping=aes(x=tmedia,y=NAT))+
          geom_point(aes(colour=factor(years),shape=factor(years)))+ 
          geom_smooth()+
          labs(x="Temperatura Media(ºC)",y="Necesidad de Atención Psicológica",
               subtitle = "Relación entre la Temperatura Media y la NAT")+
          theme_bw()

### WTF
#NAT vs salario
tabla_final %>% 
  ggplot(.,mapping=aes(x=Total_num,y=NAT))+
  geom_point(aes(colour=factor(years),shape=factor(years)))+ 
  geom_smooth()+
  labs(x="Salario",y="Necesidad de Atención Psicológica",
       subtitle = "Relación entre el salario y la NAT")+
  scale_x_continuous(limits = c(1700, 2600), breaks = seq(1700, 2600, by = 200)) +
  theme_bw()



#GRÁFICO MAPSPAIN()

# Plot with ggplot
library(ggplot2)
library(mapSpain)
library(sf)

CCAA_sf <- esp_get_ccaa()
CCAA_sf <- merge(CCAA_sf, tabla_final)
Can <- esp_get_can_box()


ggplot(CCAA_sf) +
  geom_sf(aes(fill = NAT),
          color = "grey70",
          linewidth = .3
  ) +
  geom_sf(data = Can, color = "grey70") +
  geom_sf_label(aes(label = NAT),
                fill = "white", alpha = 0.5,
                size = 3,
                label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Blues", rev = TRUE),
    n.breaks = 10,
    labels = function(x) {
      sprintf("%1.1f%%", 100 * x)
    },
    guide = guide_legend(title = "Necesidad de atención psicológica (NAT)")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))


# JULIA AL FINAL QUE HACEMOS CON ESTO???

#CARGA de datos psicologos.json 

psicologos_json_2021 <- fromJSON(file ="DATA/psicologos_2021.json")
psicologos_json_2021 <- fromJSON(file ="DATA/tasa_psicologos_2021.json")

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

