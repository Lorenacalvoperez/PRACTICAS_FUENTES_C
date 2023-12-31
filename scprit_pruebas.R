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

poblacion_json <- fromJSON(file ="DATA/poblacion.json")

poblacion_json %>% 
  spread_all() %>% 
  gather_object() %>% 
  json_types() %>% 
  count(name, type)

poblacion_Data<- poblacion_json %>% 
  enter_object(Data) %>% 
  gather_array() %>% 
  spread_all() 
 

poblacion_MetaData<- poblacion_json %>% 
  enter_object(MetaData) %>% 
  gather_array() %>% 
  spread_all()
  

View(poblacion_MetaData)


# Carga de datos de salarios .json:
# No coinciden los atributos para hacer la unión

salarios_json <- fromJSON(file ="DATA/salario_CCAA_años.json")

salarios_json %>% 
  spread_all() %>% 
  gather_object() %>% 
  json_types() %>% 
  count(name, type)

salarios_Data<- salarios_json %>% 
  enter_object(Data) %>% 
  gather_array() %>% 
  spread_all() 


salarios_Metadata<- salarios_json %>% 
  enter_object(MetaData) %>% 
  gather_array() %>% 
  spread_all() 
  

View(salarios_Metadata)
View(salarios_Data)

# Carga de datos de salarios .csv:
library(readr)
salarios <- read_delim("DATA/salarios_CCAA.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
salarios <- salarios %>% 
  mutate(Total_s = as.numeric(gsub(',', '.', gsub('\\.', '', .$Total))))
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
  select(Total_s) %>% 
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
          mutate(.,NAT=as.integer(Total_v*10^6/(Total_ps*(Total_pob*10^3))*10^5)) %>% 
          mutate(.,iniciales_CCAA = factor(CCAA,
                                   levels=c("ANDALUCIA", "ARAGON", "ASTURIAS",
                                            "BALEARES", "CANARIAS", "CANTABRIA", 
                                            "CASTILLA Y LEON", "CASTILLA-LA MANCHA", "CATALUÑA", 
                                            "COMUNIDAD VALENCIANA", "EXTREMADURA",
                                            "GALICIA", "COMUNIDAD DE MADRID", "MURCIA",
                                            "NAVARRA", "PAIS VASCO", "LA RIOJA", "CEUTA", "MELILLA"), 
                                   labels = c("An","Ar","As","Bl","Cs","Ca","CyL","Cm","Ct","Cv","Ex",
                                              "Ga","M","Mu","Na","Pv","R","Ce","Ml" ))) %>% 
          select(.,iniciales_CCAA,everything())
          
#se extrepresan los vslores en millones para que los indice salgan valores con mayor sentido.

View(tabla_final)
head(tabla_final)



# OBTENCIÓN DE GRÁFICOS:
## GRÁFICO DE DISPERSIÓN:
#NAT vs Temperatura media
library(ggplot2)

NAT_vs_tmedia <- tabla_final %>%
          filter(.,NAT<50000) %>% 
          ggplot(.,mapping=aes(x=tmedia,y=NAT))+
          geom_point(aes(colour=factor(years),shape=factor(years)))+ 
          geom_smooth()+
          labs(x="Temperatura Media(ºC)",y="Necesidad de Atención Psicológica",
               subtitle = "Relación entre la Temperatura Media y la NAT")+
          theme_bw()


#NAT vs salario
NAT_vs_salario <- tabla_final %>% 
  filter(.,NAT<50000) %>% 
  ggplot(.,mapping=aes(x=Total_s,y=NAT))+
  geom_point(aes(colour=factor(years),shape=factor(years)))+ 
  geom_smooth()+
  labs(x="Salario",y="Necesidad de Atención Psicológica",
       subtitle = "Relación entre el salario y la NAT")+
  theme_bw()

#GRÁFICOS DE BARRAS
#introduccion

#SALARIO 2021
# Plot with ggplot
library(ggplot2)
library(mapSpain)
library(sf)

Salario_2021 <- tabla_final %>% 
  filter(years == 2021) %>% 
  mutate(.,ccaa.shortname.es = factor(CCAA,
                                      levels=c("ANDALUCIA", "ARAGON", "ASTURIAS",
                                               "BALEARES", "CANARIAS", "CANTABRIA", 
                                               "CASTILLA Y LEON", "CASTILLA-LA MANCHA", "CATALUÑA", 
                                               "COMUNIDAD VALENCIANA", "EXTREMADURA",
                                               "GALICIA", "COMUNIDAD DE MADRID", "MURCIA",
                                               "NAVARRA", "PAIS VASCO", "LA RIOJA", "CEUTA", "MELILLA"), 
                                      labels = c("Andalucía", "Aragón", 
                                                 "Asturias", "Baleares ", 
                                                 "Canarias", 
                                                 "Cantabria", 
                                                 "Castilla y León","Castilla-La Mancha",
                                                 "Cataluña", "Comunidad Valenciana",
                                                 "Extremadura", "Galicia", 
                                                 "Madrid", "Murcia",
                                                 "Navarra",
                                                 "País Vasco", "La Rioja" ,"Ceuta","Melilla"
                                      )))


CCAA_sf <- esp_get_ccaa()
CCAA_sf <- merge(CCAA_sf, Salario_2021, by = "ccaa.shortname.es")
Can <- esp_get_can_box()

X11()
Salario_21_graf <- ggplot(CCAA_sf) +
  geom_sf(aes(fill = Total_s),
          color = "black",
          linewidth = .3
  ) +
  geom_sf(data = Can, color = "black") +
  geom_sf_label(aes(label = Total_s),
                fill = "white", alpha = 0.5,
                size = 3,
                label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(7, "Greens", rev = TRUE),
    n.breaks = 7,
    labels = function(x) sprintf("%1.0f", x),
    limits = c(min(CCAA_sf$Total_s), 3000),
    guide = guide_legend(title = "Salario 2021")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))

#Salario 2022
library(ggplot2)
library(mapSpain)
library(sf)
Salario_2022 <- tabla_final %>% 
  filter(years == 2022) %>% 
  mutate(.,ccaa.shortname.es = factor(CCAA,
                                      levels=c("ANDALUCIA", "ARAGON", "ASTURIAS",
                                               "BALEARES", "CANARIAS", "CANTABRIA", 
                                               "CASTILLA Y LEON", "CASTILLA-LA MANCHA", "CATALUÑA", 
                                               "COMUNIDAD VALENCIANA", "EXTREMADURA",
                                               "GALICIA", "COMUNIDAD DE MADRID", "MURCIA",
                                               "NAVARRA", "PAIS VASCO", "LA RIOJA", "CEUTA", "MELILLA"), 
                                      labels = c("Andalucía", "Aragón", 
                                                 "Asturias", "Baleares ", 
                                                 "Canarias", 
                                                 "Cantabria", 
                                                 "Castilla y León","Castilla-La Mancha",
                                                 "Cataluña", "Comunidad Valenciana",
                                                 "Extremadura", "Galicia", 
                                                 "Madrid", "Murcia",
                                                 "Navarra",
                                                 "País Vasco", "La Rioja" ,"Ceuta","Melilla"
                                      )))
CCAA_sf <- esp_get_ccaa()
CCAA_sf <- merge(CCAA_sf, Salario_2022, by = "ccaa.shortname.es")
Can <- esp_get_can_box()
X11()
Salario_22_graf <- ggplot(CCAA_sf) +
  geom_sf(aes(fill = Total_s),
          color = "black",
          linewidth = .3
  ) +
  geom_sf(data = Can, color = "black") +
  geom_sf_label(aes(label = Total_s),
                fill = "white", alpha = 0.5,
                size = 3,
                label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(7, "Purples", rev = TRUE),
    n.breaks = 7,
    labels = function(x) sprintf("%1.0f", x),
    limits = c(min(CCAA_sf$Total_s), 3000),
    guide = guide_legend(title = "Salario 2022")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))


# GRaficos combinados:
library(cowplot)
X11()
Salario_21_22 <- plot_grid(Salario_21_graf, Salario_22_graf, ncol = 2, labels = c("SALARIO 2021", "SALARIO 2022") )


# Temperatura 
#Temperatura 2021
# Plot with ggplot
library(ggplot2)
library(mapSpain)
library(sf)

Temperatura_2021 <- tabla_final %>% 
  filter(years == 2021) %>% 
  mutate(.,ccaa.shortname.es = factor(CCAA,
                                      levels=c("ANDALUCIA", "ARAGON", "ASTURIAS",
                                               "BALEARES", "CANARIAS", "CANTABRIA", 
                                               "CASTILLA Y LEON", "CASTILLA-LA MANCHA", "CATALUÑA", 
                                               "COMUNIDAD VALENCIANA", "EXTREMADURA",
                                               "GALICIA", "COMUNIDAD DE MADRID", "MURCIA",
                                               "NAVARRA", "PAIS VASCO", "LA RIOJA", "CEUTA", "MELILLA"), 
                                      labels = c("Andalucía", "Aragón", 
                                                 "Asturias", "Baleares ", 
                                                 "Canarias", 
                                                 "Cantabria", 
                                                 "Castilla y León","Castilla-La Mancha",
                                                 "Cataluña", "Comunidad Valenciana",
                                                 "Extremadura", "Galicia", 
                                                 "Madrid", "Murcia",
                                                 "Navarra",
                                                 "País Vasco", "La Rioja" ,"Ceuta","Melilla"
                                      ))) %>% 
  mutate(., tmedia = round(tmedia, 3))


CCAA_sf <- esp_get_ccaa()
CCAA_sf <- merge(CCAA_sf, Temperatura_2021, by = "ccaa.shortname.es")
Can <- esp_get_can_box()

X11()
Temperatura_21_graf <- ggplot(CCAA_sf) +
  geom_sf(aes(fill = tmedia),
          color = "black",
          linewidth = .3
  ) +
  geom_sf(data = Can, color = "black") +
  geom_sf_label(aes(label = tmedia),
                fill = "white", alpha = 0.5,
                size = 3,
                label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(7, "Oranges", rev = TRUE),
    n.breaks = 7,
    labels = function(x) sprintf("%1.0f", x),
    limits = c(min(CCAA_sf$tmedia), 20),
    guide = guide_legend(title = "Temperatura 2021")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))

#Temperatura 2022
library(ggplot2)
library(mapSpain)
library(sf)

Temperatura_2022 <- tabla_final %>% 
  filter(years == 2022) %>% 
  mutate(.,ccaa.shortname.es = factor(CCAA,
                                      levels=c("ANDALUCIA", "ARAGON", "ASTURIAS",
                                               "BALEARES", "CANARIAS", "CANTABRIA", 
                                               "CASTILLA Y LEON", "CASTILLA-LA MANCHA", "CATALUÑA", 
                                               "COMUNIDAD VALENCIANA", "EXTREMADURA",
                                               "GALICIA", "COMUNIDAD DE MADRID", "MURCIA",
                                               "NAVARRA", "PAIS VASCO", "LA RIOJA", "CEUTA", "MELILLA"), 
                                      labels = c("Andalucía", "Aragón", 
                                                 "Asturias", "Baleares ", 
                                                 "Canarias", 
                                                 "Cantabria", 
                                                 "Castilla y León","Castilla-La Mancha",
                                                 "Cataluña", "Comunidad Valenciana",
                                                 "Extremadura", "Galicia", 
                                                 "Madrid", "Murcia",
                                                 "Navarra",
                                                 "País Vasco", "La Rioja" ,"Ceuta","Melilla"
                                      ))) %>% 
  mutate(., tmedia = round(tmedia, 3))

CCAA_sf <- esp_get_ccaa()
CCAA_sf <- merge(CCAA_sf, Temperatura_2022, by = "ccaa.shortname.es")
Can <- esp_get_can_box()
X11()
Temperatura_22_graf <- ggplot(CCAA_sf) +
  geom_sf(aes(fill = tmedia),
          color = "black",
          linewidth = .3
  ) +
  geom_sf(data = Can, color = "black") +
  geom_sf_label(aes(label = tmedia),
                fill = "white", alpha = 0.5,
                size = 3,
                label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(7, "Reds", rev = TRUE),
    n.breaks = 7,
    labels = function(x) sprintf("%1.0f", x),
    limits = c(min(CCAA_sf$tmedia), 20),
    guide = guide_legend(title = "Temperatura 2022")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))


# GRaficos combinados:
library(cowplot)
X11()
Temperatura_21_22 <- plot_grid(Temperatura_21_graf, Temperatura_22_graf, ncol = 2, labels = c("TEMPERATURA 2021", "TEMPERATURA 2022") )




# SALARIO VS CCAA
Salario_vs_CCAA <- tabla_final %>% 
  ggplot(., aes(x = iniciales_CCAA, y = Total_s,fill= factor(years))) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Salario por Comunidad Autónoma",
    x = "Comunidad Autónoma",
    y = "Salario"
  )+
  theme_replace()+
  facet_wrap(facets = vars(years), nrow = 1)
  
# TEMPERATURA MEDIA VS CCAA
tmedia_vs_CCAA <- tabla_final %>% 
  ggplot(., aes(x = iniciales_CCAA, y = Total_s,fill= factor(years))) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "NAT por Comunidad Autónoma",
    x = "Comunidad Autónoma",
    y = "Temperatura Media"
  )+
  theme_replace()+
  facet_wrap(facets = vars(years), nrow = 1)

## NAT VS CCAA
NAT_vs_CCAA <- tabla_final %>% 
  ggplot(., aes(x = reorder(iniciales_CCAA, desc(NAT)), y = NAT, fill = factor(years))) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "NAT por Comunidad Autónoma",
    x = "Comunidad Autónoma",
    y = "Necesidad de Atención Psicológica"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(facets = vars(years), nrow = 1)


# Análisis de regresión final:
library(visreg)
Modelo <- lm(NAT ~ tmedia*Total_s, data = tabla_final)
summary(Modelo)

# Visualización 3D para ver la relación entre Tª media y el total de salario:
visor_3d <- visreg2d(Modelo, "tmedia", "Total_s", plot.type = "rgl")
visreg2d(Modelo, "tmedia", "Total_s", plot.type = "persp")




#GRÁFICO MAPSPAIN()

# Plot with ggplot
library(ggplot2)
library(mapSpain)
library(sf)

NAT_2021 <- tabla_final %>% 
  filter(years == 2021) %>% 
  mutate(.,ccaa.shortname.es = factor(CCAA,
                                 levels=c("ANDALUCIA", "ARAGON", "ASTURIAS",
                                          "BALEARES", "CANARIAS", "CANTABRIA", 
                                          "CASTILLA Y LEON", "CASTILLA-LA MANCHA", "CATALUÑA", 
                                          "COMUNIDAD VALENCIANA", "EXTREMADURA",
                                          "GALICIA", "COMUNIDAD DE MADRID", "MURCIA",
                                          "NAVARRA", "PAIS VASCO", "LA RIOJA", "CEUTA", "MELILLA"), 
                                 labels = c("Andalucía", "Aragón", 
                                            "Asturias", "Baleares ", 
                                            "Canarias", 
                                            "Cantabria", 
                                            "Castilla y León","Castilla-La Mancha",
                                            "Cataluña", "Comunidad Valenciana",
                                            "Extremadura", "Galicia", 
                                            "Madrid", "Murcia",
                                            "Navarra",
                                            "País Vasco", "La Rioja" ,"Ceuta","Melilla"
                                )))


CCAA_sf <- esp_get_ccaa()
CCAA_sf <- merge(CCAA_sf, NAT_2021, by = "ccaa.shortname.es")
Can <- esp_get_can_box()

X11()
NAT_21_graf <- ggplot(CCAA_sf) +
  geom_sf(aes(fill = NAT),
          color = "black",
          linewidth = .3
  ) +
  geom_sf(data = Can, color = "black") +
  geom_sf_label(aes(label = NAT),
                fill = "white", alpha = 0.5,
                size = 3,
                label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(7, "Blues", rev = TRUE),
    n.breaks = 7,
    labels = function(x) sprintf("%1.0f", x),
    limits = c(min(CCAA_sf$NAT), 13000),
    guide = guide_legend(title = "NAT 2021")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))

#NAT 2022
library(ggplot2)
library(mapSpain)
library(sf)
NAT_2022 <- tabla_final %>% 
  filter(years == 2022) %>% 
  mutate(.,ccaa.shortname.es = factor(CCAA,
                                      levels=c("ANDALUCIA", "ARAGON", "ASTURIAS",
                                               "BALEARES", "CANARIAS", "CANTABRIA", 
                                               "CASTILLA Y LEON", "CASTILLA-LA MANCHA", "CATALUÑA", 
                                               "COMUNIDAD VALENCIANA", "EXTREMADURA",
                                               "GALICIA", "COMUNIDAD DE MADRID", "MURCIA",
                                               "NAVARRA", "PAIS VASCO", "LA RIOJA", "CEUTA", "MELILLA"), 
                                      labels = c("Andalucía", "Aragón", 
                                                 "Asturias", "Baleares ", 
                                                 "Canarias", 
                                                 "Cantabria", 
                                                 "Castilla y León","Castilla-La Mancha",
                                                 "Cataluña", "Comunidad Valenciana",
                                                 "Extremadura", "Galicia", 
                                                 "Madrid", "Murcia",
                                                 "Navarra",
                                                 "País Vasco", "La Rioja" ,"Ceuta","Melilla"
                                      )))
CCAA_sf <- esp_get_ccaa()
CCAA_sf <- merge(CCAA_sf, NAT_2022, by = "ccaa.shortname.es")
Can <- esp_get_can_box()
X11()
NAT_22_graf <- ggplot(CCAA_sf) +
  geom_sf(aes(fill = NAT),
          color = "black",
          linewidth = .3
  ) +
  geom_sf(data = Can, color = "black") +
  geom_sf_label(aes(label = NAT),
                fill = "white", alpha = 0.5,
                size = 3,
                label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(7, "Reds", rev = TRUE),
    n.breaks = 7,
    labels = function(x) sprintf("%1.0f", x),
    limits = c(min(CCAA_sf$NAT), 5000),
    guide = guide_legend(title = "NAT 2022")
  ) +
  theme_void() +
  theme(legend.position = c(0.1, 0.6))


# GRaficos combinados:
library(cowplot)
X11()
NAT_21_22 <- plot_grid(NAT_21_graf, NAT_22_graf, ncol = 2, labels = c("NAT 2021", "NAT 2022") )

# Prueba para que aparezcan mejor:

plot_grid(NAT_21_graf, NAT_22_graf, ncol = 2, align = 'v', axis = 'tb', labels = c("NAT 2021", "NAT 2022"))


# JULIA AL FINAL QUE HACEMOS CON ESTO???

#CARGA de datos psicologos.json 

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

