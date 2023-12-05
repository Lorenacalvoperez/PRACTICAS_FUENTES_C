# Paquetes:
library(readr)
library(tidyverse)

# Convertir las columnas con números al formato correcto
# Pseudocódigo
Datos <-
  Datos %>%
  mutate(Atributo = as.numeric(gsub(',', '.', gsub('\\.', '', .$Atributo))))

# Ejemplo Real
# Importar el archivo
Arbolesusofinal <- read_delim("Desktop/ALUMNOS/Arbolesusofinal.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

Arbolesusofinal

# Convertir las columnas con números al formato correcto

Arbolesusofinal <-
  Arbolesusofinal %>% 
  mutate(`800 - 999` = as.numeric(gsub(',', '.', gsub('\\.', '', .$`800 - 999`))))