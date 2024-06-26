# Instalar y cargar los paquetes necesarios
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readxl")) install.packages("readxl")
if (!require("writexl")) install.packages("writexl")

library(tidyverse)
library(readxl)
library(writexl)

# Leer el archivo
file_path <- "archivo.xls"
data <- read_excel(file_path)

# Ver las primeras filas del dataframe para entender su estructura
head(data)

# Usar la columna PRUEBA para extraer las respuestas
data_separated <- data %>%
  mutate(respuestas = str_split(PRUEBA, "")) %>%
  unnest_wider(respuestas, names_sep = "Q") %>%
  select(DNI, TIPO_EXAMEN, starts_with("respuestasQ"))

# Renombrar las columnas para que sean Q1, Q2, etc.
colnames(data_separated)[3:ncol(data_separated)] <- paste0("Q", 1:(ncol(data_separated)-2))

# Respuestas correctas
# Aquí se debe incluir el patrón de resultados correcto en el formato que se proporciona
respuestas_correctas <- list(
  `1` = str_split("AABCAAABCCCBABABAABACCABBBCCAABBBBCBCCCACABBCACBAA", "")[[1]],
  `2` = str_split("ABACABBAACCCBAABBAACACBBBCBCCABCABBCBCCAACACBBBBAA", "")[[1]]
)

# Función para contar correctas, incorrectas y en blanco
contar_respuestas <- function(respuestas, correctas) {
  correctas_count <- sum(respuestas == correctas)
  incorrectas_count <- sum(respuestas != correctas & respuestas != ".")
  en_blanco_count <- sum(respuestas == ".")
  return(tibble(correctas = correctas_count, incorrectas = incorrectas_count, en_blanco = en_blanco_count))
}

# Aplicar la función para cada fila
resultados <- data_separated %>%
  rowwise() %>%
  mutate(
    correccion = list(contar_respuestas(c_across(Q1:Q50), respuestas_correctas[[as.character(TIPO_EXAMEN)]]))
  ) %>%
  unnest(correccion)

# Resultado para la nota segun la escala original y sobre 10

resultados <- resultados %>% 
  mutate(NOTA_50 = correctas-(0.5*incorrectas), NOTA_10 = NOTA_50/5  )


# Ver el dataframe resultante con las correcciones
head(resultados)



# Reordenar las columnas para que las Q1, Q2, etc. estén al final
columnas_fijas <- c("DNI", "TIPO_EXAMEN", "correctas", "incorrectas", "en_blanco", "NOTA_50", "NOTA_10")
columnas_respuestas <- setdiff(colnames(resultados), columnas_fijas)
resultados <- resultados %>%
  select(all_of(columnas_fijas), all_of(columnas_respuestas))


# Guardar el dataframe en un archivo Excel
write_xlsx(resultados, "examenes_corregidos.xlsx")
head(resultados)

# Guardar el dataframe en un archivo Excel
write_xlsx(resultados, "examenes_corregidos.xlsx")
