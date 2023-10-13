# El Quantificador

# Preliminares --------------------------------------------------------------------------------------------
#Cargamos las librerías
library(tidyverse)
library(survey)
library(ggplot2)

# Cargamos las librerías
library(tidyverse)
library(survey)
library(ggplot2)

# Leer el archivo CSV
df <- read.csv("output/ab_04_21.csv", sep = ",")

# Convertir códigos especiales a NA
df$ing4[df$ing4 %in% c(888888, 988888)] <- NA

#Limpieza de valores faltantes 
df <- df %>% filter(!is.na(estratopri))

# Creación de dummies para grupo de acuerdo y en desacuerdo
df$apoyo_democracia <- ifelse(as.numeric(as.character(df$ing4)) %in% 1:4, "No apoyo", ifelse(as.numeric(as.character(df$ing4)) %in% 5:7, "Apoyo", NA))
df$apoyo_democracia <- as.factor(df$apoyo_democracia)

# Diseño Muestral de apoyo_democracia
dm <- svydesign(ids = ~ upm,
                strata = ~ estratopri, 
                weights = ~ weight1500, 
                nest = TRUE,
                na.action = 'na.exclude',
                data = df)

# Tabulación con pesos de muestra de apoyo_democracia
apoyo_tab <- svyby(formula = ~apoyo_democracia, 
                   by = ~year, 
                   design = dm,
                   FUN = svymean,
                   na.rm = T)
head(apoyo_tab)

# Convertir a dataframe regular
apoyo_df <- as.data.frame(apoyo_tab)

# Gráfico de evolución temporal del apoyo a la democracia
plot <- ggplot(apoyo_tab, aes(x = year)) +  
  geom_line(aes(y = apoyo_democraciaApoyo, color = "Apoyo")) +
  geom_point(aes(y = apoyo_democraciaApoyo, color = "Apoyo")) +
  geom_line(aes(y = `apoyo_democraciaNo apoyo`, color = "No apoyo")) +
  geom_point(aes(y = `apoyo_democraciaNo apoyo`, color = "No apoyo")) +
  labs(
    title = "Evolución del apoyo a la democracia en Ecuador (2004-2021)",
    subtitle = "Pregunta: ¿Hasta qué punto está de acuerdo o en desacuerdo con que la democracia es mejor que cualquier otra forma de gobierno?",
    y = "Porcentaje de acuerdo",
    x = "Año",
    color = "Categoría"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_article_educacion
print(plot)

