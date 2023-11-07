# El Quantificador

# Preliminares --------------------------------------------------------------------------------------------
#Cargamos las librerías
library(tidyverse)
library(survey)
library(ggplot2)

# Leer el archivo CSV
df <- read.csv("output/ab_04_21.csv", sep = ",")

#-----------------------------------------GRÁFICO 1---------------------------------

#Variable ing4

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

# Tema para gráficos de ggplot2
theme_article_corrupcion <-
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(color = "grey20"),
        plot.subtitle = element_text(color = "grey30"),
        plot.caption = element_text(color = "grey30", hjust = 0, face = 'italic'),
        legend.background = element_blank())

# Gráfico de evolución temporal del apoyo a la democracia con línea

graph1 <- ggplot(apoyo_tab, aes(x = year)) +  
  geom_line(aes(y = apoyo_democraciaApoyo, color = "Apoyo"), size = 1) +  # Línea 
  geom_point(aes(y = apoyo_democraciaApoyo, color = "Apoyo"), size = 3) +  # Puntos
  geom_errorbar(aes(ymin = apoyo_democraciaApoyo - se.apoyo_democraciaApoyo, 
                    ymax = apoyo_democraciaApoyo + se.apoyo_democraciaApoyo, 
                    color = "Apoyo"), 
                width = 0.3) +
  geom_text(aes(y = apoyo_democraciaApoyo + 0.02, label = scales::percent(apoyo_democraciaApoyo, accuracy = 1)), vjust = 0) + # Agregar porcentajes encima de los puntos
  scale_color_manual(values = c("Apoyo" = "cyan")) + # Cambiar color a cian
  labs(
    title = "Evolución del apoyo a la democracia en Ecuador (2004-2021)",
    subtitle = "Pregunta: ¿Hasta qué punto está de acuerdo o en desacuerdo con que la democracia es mejor que cualquier otra forma de gobierno?",
    y = "Porcentaje de acuerdo",
    x = "Año",
    color = "Categoría"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_article_corrupcion +
  theme(legend.position = "none")

print(graph1)

ggsave("figures/grafico_apoyo_demo.png",plot = graph1, 
       device = "png", 
       width = 12, 
       height = 8, 
       dpi = 1000)

#------------------------------------GRÁFICO 2--------------------------------------

#Variable pn4

# Convertir códigos especiales a NA
df$pn4[df$pn4 %in% c(888888, 988888)] <- NA

# Creación de dummies para las categorías 'Satisfacción' e 'Insatisfacción'
df$sati_dem <- ifelse(df$pn4 %in% c(1, 2), "Satisfacción", ifelse(df$pn4 %in% c(3, 4), "Insatisfacción", NA))
df$sati_dem <- as.factor(df$sati_dem)

# Continuar con la tabulación
satisfaccion_tab <- svyby(formula = ~sati_dem, 
                          by = ~year, 
                          design = dm,
                          FUN = svymean,
                          na.rm = TRUE,
                          keep.names = F)
head(satisfaccion_tab)

# Gráfico de barras apiladas para mostrar la evolución de la satisfacción
graph2 <- ggplot(satisfaccion_tab, aes(x = year)) +  
  geom_line(aes(y = sati_demSatisfacción, color = "Satisfacción"), size = 1) +  # Línea
  geom_point(aes(y = sati_demSatisfacción, color = "Satisfacción"), size = 3) +  # Puntos
  geom_errorbar(aes(ymin = sati_demSatisfacción - se.sati_demSatisfacción, 
                    ymax = sati_demSatisfacción + se.sati_demSatisfacción, 
                    color = "Satisfacción"), 
                width = 0.3) +
  geom_text(aes(y = sati_demSatisfacción + 0.02, label = scales::percent(sati_demSatisfacción, accuracy = 1)), vjust = 0) + # Agregar porcentajes encima de los puntos
  scale_color_manual(values = c("Satisfacción" = "green")) +  
  labs(
    title = "Evolución de la satisfacción con la democracia en Ecuador (2004-2021)",
    y = "Porcentaje de Satisfacción",
    x = "Año",
    color = "Categoría"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_article_corrupcion +
  theme(legend.position = "none")

print(graph2)

ggsave("figures/grafico_satisfacción.png",plot = graph2, 
       device = "png", 
       width = 12, 
       height = 8, 
       dpi = 1000)


#------------------------------------GRÁFICO 3--------------------------------------
# Variable anestg (2021)

# Convertir códigos especiales a NA
df$q1[df$q1 == 3] <- NA  # Suponiendo que "otro" no es relevante para el análisis
df$anestg[df$anestg %in% c(888888, 988888)] <- NA

# Creación de la variable dummy en base a los niveles de confianza
df$confianza_gob <- ifelse(df$anestg %in% 1:2, "Confía", ifelse(df$anestg %in% 3:4, "No confía", NA))
df$confianza_gob <- factor(df$confianza_gob)

df$q1 <- factor(df$q1, levels = c(1, 2), labels = c("Hombre", "Mujer"))

# Tabulación con diseño muestral
confianza_tab <- svyby(formula = ~confianza_gob, 
                       by = ~q1,  
                       design = dm,
                       FUN = svymean,
                       na.rm = TRUE)
head(confianza_tab)







