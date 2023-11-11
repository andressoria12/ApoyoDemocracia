# El Quantificador

# Preliminares --------------------------------------------------------------------------------------------
#Cargamos las librerías
library(tidyverse)
library(tidyr)
library(survey)
library(ggplot2)
library(dplyr)
library(scales)

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
theme_article_democracia <-
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
  geom_errorbar(aes(ymin = apoyo_democraciaApoyo - (1.96*se.apoyo_democraciaApoyo), 
                    ymax = apoyo_democraciaApoyo + (1.96*se.apoyo_democraciaApoyo), 
                    color = "black"), 
                width = 0.3) +
  geom_text(aes(y = apoyo_democraciaApoyo + 0.02, label = scales::percent(apoyo_democraciaApoyo, accuracy = 1)), vjust = -1.5) + # Agregar porcentajes encima de los puntos
  scale_color_manual(values = c("Apoyo" = "cyan")) + 
  labs(
    title = "Evolución del apoyo a la democracia en Ecuador (2004-2021)",
    subtitle = "Pregunta: ¿Hasta qué punto está de acuerdo o en desacuerdo con que la democracia es mejor que cualquier otra forma de gobierno?",
    y = "Porcentaje de acuerdo",
    x = "Año",
    color = "Categoría"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_article_democracia +
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

# Diseño Muestral de apoyo_democracia
dm <- svydesign(ids = ~ upm,
                strata = ~ estratopri, 
                weights = ~ weight1500, 
                nest = TRUE,
                na.action = 'na.exclude',
                data = df)

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
  geom_errorbar(aes(ymin = sati_demSatisfacción - (1.96*se.sati_demSatisfacción), 
                    ymax = sati_demSatisfacción + (1.96*se.sati_demSatisfacción), 
                    color = "black"), 
                width = 0.3) +
  geom_text(aes(y = sati_demSatisfacción + 0.02, label = scales::percent(sati_demSatisfacción, accuracy = 1)), vjust = -2) + # Agregar porcentajes encima de los puntos
  scale_color_manual(values = c("Satisfacción" = "orange")) +  
  labs(
    title = "Evolución de la satisfacción con la democracia en Ecuador (2004-2021)",
    y = "Porcentaje de Satisfacción",
    x = "Año",
    color = "Categoría"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_article_democracia +
  theme(legend.position = "none")

print(graph2)

ggsave("figures/grafico_satisfacción.png",plot = graph2, 
       device = "png", 
       width = 12, 
       height = 8, 
       dpi = 1000)


#------------------------------------GRÁFICO 3--------------------------------------
# Variable anestg (2021) y pres_aprov 

# Convertir códigos especiales a NA
df$anestg[df$anestg %in% c(888888, 988888)] <- NA
df$pres_aprov[df$pres_aprov %in% c(888888, 988888)] <- NA

#Eliminar NAs
df <- df[!is.na(df$anestg) & !is.na(df$pres_aprov), ]

# Creación de la variable dummy en base a los niveles de confianza
df$confi_gob <- ifelse(df$anestg %in% 1:2, "Confía", ifelse(df$anestg %in% 3:4, "Desconfianza", NA))
df$confi_gob <- as.factor(df$confi_gob)

# Dummies de variable m1
df$pres_apr <- ifelse(df$pres_aprov %in% 1:2, "Bueno",
                       ifelse(df$pres_aprov == 3, "Regular",
                              ifelse(df$pres_aprov %in% 4:5, "Malo", NA)))
df$pres_apr <- as.factor(df$pres_apr)


# Diseño Muestral de apoyo_democracia
dm <- svydesign(ids = ~ upm,
                strata = ~ estratopri, 
                weights = ~ weight1500, 
                nest = TRUE,
                na.action = 'na.exclude',
                data = df)

#Tabulación con diseño muestral
confianza_tab <- svyby(formula = ~confi_gob, 
                       by = ~pres_apr,  
                       design = dm,
                       FUN = svymean,
                       na.rm = TRUE)
head(confianza_tab)

# Transformar los datos a un formato largo
confianza_long <- pivot_longer(confianza_tab, 
                               cols = starts_with("confi_gob"), 
                               names_to = "Nivel_Confianza", 
                               values_to = "Porcentaje")

# Separar el nivel de confianza en dos columnas: una para la condición (Confía/Desconfianza) y otra para el error estándar
confianza_long <- confianza_long %>% 
  mutate(Condicion = ifelse(str_detect(Nivel_Confianza, "Confía"), "Confía", "Desconfianza"),
         SE = ifelse(Condicion == "Confía", se.confi_gobConfía, se.confi_gobDesconfianza))

#Columnas coincidan con tu dataframe
confianza_long$Nivel_Confianza <- factor(confianza_long$Nivel_Confianza, levels = c("confi_gobConfía", "confi_gobDesconfianza"))

# Gráfico de confianza por aprobación presidencial 
graph3 <- ggplot(confianza_long, aes(x = pres_apr, y = Porcentaje, fill = Condicion)) +
  geom_col(position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = Porcentaje - SE, ymax = Porcentaje + SE), 
                position = position_dodge(width = 0.7), width = 0.25) +
  geom_text(aes(label = scales::percent(Porcentaje, accuracy = 0.1)),
            position = position_dodge(width = 0.7), vjust = -2, size = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Confía" = "cyan", "Desconfianza" = "orange")) +
  labs(x = "Aprobación Presidencial", y = "Porcentaje", 
       fill = "Nivel de Confianza", 
       title = "Confianza en el Gobierno por Aprobación Presidencial") +
  theme_article_democracia

# Mostrar el gráfico
print(graph3)

ggsave("figures/grafico_confianza.png",plot = graph3, 
       device = "png", 
       width = 12, 
       height = 8, 
       dpi = 1000)
