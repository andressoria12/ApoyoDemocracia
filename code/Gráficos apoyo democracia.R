# El Quantificador

# Preliminares --------------------------------------------------------------------------------------------
#Cargamos las librerías
library(tidyverse)
library(tidyr)
library(survey)
library(ggplot2)
library(dplyr)
library(scales)
library(stringr)
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

caption_graph1 <- "Fuente: El Barómetro de las Américas por el Proyecto de Opinión Pública de América Latina (LAPOP), www.LapopSurveys.org.La pregunta se basó en la afirmación 'La democracia puede tener problemas, pero es mejor que cualquier otra forma de gobierno'. Se utilizó una escala de 1 a 7, donde 1 significa 'muy en desacuerdo' y 7 'muy de acuerdo'. Para el análisis, se categorizaron las respuestas en 'no apoyo' (1-4) y 'apoyo' (5-7) a la democracia. Este gráfico refleja el porcentaje de personas mayores a 16 años que expresaron su apoyo, con intervalos de confianza del 95% y ajustes por diseño muestral multietapa estratificado."

graph1 <- ggplot(apoyo_tab, aes(x = year)) +  
  geom_line(aes(y = apoyo_democraciaApoyo, color = "Apoyo"), size = 1) +  # Línea 
  geom_point(aes(y = apoyo_democraciaApoyo, color = "Apoyo"), size = 3) +  # Puntos
  geom_errorbar(aes(ymin = apoyo_democraciaApoyo - (1.96*se.apoyo_democraciaApoyo), 
                    ymax = apoyo_democraciaApoyo + (1.96*se.apoyo_democraciaApoyo), 
                    color = "black"), 
                width = 0.3) +
  geom_text(aes(y = apoyo_democraciaApoyo + 0.05, label = scales::percent(apoyo_democraciaApoyo, accuracy = 1)), vjust = -1.5) + # Agregar porcentajes encima de los puntos
  scale_color_manual(values = c("Apoyo" = "cyan")) + 
  labs(
    title = "Evolución del apoyo a la democracia en Ecuador (2004-2021)",
    subtitle = str_wrap("¿Hasta qué punto está de acuerdo o en desacuerdo con que la democracia es mejor que cualquier otra forma de gobierno?", 80),
    y = "% de personas mayores a 16 que apoyan",
    x = NULL,
    color = "Categoría",
    caption = str_wrap(caption_graph1, 205)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_article_democracia +
  scale_x_continuous(breaks = unique(apoyo_tab$year)) +
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

caption_graph2 <- "Fuente: El Barómetro de las Américas por el Proyecto de Opinión Pública de América Latina (LAPOP), www.LapopSurveys.org.La pregunta evaluó la satisfacción con la democracia en Ecuador con opciones que iban desde 'muy satisfecho(a)' a 'muy insatisfecho(a)'. Se empleó una escala de 4 puntos, donde 1 representa 'muy satisfecho' y 4 'muy insatisfecho'. Para simplificar el análisis, se generaron variables dummies, agrupando las respuestas en dos categorías: 'satisfacción' (1 y 2) e 'insatisfacción' (3 y 4) con el funcionamiento de la democracia en el país. Este gráfico muestra el porcentaje de personas que expresaron satisfacción o insatisfacción, con intervalos de confianza del 95% ajustados por el diseño muestral."

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
    subtitle = str_wrap("En general, ¿usted diría que está muy satisfecho(a), satisfecho(a), insatisfecho(a) o muy insatisfecho(a) con la forma en que la democracia funciona en Ecuador?", 80),
    y = "% de personas mayores a 16 satisfechas",
    x = NULL,
    color = "Categoría",
    caption = str_wrap(caption_graph2, 205)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme_article_democracia +
  scale_x_continuous(breaks = unique(apoyo_tab$year)) +
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

caption_graph3 <- "Fuente: El Barómetro de las Américas por el Proyecto de Opinión Pública de América Latina (LAPOP), www.LapopSurveys.org. Este gráfico combina datos de dos preguntas. La primera, medida por la variable anestg, indaga sobre la confianza en que el gobierno nacional actúa correctamente, utilizando una escala de 1 a 4. Se crearon variables dummies para clasificar las respuestas en 'confianza' (1-2) y 'desconfianza' (3-4). La segunda pregunta, asociada a la variable presaprov y codificada como m1, evalúa la percepción del desempeño del Presidente Guillermo Lasso con opciones de respuesta en una escala de 5 puntos. Para el análisis, se categorizaron estas respuestas en tres niveles con dummies: 'bueno' (1-2), 'regular' (3) y 'malo' (4-5), reflejando la percepción sobre la administración de Lasso. Los intervalos de confianza del 95% están ajustados por diseño muestral"

graph3 <- ggplot(confianza_long, aes(x = pres_apr, y = Porcentaje, fill = Condicion)) +
  geom_col(position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = Porcentaje - SE, ymax = Porcentaje + SE), 
                position = position_dodge(width = 0.7), width = 0.25) +
  geom_text(aes(label = scales::percent(Porcentaje, accuracy = 0.1)),
            position = position_dodge(width = 0.7), vjust = -3, size = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Confía" = "cyan", "Desconfianza" = "orange")) +
  labs(x = "Aprobación del trabajo presidencial", y = "% de confianza en personas mayores a 16", 
       fill = "Nivel de Confianza", 
       title = "Aprobación presidencial y confianza gubernamental. Gobierno Ecuatoriano (2021)",
       subtitle = str_wrap("¿Qué tanto confía en que el gobierno nacional hace lo correcto? y hablando en general acerca del gobierno actual, ¿diría usted que el trabajo que está realizando el Presidente Guillermo Lasso es...?", 80), 
       caption = str_wrap(caption_graph3, 205)) +  
  theme_article_democracia

# Mostrar el gráfico
print(graph3)

ggsave("figures/grafico_confianza.png",plot = graph3, 
       device = "png", 
       width = 12, 
       height = 8, 
       dpi = 1000)
