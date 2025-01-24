#Nombre: Benjamin Zavala
#Fecha: 22/01/2025
#Tutulo:  Gráficos de all

#Librerias----------------------------------------------------------------------

pacman::p_load(dplyr, sjPlot, stargazer, kableExtra, texreg, haven, sjlabelled,
               ggplot2, summarytools, ggtext, ggpubr, hrbrthemes, tidyr, stringr, 
               sjmisc, ggalluvial, shadowtext)

#Data---------------------------------------------------------------------------

load("input/data/original/edumer_students_long.RData")


# codificamos los valores 99-88 como missing para todas las variables-----------
db_long <- edumer_students_long %>% set_na(., na = c(99, 88))

# iteramos la recodificación de etiquetas para cada variable y fraseo-----------


for (i in names(dplyr::select(db_long, tidyselect::starts_with("p1_"), 
                              starts_with("p2_"), starts_with("p8_"), starts_with("p9_"),
                              starts_with("p17_")))) {
  db_long[[i]] <- sjlabelled::set_labels(
    x = db_long[[i]],
    labels = c("Muy en db_longacuerdo", "db_longacuerdo", "De acuerdo", "Muy de acuerdo")
  )
}


for (i in names(dplyr::select(db_long, tidyselect::starts_with("p10_")))) {
  db_long[[i]] <- sjlabelled::set_labels(
    x = db_long[[i]],
    labels = c("Nada importante", "Algo importante", "Importante",
               "Muy importante")
  )
}


for (i in names(dplyr::select(db_long, tidyselect::starts_with("p11_")))) {
  db_long[[i]] <- sjlabelled::set_labels(
    x = db_long[[i]],
    labels = c("Seguro no haré esto", "Tal vez haré esto",
               "Probablemente haré esto", "Seguro haré esto")
  )
}

for (i in names(dplyr::select(db_long, tidyselect::starts_with("p12_")))) {
  db_long[[i]] <- sjlabelled::set_labels(
    x = db_long[[i]],
    labels = c("Si", "No")
  )
}

for (i in names(dplyr::select(db_long, tidyselect::starts_with("p13_")))) {
  db_long[[i]] <- sjlabelled::set_labels(
    x = db_long[[i]],
    labels = c("Nunca", "Una vez al año", "Una vez al mes", "Semanalmente",
               "Todos los días")
  )
}


for (i in names(dplyr::select(db_long, tidyselect::starts_with("p18_")))) {
  db_long[[i]] <- sjlabelled::set_labels(
    x = db_long[[i]],
    labels = c("Nunca", "Casi nunca", "Casi siempre", "Siempre")
  )
}


# Paso 1: Preparar datos--------------------------------------------------------
db_long <- db_long %>% filter(!is.na(p1_1) & !is.na(ola))
db_long$p1_1 <- factor(db_long$p1_1, labels= c("Muy en desacuerdo", "En desacuerdo",
                                               "De acuerdo", "Muy de acuerdo"))

# Paso 2: Calcular frecuencias-------------------------------------------------
pe <- db_long %>% 
  group_by(id_estudiante, ola) %>% 
  count(p1_1) %>% 
  group_by(ola) %>% 
  mutate(porcentaje=n/sum(n)) %>% 
  ungroup() %>% 
  na.omit() 

# Paso 3: Agregar etiquetas
etiquetas.pe <- db_long %>%
  group_by(ola, p1_1) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(ola) %>%
  mutate(porcentaje = count / sum(count)) %>% 
  na.omit() %>% 
  mutate(id_estudiante = 1)

# Paso 4: Ordenar los factores de la misma forma
pe$p1_1 <- factor(pe$p1_1)
etiquetas.pe$p1_1 <- factor(etiquetas.pe$p1_1)

# Paso 5: Definir colores de las barras
colors <- c("#f1eef6ff","#bdc9e1ff","#74a9cfff","#0570b0ff")

# Paso 6: Graficar
plot_pe <- pe %>% 
  ggplot(aes(x = ola, fill = p1_1, stratum = p1_1,
             alluvium = id_estudiante, y = porcentaje)) +
  ggalluvial::geom_flow(alpha = .66) + 
  ggalluvial::geom_stratum(linetype = 0) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = colors) +
  geom_shadowtext(data = etiquetas.pe,
                  aes(label = ifelse(porcentaje > 0, scales::percent(porcentaje, accuracy = .1), "")),
                  position = position_stack(vjust = .5),
                  show.legend = FALSE,
                  size = 4,
                  color = 'white',
                  bg.colour = 'grey30') +
  labs(y = "Porcentaje",
       x = "Ola",
       fill = "Grado de acuerdo",
       caption = "Fuente: EDUMER",
       title = "Figura: En esta escuela quienes se esfuerzan\nobtienen buenas notas") +
  theme(legend.position = "bottom") + 
  theme_blank()

plot_pe

#Función Gráficos aluvian-------------------

install.packages("ggshadowtext")

library(dplyr)
library(ggplot2)
library(ggalluvial)
library(ggshadowtext)

# Definición de la función
crear_grafico_alluvial <- function(data, variable, etiquetas, colores, titulo, fuente) {
  # Paso 1: Filtrar datos faltantes
  data <- data %>% filter(!is.na(!!sym(variable)) & !is.na(ola))
  
  # Convertir la variable en factor con etiquetas proporcionadas
  data[[variable]] <- factor(data[[variable]], labels = etiquetas)
  
  # Paso 2: Calcular frecuencias y porcentajes
  pe <- data %>% 
    group_by(id_estudiante, ola) %>% 
    count(!!sym(variable)) %>% 
    group_by(ola) %>% 
    mutate(porcentaje = n / sum(n)) %>% 
    ungroup() %>% 
    na.omit()
  
  # Paso 3: Crear etiquetas
  etiquetas.pe <- data %>%
    group_by(ola, !!sym(variable)) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(ola) %>%
    mutate(porcentaje = count / sum(count)) %>% 
    na.omit() %>% 
    mutate(id_estudiante = 1)
  
  # Paso 4: Ordenar factores
  pe[[variable]] <- factor(pe[[variable]])
  etiquetas.pe[[variable]] <- factor}
  
  #Ejecutar la función-----------------------
  
  etiquetas <- c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo")
  colores <- c("#f1eef6ff", "#bdc9e1ff", "#74a9cfff", "#0570b0ff")
  titulo <- "Figura: En esta escuela quienes se esfuerzan\nobtienen buenas notas"
  fuente <- "Fuente: EDUMER"
  
  grafico <- crear_grafico_alluvial(
    data = db_long,         # Tu base de datos
    variable = "p1_2",      # La variable de interés
    etiquetas = etiquetas,  # Etiquetas de los niveles
    colores = colores,      # Colores personalizados
    titulo = titulo,        # Título del gráfico
    fuente = fuente         # Pie de página
  )
  
  # Mostrar el gráfico
  
print(grafico)
  



