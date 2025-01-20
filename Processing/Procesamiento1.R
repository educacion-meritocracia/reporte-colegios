#Nombre: Benjamín Zavala
#Fecha: 18/01/2025
#Titulo Descriptivos informe colegios

#Librerias------------------------------

install.packages("pacman")

pacman::p_load(dplyr, sjPlot, stargazer, kableExtra, texreg, haven, sjlabelled,
               ggplot2, summarytools, ggtext, ggpubr, hrbrthemes, tidyr, stringr)

#Bases de datos-------------

load("input/data/original/db_proc_students.RData")

load("input/data/original/db_proc_students_w02.RData")

   W01<-db_students
   W02<-db_students_w02
#Recodificación ola1---------------

proc_datos <- W01 %>% set_na(., na = c(99, 88))

proc_datosW02 <- W02 %>% set_na(., na = c(99, 88))

for (i in names(dplyr::select(proc_datos, tidyselect::starts_with("p1_"), starts_with("p2_"), starts_with("p8_"), starts_with("p9_"), starts_with("p17_")))) {
  proc_datos[[i]] <- sjlabelled::set_labels(
    x = proc_datos[[i]],
    labels = c("Muy en desacuerdo", "Desacuerdo", "De acuerdo", "Muy de acuerdo")
  )
}

for (i in names(dplyr::select(proc_datos, tidyselect::starts_with("p1_"), starts_with("p2_"), starts_with("p8_"), starts_with("p9_"), starts_with("p17_")))) {
  proc_datosW02[[i]] <- sjlabelled::set_labels(
    x = proc_datosW02[[i]],
    labels = c("Muy en desacuerdo", "Desacuerdo", "De acuerdo", "Muy de acuerdo")
  )
}

#Graficos---------------


p1_1= round(prop.table(table(categorias=proc_datos$p1_1)),2)
p1_1 = as.data.frame(p1_1)
p1_1$categorias <- factor(p1_1$categorias, levels = c(1, 2, 3, 4), labels = c("Muy en desacuerdo", "Desacuerdo", "De acuerdo", "Muy de acuerdo"))
plot_p1_1<-ggplot(p1_1,aes(x=2,y=-Freq, fill=categorias))+
  geom_bar(stat = "identity", 
           color="white")+
  geom_text(aes(label = scales::percent(Freq)),
            position=position_stack(vjust=0.5),color="black",size=4.5)+
  coord_polar(theta = "y")+
  scale_fill_brewer(palette = "Blues")+
  theme_void()+
  theme(legend.title = element_blank(),
        plot.title = element_text(size=12, face="bold", hjust=0.5))+
  xlim(0.5,2.5) +
  labs(title="Estudiantes" )

plot_p1_1

#Función---------------


# Crear la función
graficar_dona <- function(data, columna, titulo = "Estudiantes") {
  # Calcular la tabla de frecuencias y convertir a dataframe
  tabla_frecuencia <- round(prop.table(table(categorias = data[[columna]])), 2)
  tabla_frecuencia <- as.data.frame(tabla_frecuencia)
  
  # Renombrar las categorías
  tabla_frecuencia$categorias <- factor(
    tabla_frecuencia$categorias,
    levels = c(1, 2, 3, 4),
    labels = c("Muy en desacuerdo", "Desacuerdo", "De acuerdo", "Muy de acuerdo")
  )
  
  # Crear el gráfico de dona
  plot <- ggplot(tabla_frecuencia, aes(x = 2, y = -Freq, fill = categorias)) +
    geom_bar(stat = "identity", color = "white") +
    geom_text(aes(label = scales::percent(Freq)),
              position = position_stack(vjust = 0.5), color = "black", size = 4.5) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Blues") +
    theme_void() +
    theme(legend.title = element_blank(),
          plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
    xlim(0.5, 2.5) +
    labs(title = titulo)
  
  return(plot)
}

# Graficos base OLA 1---------------

plots_list <- list()

columnas <- paste0("p1_", 1:10)

# Iterar sobre las columnas y generar los gráficos
for (col in columnas) {
  plot <- graficar_dona(proc_datos, col, "Estudiantes")
  plots_list[[col]] <- plot  # Guardar el gráfico en la lista usando el nombre de la columna
}

# Mostrar el gráfico de ejemplo, por ejemplo para "p1_1"
plots_list[["p1_5"]]

 
#Gráficos OLA 2-------------

plots_list_w02 <- list()

columnas2 <- paste0("p12_",1:10, "_o2")

# Iterar sobre las columnas y generar los gráficos
for (col in columnas2) {
  plot <- graficar_dona(proc_datosW02, col, "Estudiantes")
  plots_list_w02[[col]] <- plot  # Guardar el gráfico en la lista usando el nombre de la columna
}

# Mostrar el gráfico de ejemplo, por ejemplo para "p1_1"
plots_list_w02[["p12_o2"]]




plot2 <- graficar_dona(proc_datos, "p12_o2", "Estudiantes")
plot2

