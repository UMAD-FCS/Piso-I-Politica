
## **************************************************************************
## Funciones para  para Piso I - política
## **************************************************************************

library(tidyverse)
library(Boreluy)


# Función para asignar color a cada Partido en gráficos  ====================
coloriza_partidos <- function(vector) {
  levs <- if(is.factor(vector)) 
    levels(vector) 
  else 
    levels(factor(vector))
  
  predefinidos <- c("FA", "PN", "PC", "CA", "PI", "UP", "Cifra", "Equipos", 
                    "Factum", "Interconsult", "Opcion", "Radar", "OtrosP.", "VB/VA",
                    "Frente Amplio", "Partido Nacional", "Partido Colorado", "Cabildo Abierto", 
                    "Voto en Blanco", "Voto Anulado", "Nuevo Espacio", "Partido de la Concertacion",
                    "Frente Izquierda de Liberacion", "Partido Democrata Cristiano", "Union Civica", "Partido Socialista",
                    "Partido Independiente", "Unidad Popular", "Voto Blanco/Anulado", "Otros Partidos",
                    "Aprueba", "Desaprueba", "Ni aprueba ni desaprueba", "NS/NC", "Saldo",
                    "Sanguinetti 2", "Lacalle", "Batlle", "Vazquez 1", "Mujica", "Vazquez 2", "Lacalle Pou")
  
  pal <- c("#013197", "#99ccff", "#BA0200", "#F8BC1E", "#663399", "#00913C", "#4C9ED1", 
           "#5C8DCC", "#737AC0", "#8865AC", "#984E91", "#9F3770", "grey35", "grey75",
           "#013197", "#99ccff", "#BA0200", "#F8BC1E", 
           "grey35", "grey75", "#2CA015", "#117A65",
           "#922B21", "#E67E22", "#1F618D", "#239B56",
           "#663399", "#00913C", "gray65", "gray35",
           "#4c9ed1", "#BA0200", "#808080", "#D3D3D3", "#4c9ed1",
           "#BA0200", "#4c9ed1", "#BA0200", "#013197", "#013197", "#013197", "#4c9ed1")
  
  pal <- pal[match(levs, predefinidos)]
  
  blanks <- which(is.na(pal))
  
  pal[blanks] <- sample(colours(100), length(blanks))
  
  pal
}


# Tema personalizado de ggplot2  ============================================
theme_bdd <- function(base_family = "Titillium Web", base_size = 14) { ggplot2::theme_minimal(base_family = base_family, base_size = base_size)}


## ELECCIONES NACIONALES ====================================================

# Función para grafico votos por partido
plot_nacional <- function(anio = 2019) {
  
  suppressPackageStartupMessages(library(tidyverse))
  
  # Toma data de Boreluy
  dat_plot <- Boreluy::nacional_uy(eleccion = anio,
                                   tipo = "Presidencial") %>%
    mutate(Porcentaje = round(Porcentaje, 1)) %>% 
    Boreluy::agrupar_partidos_uy() %>% 
    mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
    mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                            "Otros Partidos")))  
  
  # Asignar color por partido
  color_partido <- dat_plot %>% 
    distinct(Partido) %>% 
    pull() %>% 
    coloriza_partidos()
  
  # Grafico
  plot <- ggplot(data =  dat_plot, 
                 aes(x = Partido,
                     y = Porcentaje, 
                     fill = Partido)) +
    geom_bar(stat = "identity",
             color = "black",
             alpha = .7) +
    coord_flip() + 
    geom_text(
      aes(label = paste0(Porcentaje, "%")),
      stat = "identity",
      hjust = -.5,
      size = 3
    ) + 
    ylim(0, max(dat_plot$Porcentaje) + 5) +
    scale_fill_manual(values = color_partido) +
    theme_bdd() +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "none") +
    labs(x = "",
         y = "",
         title = paste("Porcentaje de votos en la elección de", anio, "por partido"),
         caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 
  
  print(plot)
}

# Función para tabla votos por partido
tab_nacional <- function(anio = 2019) {
  
  suppressPackageStartupMessages(library(tidyverse))
  
  # Toma data de Boreluy
  dat_plot <- Boreluy::nacional_uy(eleccion = anio,
                                   tipo = "Presidencial") %>%
    mutate(Porcentaje = round(Porcentaje, 1)) %>% 
    Boreluy::agrupar_partidos_uy() %>% 
    mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
    mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                            "Otros Partidos"))) %>% 
    select(Partido, Votos, Porcentaje)
}

# Función para grafico votos por partido y departamento  
plot_nacional_xpto <- function(anio = 2019) {
  
  suppressPackageStartupMessages(library(tidyverse))
  
  # Toma data de Boreluy
  dat_plot <- Boreluy::nacional_uy(eleccion = anio,
                                   tipo = "Presidencial",
                                   por_departamento = TRUE) %>%
    mutate(Porcentaje = round(Porcentaje, 1)) %>% 
    mutate(Partido = ifelse(Porcentaje <= 5, "Otros Partidos", Partido),
           Sigla = ifelse(Porcentaje <= 5, "OtrosP.", Sigla)) %>%
    group_by(Partido, Departamento) %>% 
    summarise(Porcentaje = sum(Porcentaje),
              Votos = sum(Votos),
              Sigla = first(Sigla),
              Fecha = first(Fecha),
              Eleccion = first(Eleccion))%>% 
    ungroup() %>% 
    mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
    mutate(Partido = fct_relevel(Partido, "Otros Partidos")) %>% 
    arrange(desc(Porcentaje))
  
  # Asignar color por partido
  color_partido <- dat_plot %>% 
    distinct(Partido) %>% 
    pull() %>% 
    coloriza_partidos()
  
  # Grafico
  plot <- ggplot(data =  dat_plot, 
                 aes(x = Partido,
                     y = Porcentaje, 
                     fill = Partido)) +
    geom_bar(stat = "identity",
             color = "black",
             alpha = .7) +
    coord_flip() + 
    geom_text(aes(label = paste0(Porcentaje, "%")),
                  stat = "identity",
                  hjust = -.5,
                  size = 3) + 
    ylim(0, max(dat_plot$Porcentaje) + 10) +
    scale_fill_manual(values = color_partido) +
    theme_bdd() +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "none") +
    facet_wrap( ~ Departamento, ncol = 4) +
    labs(x = "",
         y = "",
         title = paste("Porcentaje de votos en la elección de", anio, "por partido y departamento"),
         caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 
  
  print(plot)
}

# Función para tabla votos por partido y departamento  
tab_nacional_xpto <- function(anio = 2019) {
  
  suppressPackageStartupMessages(library(tidyverse))
  
  # Toma data de Boreluy
  dat_plot <- Boreluy::nacional_uy(eleccion = anio,
                                   tipo = "Presidencial",
                                   por_departamento = TRUE) %>%
    mutate(Porcentaje = round(Porcentaje, 1)) %>% 
    mutate(Partido = ifelse(Porcentaje <= 5, "Otros Partidos", Partido),
           Sigla = ifelse(Porcentaje <= 5, "OtrosP.", Sigla)) %>%
    group_by(Partido, Departamento) %>% 
    summarise(Porcentaje = sum(Porcentaje),
              Votos = sum(Votos),
              Sigla = first(Sigla),
              Fecha = first(Fecha),
              Eleccion = first(Eleccion))%>% 
    ungroup() %>% 
    mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
    mutate(Partido = fct_relevel(Partido, "Otros Partidos")) %>% 
    arrange(desc(Porcentaje)) %>% 
    select(Departamento, Partido, Votos, Porcentaje)
  
}

## ELECCIONES DEPARTAMENTALES  ==============================================

# Función para grafico votos por partido y departamento  
plot_dptal <- function(anio = 2015) {
  
  suppressPackageStartupMessages(library(tidyverse))
  
  # Toma data de Boreluy
  dat_plot <- Boreluy::nacional_uy(eleccion = anio,
                                   tipo = "Departamental") %>%
    mutate(Porcentaje = round(Porcentaje, 1)) %>% 
    mutate(Partido = ifelse(Porcentaje <= 5, "Otros Partidos", Partido),
           Sigla = ifelse(Porcentaje <= 5, "OtrosP.", Sigla)) %>%
    group_by(Partido) %>% 
    summarise(Porcentaje = sum(Porcentaje),
              Votos = sum(Votos),
              Sigla = first(Sigla),
              Fecha = first(Fecha),
              Eleccion = first(Eleccion))%>% 
    ungroup() %>% 
    mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
    mutate(Partido = fct_relevel(Partido, "Otros Partidos")) %>% 
    arrange(desc(Porcentaje))
  
  # Asignar color por partido
  color_partido <- dat_plot %>% 
    distinct(Partido) %>% 
    pull() %>% 
    coloriza_partidos()
  
  # Grafico
  plot <- ggplot(data =  dat_plot, 
                 aes(x = Partido,
                     y = Porcentaje, 
                     fill = Partido)) +
    geom_bar(stat = "identity",
             color = "black",
             alpha = .7) +
    coord_flip() + 
    geom_text(aes(label = paste0(Porcentaje, "%")),
              stat = "identity",
              hjust = -.5,
              size = 3) + 
    ylim(0, max(dat_plot$Porcentaje) + 10) +
    scale_fill_manual(values = color_partido) +
    theme_bdd() +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "none") +
    labs(x = "",
         y = "",
         title = paste("Porcentaje de votos en la elección de", anio, "por partido"),
         caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 
  
  print(plot)
}


# Función para grafico votos por partido y departamento  
tab_dptal <- function(anio = 2015) {
  
  suppressPackageStartupMessages(library(tidyverse))
  
  # Toma data de Boreluy
  dat_plot <- Boreluy::nacional_uy(eleccion = anio,
                                   tipo = "Departamental") %>%
    mutate(Porcentaje = round(Porcentaje, 1)) %>% 
    mutate(Partido = ifelse(Porcentaje <= 5, "Otros Partidos", Partido),
           Sigla = ifelse(Porcentaje <= 5, "OtrosP.", Sigla)) %>%
    group_by(Partido) %>% 
    summarise(Porcentaje = sum(Porcentaje),
              Votos = sum(Votos),
              Sigla = first(Sigla),
              Fecha = first(Fecha),
              Eleccion = first(Eleccion))%>% 
    ungroup() %>% 
    mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
    mutate(Partido = fct_relevel(Partido, "Otros Partidos")) %>% 
    arrange(desc(Porcentaje)) %>% 
    select(Partido, Votos, Porcentaje)
  
}

# Función para grafico votos por partido y departamento  
plot_dptal_xpto <- function(anio = 2020) {
  
  suppressPackageStartupMessages(library(tidyverse))
  
  # Toma data de Boreluy
  dat_plot <- Boreluy::nacional_uy(eleccion = anio,
                                   tipo = "Departamental",
                                   por_departamento = TRUE) %>%
    mutate(Porcentaje = round(Porcentaje, 1)) %>% 
    mutate(Partido = ifelse(Porcentaje <= 5, "Otros Partidos", Partido),
           Sigla = ifelse(Porcentaje <= 5, "OtrosP.", Sigla)) %>%
    group_by(Partido, Departamento) %>% 
    summarise(Porcentaje = sum(Porcentaje),
              Votos = sum(Votos),
              Sigla = first(Sigla),
              Fecha = first(Fecha),
              Eleccion = first(Eleccion))%>% 
    ungroup() %>% 
    mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
    mutate(Partido = fct_relevel(Partido, "Otros Partidos")) %>% 
    arrange(desc(Porcentaje))
  
  # Asignar color por partido
  color_partido <- dat_plot %>% 
    distinct(Partido) %>% 
    pull() %>% 
    coloriza_partidos()
  
  # Grafico
  plot <- ggplot(data =  dat_plot, 
                 aes(x = Partido,
                     y = Porcentaje, 
                     fill = Partido)) +
    geom_bar(stat = "identity",
             color = "black",
             alpha = .7) +
    coord_flip() + 
    geom_text(aes(label = paste0(Porcentaje, "%")),
                  stat = "identity",
                  hjust = -.5,
                  size = 3) + 
    scale_fill_manual(values = color_partido) +
    theme_bdd() +
    facet_wrap(~ Departamento, scales = "free") +
    ylim(0, max(dat_plot$Porcentaje) + 5) +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "none") +
    facet_wrap( ~ Departamento, ncol = 4) +
    labs(x = "",
         y = "",
         title = paste("Porcentaje de votos en la elección de", anio, "por partido y departamento"),
         caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 
  
  print(plot)
}

# Función para tabla votos por partido y departamento  
tab_dptal_xpto <- function(anio = 2020) {
  
  suppressPackageStartupMessages(library(tidyverse))
  
  # Toma data de Boreluy
  dat_plot <- Boreluy::nacional_uy(eleccion = anio,
                                   tipo = "Departamental",
                                   por_departamento = TRUE) %>%
    mutate(Porcentaje = round(Porcentaje, 1)) %>% 
    mutate(Partido = ifelse(Porcentaje <= 5, "Otros Partidos", Partido),
           Sigla = ifelse(Porcentaje <= 5, "OtrosP.", Sigla)) %>%
    group_by(Partido, Departamento) %>% 
    summarise(Porcentaje = sum(Porcentaje),
              Votos = sum(Votos),
              Sigla = first(Sigla),
              Fecha = first(Fecha),
              Eleccion = first(Eleccion))%>% 
    ungroup() %>% 
    mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
    mutate(Partido = fct_relevel(Partido, "Otros Partidos")) %>% 
    arrange(Departamento, desc(Porcentaje)) %>% 
    select(Departamento, Partido, Votos, Porcentaje)
}



