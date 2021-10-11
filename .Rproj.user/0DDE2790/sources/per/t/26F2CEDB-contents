
### Elección 2019  =========================================================

### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2019, 
                       tipo = "Presidencial") 
  
# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  agrupar_partidos_uy() %>% # Agrupa partidos que no alcanzan el 2%
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Cabildo Abierto" = "#F8BC1E",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 2019 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

rm(list=ls())

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2019, 
                       tipo = "Presidencial",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Partido %in% c("Voto Anulado", "Voto en Blanco") ~ "Voto Blanco/Anulado",
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Cabildo Abierto" = "#F8BC1E",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 2019 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")


### Elección 2014  =========================================================
### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2014, 
                       tipo = "Presidencial") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  agrupar_partidos_uy() %>% # Agrupa partidos que no alcanzan el 2%
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Partido Independiente" = "#663399",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 2014 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

rm(list=ls())

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2014, 
                       tipo = "Presidencial",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Partido %in% c("Voto Anulado", "Voto en Blanco") ~ "Voto Blanco/Anulado",
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Partido Independiente" = "#663399",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 2014 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")

### Elección 2009  =========================================================
### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2009, 
                       tipo = "Presidencial") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  agrupar_partidos_uy() %>% # Agrupa partidos que no alcanzan el 2%
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Partido Independiente" = "#663399",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 2009 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2009, 
                       tipo = "Presidencial",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Partido %in% c("Voto Anulado", "Voto en Blanco") ~ "Voto Blanco/Anulado",
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Partido Independiente" = "#663399",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 2009 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")

### Elección 2004  =========================================================
### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2004, 
                       tipo = "Presidencial") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  agrupar_partidos_uy() %>% # Agrupa partidos que no alcanzan el 2%
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 2004 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2004, 
                       tipo = "Presidencial",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Partido %in% c("Voto Anulado", "Voto en Blanco") ~ "Voto Blanco/Anulado",
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 2004 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")


### Elección 1999  =========================================================
### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1999, 
                       tipo = "Presidencial") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  agrupar_partidos_uy() %>% # Agrupa partidos que no alcanzan el 2%
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Nuevo Espacio" = "#2CA015",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 1999 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1999, 
                       tipo = "Presidencial",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Partido %in% c("Voto Anulado", "Voto en Blanco") ~ "Voto Blanco/Anulado",
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Nuevo Espacio" = "#2CA015",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 1999 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")



### Elección 1994  =========================================================
### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1994, 
                       tipo = "Presidencial") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  agrupar_partidos_uy() %>% # Agrupa partidos que no alcanzan el 2%
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Nuevo Espacio" = "#2CA015",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 1994 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1994, 
                       tipo = "Presidencial",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Partido %in% c("Voto Anulado", "Voto en Blanco") ~ "Voto Blanco/Anulado",
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Nuevo Espacio" = "#2CA015",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 1994 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")


### Elección 1989  =========================================================
### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1989, 
                       tipo = "Presidencial") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  agrupar_partidos_uy() %>% # Agrupa partidos que no alcanzan el 2%
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Nuevo Espacio" = "#2CA015",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 1989 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1989, 
                       tipo = "Presidencial",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Partido %in% c("Voto Anulado", "Voto en Blanco") ~ "Voto Blanco/Anulado",
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Nuevo Espacio" = "#2CA015",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 1989 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")



### Elección 1984  =========================================================
### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1984, 
                       tipo = "Presidencial") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  agrupar_partidos_uy() %>% # Agrupa partidos que no alcanzan el 2%
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Union Civica" = "#1F618D",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 1984 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1984, 
                       tipo = "Presidencial",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Partido %in% c("Voto Anulado", "Voto en Blanco") ~ "Voto Blanco/Anulado",
    Porcentaje < 2 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Voto Blanco/Anulado", 
                                          "Otros Partidos")))  
# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Union Civica" = "#1F618D",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en la elección de 1984 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")



### Departamental 2015  =========================================================

### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2015, 
                       tipo = "Departamental") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ as.character(Partido))) %>%
  group_by(Partido) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Partido de la Concertacion" = "#117A65",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 2015 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

rm(list=ls())

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2015, 
                       tipo = "Departamental",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento, scales = "free") +
  ylim(0, max(dat_ele$Porcentaje) + 5) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Partido de la Concertacion" = "#117A65",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 2015 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")


### Departamental 2010  =========================================================

### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2010, 
                       tipo = "Departamental") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ as.character(Partido))) %>%
  group_by(Partido) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Partido de la Concertacion" = "#117A65",
                               "Voto Blanco/Anulado" = "#696969",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 2010 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

rm(list=ls())

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2010, 
                       tipo = "Departamental",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento, scales = "free") +
  ylim(0, max(dat_ele$Porcentaje) + 5) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Partido de la Concertacion" = "#117A65",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 2010 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")



### Departamental 2005  =========================================================

### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2005, 
                       tipo = "Departamental") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ as.character(Partido))) %>%
  group_by(Partido) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 2005 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

rm(list=ls())

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2005, 
                       tipo = "Departamental",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento, scales = "free") +
  ylim(0, max(dat_ele$Porcentaje) + 5) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 2005 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")



### Departamental 2000  =========================================================

### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2000, 
                       tipo = "Departamental") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ as.character(Partido))) %>%
  group_by(Partido) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 2000 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

rm(list=ls())

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 2000, 
                       tipo = "Departamental",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento, scales = "free") +
  ylim(0, max(dat_ele$Porcentaje) + 5) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 2000 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")


### Departamental 1994  =========================================================

### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1994, 
                       tipo = "Departamental") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ as.character(Partido))) %>%
  group_by(Partido) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 1994 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

rm(list=ls())

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1994, 
                       tipo = "Departamental",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento, scales = "free") +
  ylim(0, max(dat_ele$Porcentaje) + 5) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Nuevo Espacio" = "#2CA015",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 1994 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")


### Departamental 1989  =========================================================

### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1989, 
                       tipo = "Departamental") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ as.character(Partido))) %>%
  group_by(Partido) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Nuevo Espacio" = "#2CA015",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 1989 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

rm(list=ls())

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1989, 
                       tipo = "Departamental",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento, scales = "free") +
  ylim(0, max(dat_ele$Porcentaje) + 5) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Nuevo Espacio" = "#2CA015",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 1989 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")



### Departamental 1984  =========================================================

### * Total país  ==========================================================

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1984, 
                       tipo = "Departamental") 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ as.character(Partido))) %>%
  group_by(Partido) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  coord_flip() + 
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 1984 por partido"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") 

### * Total por departamento  ===============================================

rm(list=ls())

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

# Extraer datos de Boreluy
dat_ele <- nacional_uy(eleccion = 1984, 
                       tipo = "Departamental",
                       por_departamento = TRUE) 

# Ordenar datos
dat_ele <- dat_ele %>% 
  mutate(Porcentaje = round(Porcentaje, 1)) %>% 
  mutate(Partido = case_when(
    Porcentaje < 5 ~ "Otros Partidos",
    TRUE ~ Partido)) %>%
  group_by(Partido, Departamento) %>% 
  summarise(Porcentaje = sum(Porcentaje),
            Votos = sum(Votos),
            Fecha = first(Fecha),
            Eleccion = first(Eleccion)) %>% 
  ungroup() %>% 
  mutate(Partido = fct_reorder(Partido, Porcentaje)) %>% 
  mutate(Partido = fct_relevel(Partido, c("Otros Partidos")))  

# Tabla
head(dat_ele)

# Gráfico
ggplot(data = dat_ele, aes(x = Partido, y = Porcentaje, fill = Partido)) +
  geom_bar(stat = "identity", color = "black", alpha = .7) +
  geom_text(aes(label = paste0(Porcentaje, "%")),
            stat = "identity",
            hjust = -.5,
            size = 3) + 
  coord_flip() +
  facet_wrap(~ Departamento, scales = "free") +
  ylim(0, max(dat_ele$Porcentaje) + 5) +
  scale_fill_manual(values = c("Frente Amplio" = "#013197",
                               "Partido Nacional" = "#99ccff",
                               "Partido Colorado" = "#BA0200",
                               "Nuevo Espacio" = "#2CA015",
                               "Otros Partidos" = "#A9A9A9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = paste("Porcentaje de votos en las elecciones departamentales de 1984 por partido y departamento"),
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy")

## Indicadores ============================================================

## Volatilidad de Pedersen ----

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

lista_anios <- c(1984, 1989, 1994, 1999, 2004, 2009, 2014, 2019)

nacional <- lapply(lista_anios, nacional_uy) 

data_esaps <- do.call(rbind, nacional) %>% 
  as_esaps()

pedersen <- esaps::evolat(data_esaps, "Pedersen")

ggplot(pedersen, aes(election, eVolat, group = 1)) +
  geom_line(size = 1, color = "#2c3e50", alpha = 0.5) +
  geom_point(size = 3, color = "#2c3e50") +
  labs(x = "", 
       y = "",
       title = "Volatilidad de Pedersen",
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))


## Volatilidad de Powell y Tucker ----

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

lista_anios <- c(1984, 1989, 1994, 1999, 2004, 2009, 2014, 2019)

nacional <- lapply(lista_anios, nacional_uy) 

data_esaps <- do.call(rbind, nacional) %>% 
  as_esaps()

powell <- esaps::evolat(data_esaps, "Powell and Tucker") 

ggplot(powell %>% 
         pivot_longer(volat_A:volat_B,
                      names_to = "Tipo",
                      values_to = "Valor"), 
       aes(as.numeric(election), Valor, color = Tipo)) +
  geom_line(size = 1,  alpha = 0.5) +
  geom_point(size = 3) +
  labs(x = "", 
       y = "",
       title = "Volatilidad de Powell y Tucker",
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2")



## Desproporcionalidad electoral - Gallagher ----

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

lista_anios <- c(1984, 1989, 1994, 1999, 2004, 2009, 2014, 2019)

nacional <- lapply(lista_anios, nacional_uy) 

data_esaps <- do.call(rbind, nacional) %>% 
  as_esaps()

gallagher <- data_esaps %>% 
  drop_na(votes, seats) %>% 
  esaps::dispro(method = "Gallagher") 
  
ggplot(gallagher, aes(election, Gallagher, group = 1)) +
  geom_line(size = 1, color = "#2c3e50", alpha = 0.5) +
  geom_point(size = 3, color = "#2c3e50") +
  labs(x = "", 
       y = "",
       title = "Desproporcionalidad electoral - Gallagher",
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))


## Desproporcionalidad electoral - Rae ----

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

lista_anios <- c(1984, 1989, 1994, 1999, 2004, 2009, 2014, 2019)

nacional <- lapply(lista_anios, nacional_uy) 

data_esaps <- do.call(rbind, nacional) %>% 
  as_esaps()

dispro_rae <- data_esaps %>% 
  drop_na(votes, seats) %>% 
  esaps::dispro(method = "Rae")

ggplot(dispro_rae, aes(election, Rae, group = 1)) +
  geom_line(size = 1, color = "#2c3e50", alpha = 0.5) +
  geom_point(size = 3, color = "#2c3e50") +
  labs(x = "", 
       y = "",
       title = "Desproporcionalidad electoral - Rae",
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

## NEP ----

## Instalar paquetes si es necesario
# install.packages(tidyverse)
# install.packages(Boreluy)

# Cargar paquetes
library(tidyverse)
library(Boreluy)

lista_anios <- c(1984, 1989, 1994, 1999, 2004, 2009, 2014, 2019)

nacional <- lapply(lista_anios, nacional_uy) 

data_esaps <- do.call(rbind, nacional) %>% 
  as_esaps()

enp <- data_esaps %>% 
  esaps::enp() 

ggplot(enp, aes(election, enp, group = 1)) +
  geom_line(size = 1, color = "#2c3e50", alpha = 0.5) +
  geom_point(size = 3, color = "#2c3e50") +
  labs(x = "", 
       y = "",
       title = "Número Efectivo de Partidos",
       caption = "Fuente: Unidad de Métodos y Acceso a Datos (FCS - UdelaR) en base a datos de Boreluy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

