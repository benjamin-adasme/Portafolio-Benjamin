# Gráficos post rrss -----------------------------------------------------------


pacman::p_load(tidyverse, # Paquetes de data science básicos
  readxl, # Cargar archivos xlsx
  scales, # Ajustar etiquetas de gráficos
  paletteer, # Cargar paletas de colores
  gganimate, # Animar gráficos
  ggtext, # Funciones de texto
  glue) # Paste con esteroides



base_70 <- read_xlsx("posts/piramide-r/data/composicion_poblacion.xlsx", sheet = "censo_70")
base_82 <- read_xlsx("posts/piramide-r/data/composicion_poblacion.xlsx", sheet = "censo_82")
base_92 <- read_xlsx("posts/piramide-r/data/1992_reporte.xlsx", sheet = "censo_92")
base_02 <- read_xlsx("posts/piramide-r/data/2002_reporte.xlsx", sheet = "censo_02")
base_17 <- read_xlsx("posts/piramide-r/data/2017_reporte.xlsx", sheet = "censo_17")
base_24 <- read_xlsx("posts/piramide-r/data/2024_reporte.xlsx")

# unimos todo en una lista
lista_total <- list(base_70, base_82, base_92, base_02, base_17, base_24)
# aplicamos rbind a cada elemento de la lista
union <- do.call(rbind, lista_total)

consolidado <- union |> 
  # removemos la etiqueta "años"
  mutate(Edades = str_remove(Edades, " años"),
         Edades = str_remove(Edades, "\\s\\s"),
  # Transformamos a factor en el orden en que aparecen
         Edades = fct_inorder(Edades)) |> 
  pivot_longer(cols = 3:4, 
               names_to = "Sexo",
               values_to = "Poblacion")

consol_porc <- consolidado |>
  # Agrupamos por la variable año, que representa el total de población en cada censo
  group_by(Año) |>
  # Dividimos cada valor por el total poblacional de ese año
  mutate(porc_año = Poblacion / sum(Poblacion))


p_base <- consol_porc |>
  mutate(
    porc_año = # SObre escribimos la variable
      if_else(Sexo == "Mujeres", # Evaluamos si el dato es de mujeres
        porc_año, # Caso verdadero: queda igual (derecha)
        porc_año * -1
      )
  ) |> # Caso falso-hombres: va hacia la izquierda
  filter(Año == 1970) |> 
  ggplot(aes(x = porc_año, y = Edades, fill = Sexo)) +
  geom_col()

p_base

marcas <- seq(-0.06,0.06,by=0.02)

labs_pir <- tibble(label = c("<span style='color:#f9844a'>**Hombres**</span>",
                             "<span style='color:#4d908e'>**Mujeres**</span>"),
                   x = c(-0.04, 0.04),
                   y = c("80-84", "80-84"),
                   colores = c("#4d908e","#f9844a"))

p_base +
  scale_fill_manual(values = c("#f9844a", "#4d908e")) +
  scale_x_continuous(
    breaks = marcas,
    labels = percent(abs(marcas))
  ) +
  labs(#title = "1970",
    # subtitle = "Pirámides de población con datos censales",
    caption = "Fuente: Instituto Nacional de Estadísticas, Chile",
    x = "Porcentaje de la población total",
    y = "Edades(quinquenios)"
  ) +
  guides(fill = guide_legend(
    position = "bottom",  
    title = NULL
  )) +
    geom_richtext(
      data = labs_pir, # Usamos el tibble creado
      aes(
        x = x, # las coordenadas x e y
        y = y,
        label = label,
        fill = NA
      ), # Evitamos el argumento fill de las barras
      fill = NA, # Quitamos recuadro alrededor del texto
      label.colour = NA,
      size = 6,
      show.legend = FALSE
    ) +
theme_minimal() +
  theme(
    plot.title.position = "panel",
    legend.position = "none")

ggsave("posts/piramide-r/plot_rrss/pir_70.png")


p_base <- consol_porc |>
  mutate(
    porc_año = # SObre escribimos la variable
      if_else(Sexo == "Mujeres", # Evaluamos si el dato es de mujeres
        porc_año, # Caso verdadero: queda igual (derecha)
        porc_año * -1
      )
  ) |> # Caso falso-hombres: va hacia la izquierda
  filter(Año == 2024) |> 
  ggplot(aes(x = porc_año, y = Edades, fill = Sexo)) +
  geom_col()

p_base +
  scale_fill_manual(values = c("#f9844a", "#4d908e")) +
  scale_x_continuous(
    breaks = marcas,
    labels = percent(abs(marcas))
  ) +
  labs(#title = "1970",
    # subtitle = "Pirámides de población con datos censales",
    caption = "Fuente: Instituto Nacional de Estadísticas, Chile",
    x = "Porcentaje de la población total",
    y = "Edades(quinquenios)"
  ) +
  guides(fill = guide_legend(
    position = "bottom",  
    title = NULL
  )) +
    geom_richtext(
      data = labs_pir, # Usamos el tibble creado
      aes(
        x = x, # las coordenadas x e y
        y = y,
        label = label,
        fill = NA
      ), # Evitamos el argumento fill de las barras
      fill = NA, # Quitamos recuadro alrededor del texto
      label.colour = NA,
      size = 6,
      show.legend = FALSE
    ) +
coord_cartesian(xlim = c(-0.06,0.06)) +
theme_minimal() +
  theme(
    plot.title.position = "panel",
    legend.position = "none")

ggsave("posts/piramide-r/plot_rrss/pir_24.png")
