pacman::p_load(tidyverse,
               readxl,
               scales,
               paletteer,
               gganimate, 
               # camcorder,
               ggtext,
               glue)

# Loading data ------------------------------------------------------------

base_70 <- read_xlsx("posts/piramide-r/data/composicion_poblacion.xlsx", sheet = "censo_70")
base_82 <- read_xlsx("posts/piramide-r/data/composicion_poblacion.xlsx", sheet = "censo_82")
base_92 <- read_xlsx("posts/piramide-r/data/1992_reporte.xlsx", sheet = "censo_92")
base_02 <- read_xlsx("posts/piramide-r/data/2002_reporte.xlsx", sheet = "censo_02")
base_17 <- read_xlsx("posts/piramide-r/data/2017_reporte.xlsx", sheet = "censo_17")

lista_total <- list(base_70, base_82, base_92, base_02, base_17)

# writexl::write_xlsx(lista_total, "data/base_piramide_poblacion_chile.xlsx")

union <- do.call(rbind, lista_total)

consolidado <- union |> 
  mutate(Edades = str_remove(Edades, " años"),
         Edades = str_remove(Edades, "\\s\\s"),
         Edades = fct_inorder(Edades)) |> 
  pivot_longer(cols = 3:4, 
               names_to = "Sexo",
               values_to = "Poblacion")


consol_porc <- consolidado |> 
  group_by(Año) |> 
  mutate(porc_año = Poblacion/sum(Poblacion))

consol_porc |> 
  mutate(porc_año = if_else(Sexo == "Mujeres", porc_año, porc_año * -1)) |> 
  ggplot(aes(x = porc_año, y = Edades, fill = Sexo)) +
  geom_col() +
  facet_wrap(.~Año)


consol_porc |> 
  mutate(porc_año = if_else(Sexo == "Mujeres", porc_año , porc_año * -1)) |> 
  ggplot(aes(x = porc_año, y = Edades, fill = Sexo)) +
  geom_col() +
  scale_fill_manual(values = c("#f9844a", "#4d908e"))+
  guides(fill = guide_legend(title = NULL, 
                             position = "bottom",
                             reverse = F)) +
  scale_x_continuous(breaks = seq(-0.06,0.06,by=0.02),
                     labels = percent(abs(seq(-0.06,0.06,by=0.02))),
                     name = "Porcentaje respecto a la población total")+
  theme_minimal() +
  transition_states(states = Año, 
                    state_length = 4,
                    transition_length = 2) +
  # annotate("text", x = -0.04, y = "80-84", label = "{closest_state}") +
  # geom_text(aes(x = -0.04, y = "80-84", label = "{closest_state}")) +
  labs(title = "¿Cómo cambia nuestra demografía en los últimos 50 años?", 
       subtitle = "Censo de {closest_state}, Chile",
       caption = "Fuente: Censos de Población y Vivienda de 1970, 1982, 1992, 2002 y 2017, INE Chile") +
  theme(plot.title.position = "panel",
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5))

anim_save("piramide2.gif")


consol_porc |> 
  mutate(porc_año = if_else(Sexo == "Mujeres", porc_año , porc_año * -1)) |> 
  filter(Año == 1970) |> 
  ggplot(aes(x = porc_año, y = Edades, fill = Sexo)) +
  geom_col() +
  scale_fill_manual(values = c("#f9844a", "#4d908e"))+
  guides(fill = guide_legend(title = NULL, 
                             position = "bottom",
                             reverse = F)) +
  scale_x_continuous(breaks = seq(-0.06,0.06,by=0.02),
                     labels = percent(abs(seq(-0.06,0.06,by=0.02))),
                     name = "Porcentaje respecto a la población total")+
  theme_minimal() 
