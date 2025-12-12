#Codigo hecho por
#Alexis Salazar
#Víctor Rubilar

install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")

library(dplyr)
library(ggplot2)
library(stringr)



#archivo = read.csv("~/Mineria_de_Datos/Mineria-Eva-4/archivo_limpio.csv")
archivo = read.csv("archivo_limpio.csv")

View(archivo)



r_coquimbo = archivo %>% filter(Region=="Región de Coquimbo")
View(r_coquimbo)

r_arica = archivo %>% filter(Region=="Región de Arica y Parinacota")
View(r_arica)


# Evolución de registros mensual fruta hortaliza coquimbo

evo_fruta_coquimbo = r_coquimbo %>% filter(
    Categoria == "Fruta"
) %>% group_by(Annio, Mes) %>% summarise(
  c_fruta = n()
) %>% mutate(
  fecha = as.Date(paste0(Annio, "-", Mes, "-" ,"20"))
)


View(evo_fruta_coquimbo)

evo_hor_coquimbo = r_coquimbo %>% filter(
  Categoria == "Hortaliza"
) %>% group_by(Annio, Mes) %>% summarise(
  c_hor = n()
)  %>% mutate(
  fecha = as.Date(paste0(Annio, "-", Mes, "-" ,"20"))
)

View(evo_hor_coquimbo)

df_comparacion_coquimbo = data.frame(
  value = c(
    evo_fruta_coquimbo$c_fruta,
    evo_hor_coquimbo$c_hor
  ),
  fecha = rep(evo_fruta_coquimbo$fecha, 2),
  categoria = rep(c("Fruta", "Hortaliza"), 
                  each = nrow(evo_fruta_coquimbo))
)




ggplot(df_comparacion_coquimbo, aes(x = fecha, y = value, color = categoria)) + 
  geom_line() + 
  labs (
    title = "Evolución de frutas vs Hortalizas en Coquimbo",
    x = "AÑOS",
    y = "Ventas"
  )

# Evolución de registros mensual fruta hortaliza Arica


evo_fruta_arica = r_arica %>% filter(
  Categoria == "Fruta"
) %>% group_by(Annio, Mes) %>% summarise(
  a_fruta = n()
) %>% mutate(
  fecha = as.Date(paste0(Annio, "-", Mes, "-" ,"20"))
)


View(evo_fruta_arica)

evo_hor_arica = r_arica %>% filter(
  Categoria == "Hortaliza"
) %>% group_by(Annio, Mes) %>% summarise(
  a_hor = n()
)  %>% mutate(
  fecha = as.Date(paste0(Annio, "-", Mes, "-" ,"20"))
)

View(evo_hor_arica)

df_comparacion_arica = data.frame(
  value = c(
    evo_fruta_arica$a_fruta,
    evo_hor_arica$a_hor
  ),
  fecha = rep(evo_fruta_coquimbo$fecha, 2),
  categoria = rep(c("Fruta", "Hortaliza"), 
                  each = nrow(evo_fruta_coquimbo))
)

print(df_comparacion)


ggplot(df_comparacion_arica, aes(x = fecha, y = value, color = categoria)) + 
  geom_line() + 
  labs (
    title = "Evolución de frutas vs Hortalizas en Arica",
    x = "AÑOS",
    y = "Ventas"
  )

# Distribución de origen en ambas regiones de la ultima decada


origen_norte = archivo %>% filter(
  (Region=="Región de Coquimbo" | 
  Region == "Región de Arica y Parinacota") & 
  Annio >= 2021
  
) %>% 
  group_by(Extranjero) %>%
  summarise(
    n_origen = n()
  )
  

View(origen_norte)

total_origen = sum(origen_norte$n_origen) 

nacional = origen_norte$n_origen[1] / total_origen # nacional
extranjero = origen_norte$n_origen[2] / total_origen # extranjero

df_distribucion_norte = data.frame(
  value = c(
    paste0(round(nacional * 100, 2), "%"),
    paste0(round(extranjero * 100, 2), "%")
  ),
  origen = c("Nacional", "Extranjero")
)
ggplot(df_distribucion_norte, aes(x = "", y = value, fill = origen)) +
  geom_col(color = "black") +
  geom_text(aes(label = value), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  labs(
    title = "Distribución de origen de Productos en el norte de la ultima decada",
    x = "",
    y = "PORCENTAJE DE CATEGORIA",
  )
  

# Top 10 productos con mayor volumen promedio de la decada en el norte

top_10_productos = archivo %>% filter(
  (Region=="Región de Coquimbo" | 
   Region == "Región de Arica y Parinacota") & 
    Annio >= 2021
) %>% group_by(Categoria, Producto) %>% summarise(
  n_producto = round(mean(Volumen))
) %>% arrange(desc(n_producto)) 

View(top_10_productos)

ggplot(top_10_productos, aes(x = n_producto, y = reorder(Producto, n_producto))) + 
  geom_col(fill = "lightBlue") + 
  geom_text(aes(label = n_producto), position = position_stack(vjust = 0.5),) + 
  labs(
    title = "Productos con mayor volumen promedio de la decada en el norte",
    x = "VOLUMEN",
    y = "PRODUCTO"
  )

#Región vs Región | Coquimbo vs Arica y Parinacota
rgvsrg <- archivo %>%
  filter(Region == "Región de Coquimbo" | Region == "Región de Arica y Parinacota") %>%
  group_by(Region, Producto) %>%
  summarise(
    total_ventas = n(),
    precio_promedio = mean(Precio_promedio, na.rm = TRUE),
    volumen_total = sum(Volumen, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(Region, desc(total_ventas)) %>%
  group_by(Region) %>%
  slice_head(n = 10) %>%
  ungroup()

View(rgvsrg)

ggplot(rgvsrg, aes(x = total_ventas, y = reorder(Producto, total_ventas), fill = Region)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = scales::comma(total_ventas)), 
            hjust = -0.1, size = 3.5, color = "black") +
  facet_wrap(~ Region, scales = "free_y", ncol = 1) +
  labs(
    title = "Top 10 Productos: Arica vs Coquimbo",
    x = "Número de Transacciones",
    y = "Producto",
    fill = "Región"
  ) +
  scale_fill_manual(values = c("Región de Coquimbo" = "#1E90FF", "Región de Arica y Parinacota" = "#2E8B57")) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", size = 11)  
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) 


#Estacionalidad por año

ev_annio <- archivo %>%
  filter(Region == "Región de Coquimbo" | Region == "Región de Arica y Parinacota") %>%
  group_by(Region, Annio) %>%
  summarise(
    transacciones = n(),
    volumen_total = sum(Volumen, na.rm = TRUE),
    precio_promedio = mean(Precio_promedio, na.rm = TRUE),
    productos_unicos = n_distinct(Producto),
    .groups = 'drop'
  ) %>%
  mutate(
    Annio = as.factor(Annio),
    Region_simple = ifelse(Region == "Región de Arica y Parinacota", "Arica", "Coquimbo")
  )

ggplot(ev_annio, aes(x = Annio, y = volumen_total, group = Region_simple, color = Region_simple)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  geom_text(aes(label = scales::comma(round(volumen_total/1000, 1))),
            vjust = -1, size = 3, fontface = "bold") +
  labs(
    title = "Evolución Anual del Volumen de Transacciones por Región",
    subtitle = "Región de Arica y Parinacota vs Región de Coquimbo",
    x = "Año",
    y = "Volumen Total",
    color = "Región",
    caption = "Valores en miles"
  ) +
  scale_color_manual(
    values = c("Arica" = "#2E8B57", "Coquimbo" = "#1E90FF"),
    labels = c("Arica y Parinacota", "Coquimbo")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  ) +
  scale_y_continuous(labels = scales::comma)


 