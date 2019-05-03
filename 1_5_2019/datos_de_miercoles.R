
library(tidyverse)
library(igraph)
library(ggraph)
library(gganimate)
library(ggforce)
library(magrittr)





comercio_hispanoamerica_mundo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv")


Quiero hacer los grafos para hispanoamerica, y no me interesan las autoexportaciones.



subset <- comercio_hispanoamerica_mundo %>% 
  filter(pais_destino_pertenece_a_hispanoamerica==1,
         codigo_iso_destino != codigo_iso_origen)




# Voy a considerar solamente las exportaciones que representan mas del 1% del total exportado por año, país dentro de la región


total_anio_pais <- subset %>% 
  group_by(nombre_pais_origen,anio) %>%
  summarise(tot_expo = sum(valor_exportado_dolares))
  
subset_filter <- subset %>% 
  left_join(total_anio_pais,by = c("anio", "nombre_pais_origen")) %>% 
  mutate(prop_expo = valor_exportado_dolares/tot_expo) %>% 
  filter(prop_expo>0.01)



# Caso particular


seleccion_prod <- c("Productos Minerales", "Maquinaria","Alimentos" )

subset_filter <- subset_filter %>% 
  filter(anio ==2013, nombre_comunidad_producto %in% seleccion_prod)


# Creo el grafo


grafo <- subset_filter %>% 
  select(nombre_pais_origen, nombre_pais_destino, everything()) %>% 
  graph_from_data_frame()



# Hago el grafico


set.seed(1)
layout <- layout.circle(grafo) %>% as.data.frame() %>% rename(x=V1, y=V2)

ggraph(graph = grafo,layout = 'manual', node.position=layout) + 
  geom_edge_arc(aes(edge_width=prop_expo, color= nombre_comunidad_producto),curvature = 0.1,
                arrow = arrow(length = unit(5, 'mm'))) + 
  geom_node_point()+
  geom_node_text(aes(label = name), check_overlap = TRUE)+
  theme_void() +
  theme(legend.position = 'none')+
  scale_edge_width_continuous('Proporción de las expo totales del país',labels = scales::percent_format(accuracy = 1))+
  facet_edges(~nombre_comunidad_producto, ncol = 3)


ggsave('red2013.png')
# Parte fallida

### Agrego animación 


subset_filter <- subset %>% 
  left_join(total_anio_pais,by = c("anio", "nombre_pais_origen")) %>% 
  mutate(prop_expo = valor_exportado_dolares/tot_expo) %>% 
  filter(prop_expo>0.01)


seleccion_prod <- c("Productos Minerales", "Maquinaria","Alimentos" )

subset_filter <- subset_filter %>% 
  filter(nombre_comunidad_producto %in% seleccion_prod)




grafo <- subset_filter %>% 
  select(nombre_pais_origen, nombre_pais_destino, everything()) %>% 
  graph_from_data_frame()


set.seed(1)
layout <- layout.circle(grafo) %>% as.data.frame() %>% rename(x=V1, y=V2)


positions <- subset_filter %>% 
  distinct(nombre_pais_destino) %>%
  arrange(match(nombre_pais_destino, V(grafo)$name)) %>% 
  mutate(x =layout$x, 
         y=layout$y)

positions <- bind_rows(replicate(length(unique(subset_filter$anio)), positions, simplify = FALSE)) %>% 
  mutate(anio = rep(unique(subset_filter$anio),each=nrow(positions)))

positions_end <- positions %>% 
  filter(anio==2017)



ggraph(graph = grafo,layout = 'manual', node.position=layout) + 
  geom_edge_arc(aes(edge_width=prop_expo, color= nombre_comunidad_producto),curvature = 0.1,
                arrow = arrow(length = unit(5, 'mm'))) + 
  geom_node_point()+
  geom_node_text(aes(label = name), check_overlap = TRUE)+
  theme_void() +
  theme(legend.position = 'none')+
  scale_edge_width_continuous('Proporción de las expo totales del país',labels = scales::percent_format(accuracy = 1))+
  facet_edges(~nombre_comunidad_producto, ncol = 3)+
  transition_time(anio)


plot <- ggraph(graph = grafo,layout = 'circle') + 
  geom_edge_arc(aes(edge_width=prop_expo, color= nombre_comunidad_producto),curvature = 0.1,
                arrow = arrow(length = unit(5, 'mm'))) + 
  # geom_node_point(data = positions_end, aes(x=x, y=y))+
  geom_node_point()+
  geom_node_text(aes(label = name), check_overlap = TRUE)+
  theme_void() +
  theme(legend.position = 'none')+
  scale_edge_width_continuous('Proporción de las expo totales del país',labels = scales::percent_format(accuracy = 1))+
  facet_edges(~nombre_comunidad_producto, ncol = 3)+
  transition_time(anio)


anim_save("gganigrafos.gif", plot)