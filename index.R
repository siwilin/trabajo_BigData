# CÓDIGO USADO EN EL TRABAJO

# Librerias y paquetes

library(jsonlite) #install.packages(jsonlite)
library(tidyverse) #install.packages(tidyverse)
library(plotly) #install.packages(plotly)
library(ggthemes) #install.packages(ggthemes)
library(gt) #install.packages(gt)
library("vembedr") #install.packages("vembedr")



historial.completo <- fromJSON("assets/data.json", flatten = TRUE)

my_spotify <- historial.completo %>%
  mutate(fechahora = as_datetime(ts, tz = NULL, format = NULL))
my_spotify <- my_spotify [, -c(1,2,4,5,9,10,11,12,13,14,15,16,17,18,19)]

my_spotify <- my_spotify %>%
  rename(cancion = master_metadata_track_name) %>%
  rename(album = master_metadata_album_album_name) %>%
  rename(artista = master_metadata_album_artist_name)

my_spotify <- my_spotify %>%
  mutate(dia = as_date(fechahora, tz = "UTC")) %>%
  mutate(fechahora = fechahora + hours(1) ) %>%
  mutate(segundos = ms_played/1000) %>%
  mutate(minutos = segundos/60)

my_spotify <- my_spotify %>%
  filter(dia >= "2024-01-01")

# Total artistas escuchados

artistasunicos <- my_spotify %>%
  summarize(totalartistas = n_distinct(artista))

artistasunicos

# Evolución del número de artistas diferentes escuchados a lo largo del año
# + su respectivo gráfico

artistasunicos_acumulado <- my_spotify %>% 
  arrange(dia) %>% 
  distinct(artista, .keep_all = TRUE) %>% 
  group_by(dia) %>% 
  summarize(totalartistas = n_distinct(artista)) %>% 
  mutate(sumaartistas = cumsum(totalartistas))
graficoartistas <- artistasunicos_acumulado %>%
  ggplot(aes(x = dia, y = sumaartistas)) + 
  geom_col(aes(fill = sumaartistas)) +
  scale_fill_gradient(high = "#1db954", low = "#1db954") + 
  labs(x = "Fecha", y = "Nº artistas") + 
  ggtitle("Número acumulado de artistas únicos escuchados") +
  theme_minimal()+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")

graficoartistas

# Encontrar el dia en que el valor coincide con el que da Spotify Wrapped

diadatos <- artistasunicos_acumulado %>%
  filter(sumaartistas >= 2070)%>%
  slice_head(n = 1) %>%
  pull(dia)

# Total canciones escuchadas

cancionesunicas <- my_spotify %>%
  summarize(totalcanciones = n_distinct(cancion))

cancionesunicas

# Evolución del número de canciones diferentes escuchadas a lo largo del año
# + su respectivo gráfico

cancionesunicas_acumulado <- my_spotify %>% 
  distinct(cancion, .keep_all = TRUE) %>%
  arrange(dia) %>% 
  group_by(dia) %>% 
  summarize(totalcanciones = n_distinct(cancion)) %>% 
  mutate(sumacanciones = cumsum(totalcanciones))

graficocanciones <- cancionesunicas_acumulado %>%
  ggplot(aes(x = dia, y = sumacanciones)) + 
  geom_col(aes(fill = sumacanciones)) +
  scale_fill_gradient(high = "#1db954", low = "#1db954") + 
  labs(x = "Fecha", y = "Nº canciones") + 
  ggtitle("Número acumulado de canciones únicas escuchadas") +
  theme_minimal()+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")

graficocanciones

# Encontrar el dia en que el valor coincide con el que da Spotify Wrapped

diadatos2 <- cancionesunicas_acumulado %>%
  filter(sumacanciones >= 2745)%>%
  slice_head(n = 1) %>%
  pull(dia)

# Minutos totales de musica escuchados

minutosescucha <- my_spotify %>% 
  summarize(totalminutos = sum(minutos))%>%
  pull(totalminutos) %>%
  as.integer()
minutosescucha

# Top día con más tiempo escuchado

topmin <- my_spotify %>%
  group_by(dia) %>%
  summarize(totalminutos = sum(minutos)) %>%
  arrange(desc(totalminutos)) %>%
  slice_head(n = 1)

# Top dia con más minutos escuchados -> día

topmin_dia <- topmin%>%
  pull(dia)

# Top dia con más minutos escuchados -> minutos escuchados ese dia

topmin_min <- topmin%>%
  pull(totalminutos) %>%
  as.integer()

# Evolución del tiempo total de música escuchada a lo largo del año
# + su respectivo gráfico

totalminutos <- my_spotify %>% 
  arrange(dia) %>% 
  group_by(dia) %>% 
  summarize(totalminutos = sum(minutos))%>%
  mutate(sumaminutos = cumsum(totalminutos))

graficominutos <- totalminutos %>%
  ggplot(aes(x = dia, y = sumaminutos)) + 
  geom_col(aes(fill = sumaminutos)) +
  scale_fill_gradient(high = "#1db954", low = "#1db954") + 
  labs(x = "Fecha", y = "Nº minutos") + 
  ggtitle("Número total de minutos escuchados") +
  theme_minimal()+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")

graficominutos

# Encontrar el dia en que el valor coincide con el que da Spotify Wrapped

diadatos3 <- totalminutos %>%
  filter(sumaminutos >= 20911)%>%
  slice_head(n = 1) %>%
  pull(dia)

# Top 5 artistas por tiempo de escucha

top5artistas_tiempo <- my_spotify %>%
  group_by(artista) %>%
  summarize(minutosartista = sum(minutos)) %>%
  arrange(desc(minutosartista)) %>%
  slice_head(n = 5)

# Top 1 artistas por tiempo de escucha (tiempo de escucha)

topartista_tiempo <- top5artistas_tiempo%>%
  slice_head(n=1)%>%
  pull(minutosartista)%>%
  as.integer()

# Top 1 artistas por tiempo de escucha (artista)

topartista <- top5artistas_tiempo%>%
  slice_head(n=1)%>%
  pull(artista)

# Tabla top 5 

top5artistas_tiempo %>%
  gt() %>%
  tab_header (
    title = "Top 5 artistas 2024 - Playtime",
    subtitle = "Minutos escuchados por artista"
  ) %>%
  cols_label(
    artista = "Artista",
    minutosartista = "Minutos" 
  ) %>%
  fmt_number (
    columns = "minutosartista",
    decimals = 0 
  )

# Top 5 artistas por nº de reproducciones

top5artistas_reproducciones <- my_spotify %>%
  group_by(artista) %>%
  summarize(reproducciones = n()) %>% 
  arrange(desc(reproducciones)) %>%
  slice_head(n = 5)

# Tabla top 5

top5artistas_reproducciones_tabla <- top5artistas_reproducciones %>%
  gt() %>%
  tab_header(
    title = "Top 5 artistas 2024 - Reproducciones",
    subtitle = "Número de reproducciones por artista"
  ) %>%
  cols_label(
    artista = "Artista",
    reproducciones = "Reproducciones"
  ) %>%
  fmt_number(
    columns = "reproducciones",
    decimals = 0
  )

top5artistas_reproducciones_tabla

# Top 5 canciones (tiempo de escucha)

topcanciones_tiempo <- my_spotify %>% 
  group_by(cancion) %>% 
  summarize(totalminutos = sum(minutos)) %>% 
  arrange(desc(totalminutos)) %>%
  slice_head(n = 5)

# Top 1 cancion tiempo de escucha (tiempo de escucha)

topcancion_tiempo <- topcanciones_tiempo%>%
  slice_head(n=1)%>%
  pull(totalminutos)%>%
  as.integer()

# Top 1 cancion tiempo de escucha (cancion)

topcancion <- topcanciones_tiempo%>%
  slice_head(n=1)%>%
  pull(cancion)

# Grafico top 5 canciones (tiempo de escucha)

t5tiempografico <- topcanciones_tiempo %>%
  ggplot(aes(x = reorder(cancion, totalminutos), y = totalminutos)) +
  geom_col(aes(fill = totalminutos)) +
  scale_fill_gradient(low = "#1db954", high = "#1db954") + 
  labs(x= "", y= "Minutos") + 
  ggtitle("Top 5 canciones más escuchadas") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")+
  coord_flip()

t5tiempografico

topcanciones_tiempo

# Top 5 canciones (nº de reproducciones)

topcanciones_repro <- my_spotify %>% 
  group_by(cancion) %>% 
  summarize(reproducciones = n()) %>% 
  arrange(desc(reproducciones)) %>%
  slice_head(n = 5) 

# Grafico top 5 canciones (nº de reproducciones)

t5tiempografico <- topcanciones_repro%>%   
  ggplot(aes(x = reorder(cancion, reproducciones), 
             y = reproducciones)) +
  geom_col(aes(fill = reproducciones)) +
  scale_fill_gradient(low = "#1db954", high = "#1db954") + 
  labs(x= "", y= "Reproducciones") + 
  ggtitle("Top 5 canciones más reproducidas") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")+
  coord_flip()

t5tiempografico 

# Tabla top 5 canciones (tiempo de escucha)

top5canciones_tiempo_tabla <- topcanciones_tiempo %>%
  gt() %>%
  tab_header (
    title = "Top 5 canciones",
    subtitle = "Minutos escuchados por canción"
  ) %>%
  cols_label(
    cancion = "Cancion",
    totalminutos = "Minutos" 
  ) %>%
  fmt_number (
    columns = everything(),
    decimals = 0 
  )

top5canciones_tiempo_tabla

# Numero de artistas escuchados cada dia del año

artistaspordia <- my_spotify %>% 
  group_by(dia) %>% 
  summarize(totalartistas = n_distinct(artista)) %>% 
  arrange(dia) %>%
  ggplot(aes(x = dia, y = totalartistas)) + 
  geom_col(aes(fill = totalartistas)) +
  scale_fill_gradient(high = "#1db954", low = "#1db954") + 
  labs(x= "Fecha", y= "Nº artistas") + 
  ggtitle("Numero de artistas escuchados") + 
  theme(legend.position = "none")
artistaspordia

# Horas de música escuchadas cada semana

horasmusica <- my_spotify %>% 
  group_by(dia) %>% 
  group_by(dia = floor_date(dia, "week")) %>%
  summarize(horas = sum(minutos) / 60) %>% 
  arrange(dia) %>% 
  ggplot(aes(x = dia, y = horas)) + 
  geom_col(aes(fill = horas)) +
  scale_fill_gradient(low = "#1db954", high = "#1db954") + 
  labs(x= "FECHA", 
       y= "HORAS",
       title = "Gráfico 1: Horas de música escuchadas en 2024",
       subtitle = "(Diferenciando entre semanas)",
       caption = "Fuente: Datos de mi propia cuenta de Spotify") + 
  theme(legend.position = "none")
horasmusica

# Info de la sesión

#| echo: false
sessioninfo::session_info() %>% 
  details::details(summary = 'current session info') 

