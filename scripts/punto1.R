#########################
# Taller 4: Punto 1 
#########################

# Libraries ---------------------------------------------------------------
require(pacman)
p_load("tidyverse", "sf", "RColorBrewer", "stats")

# Data --------------------------------------------------------------------
load("data/Taller4_Ejercicio1.Rdata")
colnames(barrios) <- c("zona180", "geometry") #Para unir con las demás

# Figura 1 ----------------------------------------------------------------

# Conteo de restaurantes por zona y año
restaurants_zona <- restaurants %>%
  mutate(restaurant_2004 = ifelse(is.na(lat2004),0,1)) %>%
  mutate(restaurant_2012 = ifelse(is.na(lat2012),0,1)) %>%
  filter(restaurant_2004!=0 | restaurant_2012!=0) %>%
  group_by(zona180) %>%
  summarise(n_restaurants_2004 = sum(restaurant_2004), 
            n_restaurants_2012 = sum(restaurant_2012))

# Unir con poblacion y geometría
restaurants_zona <- restaurants_zona %>%
  left_join(poblacion) %>%
  left_join(as.data.frame(barrios))

# Encontrar densidades
restaurants_zona <- restaurants_zona %>%
  mutate(densidad_restaurantes_2004 = n_restaurants_2004 / (day_pop / 1000),
         densidad_restaurantes_2012 = n_restaurants_2012 / (day_pop / 1000))

# Densidades relativas a la media y cambio porcentual
mean_densidad_2004 <- mean(restaurants_zona$densidad_restaurantes_2004)
mean_densidad_2012 <- mean(restaurants_zona$densidad_restaurantes_2012)

restaurants_zona <- restaurants_zona %>%
  mutate(densidad_relativa_2004 = densidad_restaurantes_2004 - mean_densidad_2004,
         densidad_relativa_2012 = densidad_restaurantes_2012 - mean_densidad_2012) %>%
  mutate(perc_growth = (densidad_restaurantes_2012 / densidad_restaurantes_2004) -1) %>%
  st_as_sf()

### Mapas

df_maps_densidad <- restaurants_zona %>%
  select(zona180, densidad_relativa_2004, densidad_relativa_2012, geometry) %>%
  pivot_longer(cols = c(densidad_relativa_2004, densidad_relativa_2012), names_to = "year", values_to = "densidad_relativa") %>%
  mutate(year = case_when(
    year == "densidad_relativa_2004" ~ 2004, 
    year == "densidad_relativa_2012" ~ 2012
  )) %>%
  st_as_sf()

# a) Densidad relativa en 2004 y 2012

map_densidad <- ggplot(df_maps_densidad) +
  geom_sf(aes(fill = densidad_relativa), color = "black") +
  facet_wrap(~year, 
             labeller = labeller(year = 
                                   c("2004" = "Restaurantes per capita (vs la media) en 2004",
                                     "2012" = "Restaurantes per capita (vs la media) en 2012 "))) +
  scale_fill_viridis_b(breaks = seq(-3, 12, 3), option = "mako", direction = -1) +
  labs(fill="Densidad\nrestaurantes") +
  theme_void()

ggsave( "outputs/map_densidad.png", map_densidad, dpi=300)

# b) Cambio porcentual 

map_perc <- ggplot(restaurants_zona) +
  geom_sf(aes(fill = perc_growth), color = "black") +
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn", 
               palette = function(x) brewer.pal(length(seq(-.8,.8,.20)), "BuPu"),
               breaks = seq(-.8,.8,.20),
               guide = "colorsteps",
               labels = scales::label_percent()) +  
  labs(fill="Cambio\nporcentual") +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

ggsave("outputs/map_perc.png", map_perc, dpi=300)


# Kernel estimators -------------------------------------------------------

# 1) Rule of thumb: Gaussian y Epanechnikov
prices_2004 <- restaurants$prezzo2004[!is.na(restaurants$prezzo2004)]
prices_2012 <- restaurants$prezzo2012[!is.na(restaurants$prezzo2012)]

thumb_2004 <- bw.nrd(prices_2004)
thumb_2012 <- bw.nrd(prices_2012) #Rule of thumb calculator

# Epanechnikov

densidad_precios_2004_ep_thumb <- density(prices_2004, bw = thumb_2004, kernel = "epanechnikov")
densidad_precios_2012_ep_thumb <- density(prices_2012, bw = thumb_2012, kernel = "epanechnikov")

# Gaussian

densidad_precios_2004_ga_thumb <- density(prices_2004, bw = thumb_2004, kernel = "gaussian")
densidad_precios_2012_ga_thumb <- density(prices_2012, bw = thumb_2012, kernel = "gaussian")

# 2) No rule of thumb: Epanechnikov

densidad_precios_2004_ep_thumb_2 <- density(prices_2004, bw = thumb_2004*2, kernel = "epanechnikov")
densidad_precios_2012_ep_thumb_2 <- density(prices_2012, bw = thumb_2012*2, kernel = "epanechnikov")

densidad_precios_2004_ep_thumb_05 <- density(prices_2004, bw = thumb_2004/2, kernel = "epanechnikov")
densidad_precios_2012_ep_thumb_05 <- density(prices_2012, bw = thumb_2012/2, kernel = "epanechnikov")

## Graficas

#Epanechnikov (thumb)
pdf("outputs/EP Thumb.pdf")
plot(densidad_precios_2004_ep_thumb, 
     ylim = range(densidad_precios_2004_ep_thumb$y, densidad_precios_2012_ep_thumb$y),
     col = "red",
     main = "Epanechnikov\n(Rule of thumb)",
     ylab = "Densidad",
     xlab = "Precio en el restaurante")
lines(densidad_precios_2012_ep_thumb, col = "black")
legend("topright", c("2004","2012"), lty = c(1,1), col = c("red","black"))
dev.off()

#Gaussian (thumb)
pdf("outputs/Gauss Thumb.pdf")
plot(densidad_precios_2004_ga_thumb, 
                 ylim = range(densidad_precios_2004_ga_thumb$y, densidad_precios_2012_ga_thumb$y),
                 col = "red",
                 main = "Gaussiano\n(Rule of thumb)",
                 ylab = "Densidad",
                 xlab = "Precio en el restaurante")
lines(densidad_precios_2012_ga_thumb, col = "black")
legend("topright", c("2004","2012"), lty = c(1,1), col = c("red","black"))
dev.off()

#Epanechnikov (thumb*2)
pdf("outputs/EP Thumb2.pdf")
plot(densidad_precios_2004_ep_thumb_2, 
                 ylim = range(densidad_precios_2004_ep_thumb_2$y, densidad_precios_2012_ep_thumb_2$y),
                 col = "red",
                 main = "Epanechnikov\n(Rule of thumb * 2)",
                 ylab = "Densidad",
                 xlab = "Precio en el restaurante")
lines(densidad_precios_2012_ep_thumb_2, col = "black")
legend("topright", c("2004","2012"), lty = c(1,1), col = c("red","black"))
dev.off()

#Epanechnikov (thumb/2)
pdf("outputs/EP Thumb05.pdf")
plot(densidad_precios_2004_ep_thumb_05, 
                 ylim = range(densidad_precios_2004_ep_thumb_05$y, densidad_precios_2012_ep_thumb_05$y),
                 col = "red",
                 main = "Epanechnikov\n(Rule of thumb / 2)",
                 ylab = "Densidad",
                 xlab = "Precio en el restaurante")
lines(densidad_precios_2012_ep_thumb_05, col = "black")
legend("topright", c("2004","2012"), lty = c(1,1), col = c("red","black"))
dev.off()

graphics.off()


# Prueba de Duranton - Overman --------------------------------------------

# 1) Extraer los cinco barrios con mayor crecimiento de restaurantes per capita

top5_barrios <- restaurants_zona %>%
  arrange(desc(perc_growth))

top5_barrios <- top5_barrios[1:5, c("zona180")]

# 2) Seleccionar los datos relevantes de estos barrios

restaurants_top_2004 <- restaurants %>%
  mutate(restaurant_2004 = ifelse(is.na(lat2004),0,1)) %>%
  filter(restaurant_2004==1) %>%
  filter(zona180%in%top5_barrios$zona180) %>%
  st_as_sf(coords = c("long2004", "lat2004"), crs = 4326) %>%
  st_transform(crs = 6875) #Projection of northern Italy in meters (Euclidean distances)

restaurants_top_2012 <- restaurants %>%
  mutate(restaurant_2012 = ifelse(is.na(lat2012),0,1)) %>%
  filter(restaurant_2012==1) %>%
  filter(zona180%in%top5_barrios$zona180) %>%
  st_as_sf(coords = c("long2012", "lat2012"), crs = 4326) %>%
  st_transform(crs = 6875)

# 3) Calcular distancias bilaterales y densidades gaussianas

density_calculator<-function(dta){
  dta<-st_distance(dta, which = "Euclidean") 
  dta[lower.tri(dta, diag=TRUE)] <- NA
  dta<-c(dta)
  dta<-dta[!is.na(dta)]
  dens<-density(dta, bw="nrd0", kernel="gaussian", n=1000, from=0, to=1000)
  return(dens$y)
}

dens_x <- function(dta){
  dta<-st_distance(dta, which = "Euclidean") 
  dta[lower.tri(dta, diag=TRUE)] <- NA
  dta<-c(dta)
  dta<-dta[!is.na(dta)]
  dens<-density(dta, bw="nrd0", kernel="gaussian", n=1000, from=0, to=1000)
  return(dens$x)
}

distancias_x <- dens_x(restaurants_top_2004) #Todas van de 0 a 1000

#Extraemos los barrios donde se hace el sampling
barrios_top <- barrios %>%
  filter(zona180%in%top5_barrios$zona180) %>%
  st_transform(crs = 6875) # Misma CRS de las distancias originales

# Densidades reales
for (b in top5_barrios$zona180){
  
  restaurants_2004_b <- restaurants_top_2004 %>%
    filter(zona180==b)
  
  restaurants_2012_b <- restaurants_top_2012 %>%
    filter(zona180==b)
  
  densidad_2004_b <- density_calculator(restaurants_2004_b)
  densidad_2012_b <- density_calculator(restaurants_2012_b)  

  assign(paste0("densidad_restaurantes_2004_barrio_", b), densidad_2004_b)
  assign(paste0("densidad_restaurantes_2012_barrio_", b), densidad_2012_b)
}

# 4) Contrafactual: Muestras (sin reemplazo) de los posibles restaurantes y sus densidades, intervalos de confianza

set.seed(1975)

### Muestras para 2004

for (b in top5_barrios$zona180){
  
  #Elementos necesarios
  sample_list <- list()
  restaurants_b <- restaurants_top_2004 %>%
    filter(zona180==b)
  
  #Parametros para el resampling:
  size_restaurants_b <- nrow(restaurants_b)
  barrio_b <- barrios_top %>%
    filter(zona180==b)
  barrio_b <- unique(barrio_b$geometry)[[1]]
  
  #Loop con resampling
  for (s in 1:1000) {
    sample_list[[s]] <- st_sample(barrio_b, size = size_restaurants_b, type = "random")
  }
  
  #Densidad
  densidades_samples <- lapply(sample_list, density_calculator)
  contrafactuales <- do.call(cbind, densidades_samples) 
  
  #Intervalos
  lower <- apply(contrafactuales, 1, function(x)quantile(x, probs=0.05))
  upper <- apply(contrafactuales, 1, function(x)quantile(x, probs=0.95))
  
  intervalos_b <- data.frame(lower = lower, upper = upper)
  assign(paste0("intervalos_2004_barrio_", b), intervalos_b)
  
}

### Muestras para 2012

for (b in top5_barrios$zona180){
  
  #Elementos necesarios
  sample_list <- list()
  restaurants_b <- restaurants_top_2012 %>%
    filter(zona180==b)
  
  #Parametros para el resampling:
  size_restaurants_b <- nrow(restaurants_b)
  barrio_b <- barrios_top %>%
    filter(zona180==b)
  barrio_b <- unique(barrio_b$geometry)[[1]]
  
  #Loop con resampling
  for (s in 1:1000) {
    sample_list[[s]] <- st_sample(barrio_b, size = size_restaurants_b, type = "random")
  }
  
  #Densidad
  densidades_samples <- lapply(sample_list, density_calculator)
  contrafactuales <- do.call(cbind, densidades_samples) 
  
  #Intervalos
  lower <- apply(contrafactuales, 1, function(x)quantile(x, probs=0.05))
  upper <- apply(contrafactuales, 1, function(x)quantile(x, probs=0.95))
  
  intervalos_b <- data.frame(lower = lower, upper = upper)
  assign(paste0("intervalos_2012_barrio_", b), intervalos_b)
  
}

# 5) Resultados test -----------------------------------------------------------

#Concentraciones en 2004


for (b in barrios_top$zona180){
  
  #Dataframes
  intervalos <- eval(parse(text = paste0("intervalos_2004_barrio_", b)))
  df_densidades_2004 <- data.frame(distancia = distancias_x, 
                                   densidad_real = eval(parse(text = paste0("densidad_restaurantes_2004_barrio_", b))))
  df_densidades_2004 <- cbind(df_densidades_2004, intervalos)
  
  #Grafica
  resultados_2004 <- ggplot(df_densidades_2004, aes(x = distancias_x)) + 
    geom_line(aes(y = densidad_real)) +
    geom_line(aes(y = lower), linetype = "dashed", alpha = 0.7) +
    geom_line(aes(y = upper), linetype = "dashed", alpha = 0.7) +
    ylab(paste0("Densidad en el barrio ", b, "\n(2004)")) +
    xlab("Distancia (m)") +
    scale_y_continuous(limits = c(NA, 0.0035)) +
    theme_bw()
  
  proj <- paste0("outputs/Resultados 2004 b_", b, ".png" )
  ggsave(proj, resultados_2004, dpi = 300)

}

#Concentracion en 2012

for (b in barrios_top$zona180){
  
  #Dataframes
  intervalos <- eval(parse(text = paste0("intervalos_2012_barrio_", b)))
  df_densidades_2012 <- data.frame(distancia = distancias_x, 
                                   densidad_real = eval(parse(text = paste0("densidad_restaurantes_2012_barrio_", b))))
  df_densidades_2012 <- cbind(df_densidades_2012, intervalos)
  
  #Grafica
  resultados_2012 <- ggplot(df_densidades_2012, aes(x = distancias_x)) + 
    geom_line(aes(y = densidad_real)) +
    geom_line(aes(y = lower), linetype = "dashed", alpha = 0.7) +
    geom_line(aes(y = upper), linetype = "dashed", alpha = 0.7) +
    ylab(paste0("Densidad en el barrio ", b, "\n(2012)")) +
    xlab("Distancia (m)") +
    scale_y_continuous(limits = c(NA, 0.0035)) +
    theme_bw()
  
  proj <- paste0("outputs/Resultados 2012 b_", b, ".png" )
  ggsave(proj, resultados_2012, dpi = 300)
  
}

