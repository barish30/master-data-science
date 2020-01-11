#Descargamos los datos:
  
library(dslabs)
data(gapminder)
#si no tienes el paquete instalado
#gapminder=read.csv("DataSets/Gapminder.csv")

#Exploramos los datos un poco
View(gapminder)
head(gapminder)
str(gapminder)

# identifiquemos entre varias parejas de paises cuáles tienen la mayor mortalidad infantil en 2015?
  
  gapminder %>% 
    filter(year == 2015 & country %in% c("Sri Lanka","Turkey")) %>% 
    select(country, infant_mortality)
  gapminder %>% 
    filter(year == 2015 & country %in% c("Poland","South Korea")) %>%
    select(country, infant_mortality)
  gapminder %>% 
    filter(year == 2015 & country %in% c("Malaysia","Russia")) %>% 
    select(country, infant_mortality)
  gapminder %>% 
    filter(year == 2015 & country %in% c("Pakistan","Vietnam")) %>% 
    select(country, infant_mortality)
  gapminder %>% 
    filter(year == 2015 & country %in% c("Thailand","South Africa")) %>% 
    select(country, infant_mortality)
  
# Graficamos nuestro plot con ggplot

# Seleccionamos los años  
  years <- c(1962, 1980, 1990, 2000, 2012)

# Continentes a comparar
  continents <- c("Europe", "Asia")

#Seleccionamos los datos del dataframe
  gapminder %>% 
    filter(year %in% years & continent %in% continents) %>%
    
    #que elementos queremos graficar y definimos la columna
    ggplot( aes(fertility, life_expectancy, col = continent)) +
    geom_point() +
    facet_wrap(~year) 
  
#Creamos una tidy_data de nuestro DF seleccionando los elementos de interés:
  
  data("gapminder")
  tidy_data <- gapminder %>% 
    filter(country %in% c("South Korea", "Germany")) %>%
    select(country, year, fertility)
  
  head(tidy_data)
#Ploteamos nuestro tidy en ggplot
  tidy_data %>% 
    ggplot(aes(year, fertility, color = country)) +
    geom_point()