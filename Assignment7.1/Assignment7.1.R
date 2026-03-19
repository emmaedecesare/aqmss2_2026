library(tidyverse)
library(dplyr)
install.packages("modelsummary")
install.packages("marginaleffects")
library(ggplot2)
library(broom)
install.packages("sf")
install.packages("spData")
library(spData)
data(world)

df = world
view(world)

#------------------------------------------------------------------------------
                                          #1
#------------------------------------------------------------------------------
class(world)
names(world)
nrow(world)

#Geography column depicts the polygons and the locations of each unit. In this
  #case, the geography column consists of multipolygons for the locations of 
  #each country.

sf::st_crs(world)

#4326 is the EPSG code, meaning they use lat/long corrdinates
#WGS84 is the "World Geodetic System 1984"

sf::st_geometry_type(world)
unique(sf::st_geometry_type(world))

#Multipolygons are units that have multiple polygons that are not attahced to
  #each other. For example, the United States with Alaska, Puerto Rico, Hawaii,
  #etc. and Spain with the Canary Islands and other islands.

plot(world)
pdf("world_gdp_base.pdf")
plot(world["gdpPercap"])
dev.off()

#Africa seems to be the poorest and North America, Europe and Australia seem to
  #be among the richest

#------------------------------------------------------------------------------
                                        #2
#------------------------------------------------------------------------------

africa = filter(world, continent == "Africa")
nrow(africa)
plot(africa["gdpPercap"])
plot(gdp_by_continent)

world = world %>%
  mutate(pop_millions = pop / 1e6)

gdp_by_continent = world %>%
  group_by(continent) %>%
  summarize(mean_gdpPercap = mean(gdpPercap, na.rm = TRUE))

print(gdp_by_continent)
print(sf::st_drop_geometry(gdp_by_continent))

africa_sorted = africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap)

print(head(sf::st_drop_geometry(africa_sorted), 5))

#Equatorial Guinea, Gabon, Libya, Botswana and Algeria

#------------------------------------------------------------------------------
                                      #3
#------------------------------------------------------------------------------

crs <- "+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

ggplot(world) + 
  geom_sf(fill = "red", color = "white", linewidth = 0.3) +
  coord_map() +
  coord_sf(crs = crs)

centroids = sf::st_centroid(world)

ggplot()+
  geom_sf(data = world) +
  geom_sf(data = centroids, color = "red")

plot1 <- ggplot(world) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country")
plot1

ggsave("world_gdp.pdf", plot1, width = 10, height = 5)

plot2 <- ggplot(africa) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country")
plot2
ggsave("africa_gdp.pdf", plot2, width = 10, height = 5)

#The middle of the African continent has a lower GDP, whereas the northern most
  #and southern most countries have higher GDP per capita

plot3 <- ggplot(africa) +
  geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country")
plot3
ggsave("africa_gdp_borders.pdf", plot3, width = 10, height = 5)

#It helps define the country boundaries to differentiate between countries with
  #the same GDP per capita levels. Otherwise, they all kind of blend together.