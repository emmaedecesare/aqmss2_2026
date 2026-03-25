library(tidyverse)
library(dplyr)
install.packages("modelsummary")
install.packages("marginaleffects")
library(ggplot2)
library(broom)
install.packages("sf")
library(sf)
install.packages("spData")
library(spData)
data(world)

df = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/spatial/conflict_events.csv")

#------------------------------------------------------------------------------
                                          #1
#------------------------------------------------------------------------------
df2 = st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

class(df2$geometry)
st_crs(df2$geometry)

#The coords argument signifies the longitude and latitude of the coordinates of 
  #each event. crs = 4326 means that we are using the World Geodetic System 1984
  #Ensemble as the way to understand location (longitude and latitude)


nrow(df2)
table(df2$event_type)

#The state-based event type is the most common


plot1 = ggplot()+
  geom_sf(data = world, color = "gray") +
  geom_sf(data = df2, aes(fill = event_type, color = event_type))
plot1

ggsave("plot1_Assignment7.2.png", plot1, width = 6, height = 4)

#The conflicts are only in Africa


#------------------------------------------------------------------------------
                                          #2
#------------------------------------------------------------------------------

st_crs(world)

df3 = st_join(df2, world, join = st_intersects)

nrow(df3)
nrow(df2)

#It uses the geometry column to determine the location of each event and matches
  #it to the second dataset. Checking the CRS is important because it determines
  #how each dataset depicts the location, so if they were inconsistent, the 
  #locations wouldn't match and it would be incorrect.


sum(is.na(df3$name_long))
sum(is.na(df3$name_long)) / nrow(df3)
#1576 / 68354 or 2.3% of events have no matching country, it could have occurred
  #at sea or on a border.


#Count the number of events and total fatalities per country. Hint: filter out 
#events with no matching country, then use group by() and summarise() with n() and sum()

df3 = subset(df3, !is.na(name_long))

events_sorted = df3 %>%
  group_by(name_long) %>%
  summarize(
    num_events = n(),
    num_fatalities = sum(fatalities),
  )

events_sorted = events_sorted %>%
  arrange(desc(events_sorted[2]))

print(head(st_drop_geometry(events_sorted), 10))

#Yes, this is consistent with prior knowledge


#------------------------------------------------------------------------------
                                          #3
#------------------------------------------------------------------------------

df4 = st_join(world, events_sorted, join = st_intersects)

df4 = df4 %>%
  mutate(
    num_events = replace_na(num_events, 0),
    num_fatalities = replace_na(num_fatalities, 0)
  )

nrow(world)
nrow(df4)


plot2 = ggplot() + 
  geom_sf(data = df4, aes(fill = num_events)) + 
  scale_fill_distiller(
    palette = "Reds", 
    name = "Number of Events") +
  theme_void() + 
  labs(title = "Conflict Events Count by Country")
plot2

ggsave("plot2_Assignment7.2.png", plot2, width = 6, height = 4)

#It does, all of the events occurred in Africa.


plot3 = ggplot() + 
  geom_sf(data = df4, aes(fill = log1p(num_events))) + 
  scale_fill_distiller(
    palette = "YlOrRd", 
    direction = 1,
    name = "Log(events + 1)") +
  theme_void() + 
  labs(title = "Conflict Log Map")
plot3

ggsave("conflict_log_map.pdf", plot3, width = 6, height = 4)

#------------------------------------------------------------------------------
                                          #5
#------------------------------------------------------------------------------

#These models don't take into account events that occur on the borders of 
  #countries or events that take place in the ocean or sea, so it is biased. I
  #might change the shape and mass of the polygons to include the events that
  #occur just outside of boundaries or on boundaries, depending on how you'd
  #classify them.

#left_join keeps all observations in the 'x' data frame, whereas st_join 
  #combines the two data frames. left_join combines the data sets so that there 
  #is no overlapping variables, but st_join just adds all, even if they are the 
  #same. Therefore there may be two of the same column. So if you have two
  #datasets with the same column you would prefer left_join over st_join.
  
  
  
  
  
  
  
  
  
  