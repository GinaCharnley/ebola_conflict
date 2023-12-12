library(sf)
library(ggplot2)
library(geosphere)
library(tidyr)

batts <- subset(conf, year == 2021)
prots <- subset(batts, event_type == "Protests")
riots <- subset(batts, event_type == "Riots")
batts <- subset(batts, event_type == "Battles")

prots <- prots[c(3,4)]
riots <- riots[c(3,4)]
batts <- batts[c(3,4)]

names(prots)[1]<-paste("lat1")
names(riots)[1]<-paste("lat1")
names(batts)[1]<-paste("lat1")

names(prots)[2]<-paste("lon1")
names(riots)[2]<-paste("lon1")
names(batts)[2]<-paste("lon1")

expand <- crossing(riots,gui_pop)
coord.list <- split(expand, seq(nrow(expand)))

proximity <- function(x) {
  with(x, distm(c(lon1, lat1), c(lon2, lat2), distHaversine))
}  

results <- lapply(coord.list, proximity)

distances <- unlist(as.numeric(results))
mean(distances)

gui_map <- st_read("gin_admbnda_adm2_ocha.shp", stringsAsFactors = FALSE) 

ggplot(gui_map) + geom_sf(lwd = .2) + theme_void() + 
  geom_point(data = subset(conf, year == 2021), 
             aes(longitude, latitude, color = event_type, fill = event_type)) + 
  geom_point(data = gui_pop, 
             aes(lng, lat, size = population), alpha = .7) + 
  scale_color_brewer(palette = "Set2") + scale_fill_brewer(palette = "Set2") + 
  labs(fill = "Event Type 2021", color = "Event Type 2021", size = "Population") + 
  theme(text = element_text(face = "bold"))






