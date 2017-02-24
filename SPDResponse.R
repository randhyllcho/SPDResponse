library(tidyverse)
library(lubridate)
library(ggmap)

responses <- read.csv("spdResponse.csv", header = TRUE, stringsAsFactors = F)
str(responses)

responses$At.Scene.Time <- mdy_hms(responses$At.Scene.Time)

ggplot(responses, aes(as.factor(Event.Clearance.Group))) +
  geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Event Clearence Group")

responses %>% filter(Event.Clearance.Group=="SUSPICIOUS CIRCUMSTANCES")

ggplot(responses %>% filter(Initial.Type.Description == "CHILD - LURING"), aes(Longitude, Latitude)) +
  geom_point()

ggmap(seaMap) +
  geom_point(data = responses %>% filter(Initial.Type.Description == "CHILD - LURING"), aes(Longitude, Latitude))

gatherings <- responses %>% filter(Event.Clearance.Group == "PUBLIC GATHERINGS") 
gatherings <- gatherings %>% separate(At.Scene.Time, c("Year", "Month", "Day", "Hour", "minute", "Second"))
dat <- gatherings %>% filter(Event.Clearance.Description == "DEMONSTRATION MANAGEMENT (Control tactics used)") %>% group_by(Year) 
ggplot(gatherings, aes(Longitude, Latitude, col = Year)) +
  geom_point()

seattle <- geocode("seattle")

seaMap <- get_map(seattle, zoom = 10)
ggmap(seaMap) +
  geom_density2d(data = gatherings, aes(Longitude, Latitude, col = Year), size = 2, alpha = 0.7)

