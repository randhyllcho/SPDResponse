library(tidyverse)
library(lubridate)
library(ggmap)
library(ggthemes)
library(RColorBrewer)

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
data <- gatherings %>% filter(Event.Clearance.Description == "DEMONSTRATION MANAGEMENT (Control tactics used)") %>% group_by(Year) 
ggplot(gatherings, aes(Longitude, Latitude, col = Year)) +
  geom_point()

seattle <- geocode("seattle")

seaMap <- get_map(seattle, zoom = 11)
ggmap(seaMap) +
  geom_point(data = gatherings, aes(Longitude, Latitude, col = Year), size = 2, alpha = 0.7)

ggplot(responses %>% filter(Initial.Type.Subgroup == "MOTOR VEHICLE COLLISION INVESTIGATION", At.Scene.Time <= "2016-12-31"), aes(Longitude, Latitude)) +
  geom_point(alpha = 0.1, col = "#331A00") +
  theme_classic() +
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        aspect.ratio = 4/3,
        legend.text = element_text(size = 7),
        legend.position = "bottom", 
        strip.background = element_blank(),
        strip.text = element_blank()) 


dat <- responses %>% select(Initial.Type.Subgroup, At.Scene.Time) %>% filter(Initial.Type.Subgroup == "MOTOR VEHICLE COLLISION INVESTIGATION")
head(dat[,2])
dat$DayOfWeek <- wday(dat[,2], label = TRUE)
dat$Month <- month(dat[,2], label = TRUE)
dat$ToD <- hour(dat[,2])
dat$DoM <- as.factor(day(dat[,2]))
str(dat)

colorCount <- length(unique(dat$ToD))
bigPalette <- colorRampPalette(brewer.pal(9, "Spectral"))
myPal <- bigPalette(colorCount)

ggplot(dat, aes(DayOfWeek, fill = as.factor(ToD))) +
  geom_bar(stat = "count") +
  facet_wrap(~Month, ncol = 4) +
  scale_fill_manual(values = myPal, name = "Time Of Day") +
  theme_few() +
  theme(axis.text.x = element_text(size = 9),
        legend.position = "right",
        legend.direction = "vertical",
        axis.title.x = element_text(hjust = 0, size = 15),
        text = element_text(family = "mono"),
        title = element_text(size = 20),
        legend.title = element_text(size = 12)) +
  labs(x = "Crashes by Day and Hour", y = "", title = "Car crash distribution in Seattle 2016")

ggplot(dat, aes(ToD, fill = DayOfWeek)) +
  geom_bar(stat = "count", position = "fill") +
  facet_wrap(~Month, ncol = 4) +
  labs(x = "Time Of Day", y = "") +
  scale_x_continuous(breaks = seq(0,24,by=1)) +
  scale_fill_brewer(type = "seq", palette = "YlGnBu") +
  theme_few() +
  theme(axis.text.x = element_text(size = 6),
        text = element_text(family = "mono"))

ggplot(dat, aes(DayOfWeek, fill = as.factor(ToD))) +
  geom_bar(stat = "count") +
  scale_fill_manual(values = myPal, name = "Time Of Day") +
  theme_few() +
  theme(axis.text.x = element_text(size = 9),
        legend.position = "right",
        legend.direction = "vertical",
        axis.title.x = element_text(hjust = 0, size = 15),
        text = element_text(family = "mono"),
        title = element_text(size = 20),
        legend.title = element_text(size = 12)) +
  labs(x = "Crashes by Day and Hour", y = "", title = "Car crash distribution in Seattle 2016")

