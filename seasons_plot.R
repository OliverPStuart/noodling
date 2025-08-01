
# Library
library(suncalc)
library(ggplot2)
library(magrittr)
library(dplyr)
library(patchwork)

# Set location
lat=51.8985;lon=-8.4756

# Dates
dates=as.Date(as.Date("2024-12-21"):as.Date("2025-12-20"))

# Get current date
today <- as.Date(Sys.Date())

# Sunlight data
sunlight <- getSunlightTimes(lat=lat,lon=lon,date=dates) %>%
  mutate(dayLength = sunset-sunrise,
         extraDayLength=(dayLength - lag(dayLength, default = last(dayLength)))*60)

# Extra objects
winter1 <- geom_rect(xmin=as.Date("2024-12-21"),xmax=as.Date("2025-03-01"),ymin=-Inf,ymax=Inf,
                     fill="lightblue",alpha=0.5,colour=NA)
spring <- geom_rect(xmin=as.Date("2025-03-01"),xmax=as.Date("2025-06-01"),ymin=-Inf,ymax=Inf,
                    fill="lightgreen",alpha=0.5,colour=NA)
summer <-   geom_rect(xmin=as.Date("2025-06-01"),xmax=as.Date("2025-09-01"),ymin=-Inf,ymax=Inf,
                      fill="gold",alpha=0.5,colour=NA)
autumn <-   geom_rect(xmin=as.Date("2025-09-01"),xmax=as.Date("2025-12-01"),ymin=-Inf,ymax=Inf,
                      fill="orange",alpha=0.5,colour=NA)
winter2 <-  geom_rect(xmin=as.Date("2025-12-01"),xmax=as.Date("2025-12-20"),ymin=-Inf,ymax=Inf,
                      fill="lightblue",alpha=0.5,colour=NA)
date_scale <- scale_x_date(limits = c(as.Date("2024-12-21"),as.Date("2025-12-20")),expand=c(0,0),
                           breaks=seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "1 month"),
                           labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

extra_day_length <- sunlight %>%
  ggplot(aes(x=date,y=extraDayLength)) + 
  winter1 + spring + summer + autumn + winter2 +
  geom_line(linewidth=1.5) + 
  theme_bw() + 
  date_scale + 
  geom_vline(xintercept=today,linewidth=1.3,colour="purple",linetype="dashed") + 
  labs(y="Extra minutes\nper day") +
  theme(axis.title.y=element_text(angle=0,vjust=0.5,hjust=1))

day_length <- sunlight %>%
  ggplot(aes(x=date,y=dayLength)) + 
  winter1 + spring + summer + autumn + winter2 +
  geom_line(linewidth=1.5) + 
  theme_bw() + 
  date_scale + 
  geom_vline(xintercept=today,linewidth=1.3,colour="purple",linetype="dashed") + 
  labs(y="Daylight\nhours") +
  theme(axis.title.y=element_text(angle=0,vjust=0.5,hjust=1))
  
extra_day_length / day_length + plot_layout(axes="collect")
  
