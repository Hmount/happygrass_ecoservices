#### Soil moisture data wrangling and initial summary plot
#### (code from happygrass-happyforb scripts)
#### TO DO: 
## remove probes/ ports with errors? (from Jules notes)


library(tidyverse)
library(ggpubr)

## WY (all values in m3/m3 VWC)
sm.wy21 <- read.csv("data/soil_moisture_wy/Restoration_soilmoisture_2021.csv")
sm.wy21 <- sm.wy21 %>% mutate(across(-1, as.numeric))
sm.wy22 <- read.csv("data/soil_moisture_wy/Restoration_soilmoisture_2022.csv")
sm.wy22 <- sm.wy22 %>% mutate(across(-1, as.numeric))
sm.wy23 <- read.csv("data/soil_moisture_wy/Restoration_soilmoisture_2023(toAug).csv")
sm.wy23 <- sm.wy23 %>% mutate(across(-1, as.numeric))
sm.wy24 <- read.csv("data/soil_moisture_wy/Restoration_soilmoisture_2024_all.csv")
sm.wy24 <- sm.wy24 %>% mutate(across(-1, as.numeric))
ranges <- sapply(sm.wy22, range, na.rm = T)
sm.wy <- bind_rows(sm.wy21,sm.wy22,sm.wy23, sm.wy24) #combine all annual soil moisture data
sm.wy <- sm.wy %>% mutate(Timestamp = mdy_hm(Measurement.Time))

#pivot longer and combine timestamps into daily soil moisture value
sm.wy.long <- sm.wy %>% pivot_longer(where(is.numeric), names_to = "probenumber", values_to = "Moisture")
sm.wy.long$Block <- gsub("X(\\d+)_Port\\d+", "\\1", sm.wy.long$probenumber)  # Extract block number
sm.wy.long$Port <- gsub(".*_Port([0-9]+)", "\\1", sm.wy.long$probenumber)   # Extract port number
# drought blocks
droughtblocks <- data.frame(Block= c(20,22,27,30,33,42,49,50,51,58),
                            trt = c("drt","drt","cntl","drt","cntl","cntl","cntl","drt","cntl","drt"))
sm.wy.long <- merge(sm.wy.long, droughtblocks)

sm.wy.long <- (sm.wy.long %>% mutate(Date = floor_date(Timestamp, unit = "day"))
               %>% select(Date, trt, Moisture,Block)
               %>% group_by(trt, Date)
               %>% summarize(Moisture = mean(Moisture, na.rm = TRUE))
               %>% arrange(trt, Date))

write.csv(sm.wy.long, "code/soil_moisture/clean_soilmoisture.csv", row.names = F)

### create pretty soil moisture figure 
smwy <- sm.wy.long %>% select(trt,Date,Moisture) #keep only relevant variables
smwy$Treatment <- as.factor(smwy$trt) #make factor

## Create pretty plot with correct colors, axes, and shade growing season
## separate out growing seasons in their own dataframes and re-combine
#Create a data.frame for the shaded growing season WY (May - August)
wyshade_data <- data.frame(
  xmin = as.POSIXct(c("2021-05-01", "2022-05-01", "2023-05-01", "2024-05-01"), tz = "UTC"),
  xmax = as.POSIXct(c("2021-09-01", "2022-09-01", "2023-09-01", "2024-09-01"), tz = "UTC"),
  ymin = -Inf,  # Extend shading to the bottom of the plot
  ymax = Inf    # Extend shading to the top of the plot
)
#create sequence to use for x axis breaks on both plots
x_breaks <- seq(as.POSIXct("2021-01-01"), as.POSIXct("2024-09-01"), by = "6 months")  # Example: every 6 months

# make WY plot
wysmplot <- ggplot(smwy, (aes(y=Moisture, x=Date, col=trt)))+
  geom_line()+
  # ylab(expression(
  #   Soil ~ volumetric ~ water ~ content ~ (m ^ 3 ~ m ^ -3)
  # ))+
  scale_x_datetime(
    limits = as.POSIXct(c("2021-01-01 00:00:00", "2024-09-01 00:00:00"), tz = "UTC"),
    date_labels = "%Y-%b",  # Optional: Custom labels like "2021-Jan"
    breaks = x_breaks
  )+
  scale_y_continuous(breaks=c(.10,.15,.20,.25,.30,.35))+
  labs(col="Precipitation 
Treatment", x=" ", y=" ")+
  scale_color_manual(values = c("blue","skyblue"), labels=c("Ambient", "Reduction"))+
  geom_rect(
    data = wyshade_data, 
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
    inherit.aes = F,  # Prevents inheriting aesthetics from the main plot
    fill = "grey18",        # Choose a shading color
    alpha = 0.2           # Set transparency
  ) +
  theme_minimal()+
  theme(legend.position = "right",
        legend.key.size = unit(0.5, "cm"),    # Reduce the size of legend keys
        legend.text = element_text(size = 8), # Make legend text smaller
        legend.title = element_text(size = 9),# Adjust title size (optional)
        legend.spacing.y = unit(0.2, "cm"),
        axis.ticks.x = element_line(size=.5))
