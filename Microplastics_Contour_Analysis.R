###########Package use#####################

library(tidyverse)
library(patchwork)
library(ggmap)
library(askgpt)
library(akima)
library(broom)


###############Read in the data##################: 
df <- read_csv("MP_DF.csv")

df$Distance_from_coast_meter = as.factor(df$Distance_from_coast_meter)
df$Sites = as.factor(df$Sites)
df$Stations = as.factor(df$Stations)

str(df)
df



###############Creating Contour Map#################
register_google(key = 123)

#Lajas Map
Lajas <- c(-67.059205, 17.932439,-67.0421317,17.979919)

Parguera_map <- ggmap(get_map(location = Lajas, zoom = 14, maptype = "satellite", extent = "device", legend = "bottom"))


Interp_Data <- with(df, interp(x = lon_W, y = Lat_N, z = Number_micro_debris,
                               xo=seq(min(lon_W), max(lon_W), length = 5000), 
                               yo=seq(min(Lat_N), max(Lat_N), length = 5000),
                               linear=FALSE))

ITP_Data =tidy(Interp_Data)

parguera_contour <- Parguera_map+
  geom_contour_filled(data = ITP_Data, aes(x = x, y = y, z = z), na.rm = T, alpha = 0.5, show.legend = F, bins = 30)+
  geom_point(data = df, aes(x = lon_W, Lat_N, color = Number_micro_debris), size = 3.8, alpha = 0.7)+
  scale_color_gradient(low = "#ffeda0", high = "#f03b20", name = "Number of Microplastics")+
  scale_fill_manual(values = c( "#fed976","#fed976","#fed976","#fed976","#fed976",
                                         "#feb24c","#feb24c","#feb24c","#feb24c","#feb24c",
                                         "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c",
                                         "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a",
                                         "#e31a1c","#e31a1c","#e31a1c","#e31a1c","#e31a1c",
                                         "#b10026","#b10026","#b10026","#b10026","#b10026"))+
                                           theme(axis.title.x = element_blank(),
                                                 axis.text.x = element_blank(),
                                                 axis.ticks.x = element_blank(),axis.title.y = element_blank(),
                                                 axis.text.y = element_blank(),
                                                 axis.ticks.y = element_blank(), 
                                                 legend.position = "top"
                                           )

#Guanica Map

####################Other Plots and Analysis########################

# Plotting (Number_of_MP vs Distance_from_coast)
# Create a plot with Regresion of Guanica Turbidiy Data vs MP concentration, and try to tell what model fits best the data for prediction. 
#Seasons:
#Rainy Season – April through November.
#Dry Season – December through March.
#Hurricane Season – June to November.

#pring - March 1 to May 31;
#ummer - June 1 to August 31;
#all - September 1 to November 30
#inter  December 1 to February 28#