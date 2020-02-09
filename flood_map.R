rm(list=ls())
setwd("~/Desktop/Important/UBC_Stuff/Rieseberg Lab/Random")

#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

#library(cowplot)
#library(googleway)
library(ggplot2)
library(ggmap)
library(ggpubr)
#library(ggrepel)
#library(ggspatial)
#library(libwgeom)
#library(sf)
#library(rnaturalearth)
#library(rnaturalearthdata)

floods<-read.csv("floods_fema.csv")
years <- as.vector(unique(floods$Year))
years <- sort(years, decreasing = F)
vvv <- as.vector(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
out_plot <- data.frame(years,vvv)


years <- sort(floods$Year, decreasing = F)
years <- unique(years)
counter = 1
for(x in years){
  temp <- subset(floods, floods$Year == x)
  temp <- subset(temp, temp$Lon >= -165)
  temp <- subset(temp, temp$Lon <= -35)
  temp <- subset(temp, temp$Lat <= 70)
  temp <- subset(temp, temp$Lat >= 15)
  j = 1
  total_floods = 0
  while(j < nrow(temp) + 1){
   total_floods = total_floods + temp$NumEpisodes[j]
   j = j + 1
  }
  
  out_plot$vvv[counter] <- total_floods
  
  temp_v <- as.vector(c(NA, NA, NA, 15, -165, 0, NA))
  temp_v2 <- as.vector(c(NA, NA, NA, 70, -35, 0, NA))
  temp <- rbind(temp,temp_v,temp_v2)
  
  plot_map <- qmplot(Lon, Lat, data = temp, colour = I('#0080FF'), size = I(temp$NumEpisodes), alpha = I(0.15), zoom = 4, mapcolor = "color", margins = F) 
  
  plot_total <- ggplot(data = out_plot, aes(x = years, y = vvv))+
    geom_point(size = 3, col = "#0080FF") +
    geom_line(linetype = "dotted") +
    xlab("Year") +
    ylab("Floods") +
    labs(title = "Total Floods per Year") +
    ylim(0,10000) +
    theme(axis.text.x = element_text(size = 8, angle = 90),
          axis.text.y = element_text(size = 8),  
          axis.title.x = element_text(size = 9),
          axis.title.y = element_text(size = 9, angle = 90),
          plot.title = element_text(size = 12, hjust=0.5))
  
  lay <- rbind(c(1,1,1,1,2,2),
               c(1,1,1,1,2,2),
               c(1,1,1,1,2,2),
               c(3,3,3,3,2,2))
  
  title <- text_grob(x, size = 30, face = "bold")
  
  require(gridExtra)
  temp_outgraph <- grid.arrange(plot_map, plot_total, title, layout_matrix = lay)
  
  t <- paste("df",x,sep="")
  tf <- paste("total_",x,sep="")
  assign(t,temp)
  assign(tf,total_floods)
  
  temp_name <- paste(x, "_map.png",sep ="")
  setwd("~/Desktop")
  ggsave(temp_name, plot = temp_outgraph)
  
  counter = counter + 1
}

rm(dfNA)
rm(total_NA)
