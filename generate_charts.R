load("./data/climo_data.Rdata")

library(shiny)
library(tidyverse)
library(stringr)
library(htmltools)
library(forcats)
library(leaflet)
library(readr)
library(scales)
library(plotbiomes)
library(mapview)

clean_data <- clean_data %>%
    mutate(Species = if_else(Species  == "Picea abies, Abies alba, Fagus sylvatica" , "Picea abies; Abies alba; Fagus sylvatica", Species))

clean_data %>% 
    ggplot() +
    # geom_polygon(data = Whittaker_biomes,
    #              aes(x    = temp_c,
    #                  y    = precp_cm,
    #                  fill = biome),
    #              # adjust polygon borders
    #              colour = "gray98",
    #              size   = 1) +
    
    geom_point(aes(x=Temp, y=Prec), color="steelblue", size   = 3,
               shape  = 21,  colour = "gray95",  fill   = "steelblue",   stroke = 1,
               alpha  = 0.5) +
    geom_point( aes(x=Temp, y=Prec), color="red", size=4, alpha = 0.7) +
    # scale_y_continuous(breaks= pretty_breaks()) +
    scale_fill_manual(name   = "Whittaker biomes",
                      breaks = names(Ricklefs_colors),
                      labels = names(Ricklefs_colors),
                      values = Ricklefs_colors) +
    ylab("Precipitation (mm)") + xlab("Temperature (ºC)") +
    theme_light() + 
    theme(legend.justification = c(0, 1), # pick the upper left corner of the legend box and
          legend.position = c(0, 1), # adjust the position of the corner as relative to axis
          legend.background = element_rect(fill = NA), # transparent legend background
          legend.box = "horizontal", # horizontal arrangement of multiple legends
          legend.spacing.x = unit(0.5, units = "cm"), # horizontal spacing between legends
          panel.grid = element_blank(),
          axis.text = element_text(size=10),
          axis.title = element_text(size=12),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank())

#  Mapa

    map <- leaflet(clean_data) %>%
        addProviderTiles(providers$Stamen.Terrain) %>%
        setView(lat = 55, 
                lng = 8,
                zoom = 4) %>%
        addCircles(color = "red",group = "Plots",radius = 3.5, fillOpacity = 0.8)
    mapshot(map, file = "figs/map.png")
    
    # Species
    hey <- data.frame(Species=factor(clean_data %>%
                                         pull(Species) %>%
                                         strsplit(split = ";.", ",") %>%

                                         unlist()))
    
species <-     hey %>%
        count(Species) %>%
        arrange(desc(n)) %>%
    slice(1:20) %>%
arrange(n) %>%

        mutate(Species = factor(Species, Species)) %>%
        ggplot() +
        geom_segment(aes(x=Species, xend=Species, y=0, yend=n), color="grey") +
        geom_point( aes(x=Species, y = n), color="steelblue", size=3) +
        # geom_bar(aes(x = Species), stat= "count", fill="steelblue") +
        scale_y_continuous(breaks= pretty_breaks()) +
        ylab("Number of plots") +
        coord_flip( ) +
        theme_light() + 
        theme(axis.text = element_text(size=12),
              axis.title = element_text(size=14),
              panel.grid.major.y = element_blank(),
              panel.border = element_blank(),
              axis.ticks.y = element_blank())

# Countries
countries <-    clean_data %>%
        count(Country) %>%
        arrange(n) %>%
        mutate(Country = factor(Country, Country)) %>%
        ggplot() +
        geom_segment(aes(x=Country, xend=Country, y=0, yend=n), color="grey") +
        geom_point( aes(x=Country, y = n), color="dark green", size=3) +
        #geom_bar(aes(x = Country), stat= "count", fill="dark green") +
        scale_y_continuous(breaks= pretty_breaks()) +
        ylab("Number of plots") +
        coord_flip( ) +
        theme_light() + 
        theme(axis.text = element_text(size=12),
              axis.title = element_text(size=14),
              panel.grid.major.y = element_blank(),
              panel.border = element_blank(),
              axis.ticks.y = element_blank())


    # Elevation
elevation <-    
    ggplot(clean_data)+
       geom_histogram(aes(Elevation), fill= "steelblue", binwidth = 100) +
       theme_bw() + xlab("Elevation")+

       ylab("Number of plots") +
       theme(axis.text = element_text(size=12),
             axis.title = element_text(size=14))

#§ Year
   
year <-  ggplot(clean_data)+
       geom_histogram(aes(Ini_year), fill="dark green" , binwidth = 5) +
       theme_bw() + xlab("Year of establishment")+ 
       ylab("Number of plots") +

       theme(axis.text = element_text(size=12),
             axis.title = element_text(size=14))

library(patchwork)

pdf("figs/figura.pdf", width = 9, height = 9)
species  + labs(tag = 'A') +
    elevation + labs(tag = 'B') +
    countries + labs(tag = 'C') +
    year + labs(tag = 'D') 
dev.off()
