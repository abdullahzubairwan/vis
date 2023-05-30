# Title: Visualization of Malaysian Rivers
# Version: 1.0
# Author: Zubs
# Date: 28 May 2023
# Edit: -

# 1.0 Libraries
# Essentials
library(tidyverse)
library(osmdata)
library(sf)
library(mapview)
library(geodata)
library(ggplot2)
# Spatial Libraries
library(basemaps)
library(terra)
library(tidyterra)
# Plotting Cosmetics
library(ggtext)
library(showtext)
library(sysfonts)
library(extrafont)
library(countrycode)
library(ggimage)

# 2.0 Setup
# Data
mys_water = st_read("data/waterway/hotosm_mys_waterways_lines.shp")
mys_boundary = st_read("data/administrative/gadm41_MYS_1.json")

# Fonts/Logo
font_add_google("Raleway", family = "Raleway")
#must install font awesome brands locally
font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
fonttable()
showtext_auto()

# 3.0 Filter
mys_select = mys_water %>% filter(waterway %in% c("river"))

mys_notna = mys_select %>% filter(!is.na(name)) %>%
  mutate(label = 'Labelled')
mys_na = mys_select %>% filter(is.na(name)) %>% mutate(label = 'Unlabelled')

mys_river = bind_rows(mys_notna,mys_na)


smjg = mys_boundary %>% filter(!NAME_1 %in% c('Sabah','Sarawak','Labuan'))
borneo = mys_boundary %>% filter(NAME_1 %in% c('Sabah','Sarawak','Labuan'))

mys_river_smjg = st_filter(mys_river,smjg)
mys_river_borneo = st_filter(mys_river,borneo)

# 4.0 Visualization

# 4.1 Peninsular
# 4.1.1 Setting Up Labels
maintitle = "Malaysia's River Network in \nPeninsular Malaysia"
subtitle = "Makes up 47% of Malaysian Rivers\nSungai Golok is the only river that shares international border
Colors indicates whether the river is named in OpenStreetMap"

caption = paste0("<span><br><span><span>Data: OpenStreetMap | Source: wwf.org.my | Basemap : ESRI</span><br><span style='font-family: \"fa-brands\"'>&#xf099; </span> @zubairwh | ",
                 "<span style='font-family: \"fa-brands\"'>&#xf09b; </span> abdullahzubairwan ")
# 4.1.2 Setting up Basemap
x <- basemap_raster(smjg, map_service = "esri", map_type = "world_terrain_base")
x_terr <- rast(x)
# 4.1.3 Plot
mys_plot_river = ggplot()+
  geom_spatraster_rgb(data = x_terr) +
  geom_sf(data = smjg,fill = NA,color = 'black',linewidth = 0.3,alpha = 0.3)+
  geom_sf(data = mys_river_smjg,aes(color = label,fill = label),alpha = 0.5)+
  scale_color_manual(values = c("Labelled" = "blue",
                                "Unlabelled" = "red"
  ))+
  scale_fill_manual(values = c("Labelled" = "blue",
                               "Unlabelled" = "red"
                               
  ))+
  theme(
    # strip.text = element_markdown(face = "bold", color = "white"),
    plot.title.position = "plot",
    plot.title = element_text(size = 40, face = "bold",color = 'black',family = "Raleway"),
    plot.subtitle = element_text(size = 20,color = 'black'),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_textbox_simple(size = 15,color = 'black'),
    
    panel.background = element_rect(fill = "white",
                                    colour = "white"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    # legend.key = element_rect(fill = "black"),
    legend.background = element_rect(fill="white"),
    legend.position = "bottom",
    legend.title = element_blank(),
    # legend.title = element_text(size = 13,face = "bold",color = 'white'),
    legend.text = element_text(size = 12,color = 'black'),
    # legend.key.height = unit(1,'cm'),
    # legend.key.width =  unit(1,'cm')
  ) +
  guides(fill=guide_legend(title="Labelling"),
         color = guide_legend(title="Labelling")) +
  labs(
    title = maintitle,
    subtitle = subtitle,
    caption = caption)+
  xlab("")+ylab("")

mys_plot_river

# 4.2 East Malaysia
# 4.2.1 Setting up Labels
maintitle = "Malaysia's River Network in \nEast Malaysia"
subtitle = "Sabah makes up 78% of the rivers in East Malaysia\nSungai Rajang is 563km long which makes it the longest river in Malaysia
Colors indicates whether the river is named in OpenStreetMap"
# 4.2.2 Setting up Basemap
x <- basemap_raster(borneo, map_service = "esri", map_type = "world_terrain_base")
x_terr <- rast(x)
# 4.2.3 Plot
mys_plot_river = ggplot()+
  geom_spatraster_rgb(data = x_terr) +
  geom_sf(data = borneo,fill = NA,color = 'black',linewidth = 0.3,alpha = 0.3)+
  geom_sf(data = mys_river_borneo,aes(color = label,fill = label),alpha = 0.5)+
  scale_color_manual(values = c("Labelled" = "blue",
                                "Unlabelled" = "red"
  ))+
  scale_fill_manual(values = c("Labelled" = "blue",
                               "Unlabelled" = "red"
                               
  ))+
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 40, face = "bold",color = 'black',family = "Raleway"),
    plot.subtitle = element_text(size = 20,color = 'black'),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_textbox_simple(size = 15,color = 'black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.background = element_rect(fill="white"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12,color = 'black')
  ) +
  guides(fill=guide_legend(title="Labelling"),
         color = guide_legend(title="Labelling")) +
  labs(
    title = maintitle,
    subtitle = subtitle,
    caption = caption)+
  xlab("")+ylab("")

mys_plot_river
