# 1.0 Library
library(tidyverse)
library(ggtext)
library(showtext)
library(sysfonts)
library(extrafont)
library(shadowtext)

# 2.0 Fonts Setting
font_add_google("Signika", family = "Signika")
font_add_google("Roboto", family = "Roboto")
showtext::showtext_auto()
showtext_opts(dpi = 300)
font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')


# 3.0 Data
event_path = data.frame(event = list.files("data",pattern = "hm"))
event_path = event_path %>% separate(event, into = c("event_org","run_type","year","ext"),sep = "_",remove = F)

event_data_list = list()
for(i in 1:nrow(event_path)){
  x = read_csv(paste0("data/",event_path$event[i])) %>% mutate(event = event_path$event_org[i])
  event_data_list[[event_path$event_org[i]]] = x
}

# Main Data
event_df = bind_rows(event_data_list) %>% 
  rename("distance_km" = 'Distance from Start (km)',
         "height_sealvl_m" = 'Height Above Sea Level (m)',
         "comments" = 'Comments') 

# Image for Facet
#https://umairdurrani.com/posts/images_in_facets/images_in_facets
img_list = list.files("logo/",pattern = "jpg|png")
img_names = tools::file_path_sans_ext(img_list)

img_list = paste0("<img src=logo/",img_list," width='100'/>")
names(img_list) = img_names

# Text For Vis

teks_df = data.frame(event = c("twincity","kuching","borneo","score","klscm","pb","lihm"),
                     lokasi = c("Cyberjaya-Putrajaya","Kuching","Kota Kinabalu","Putrajaya","Kuala Lumpur","Pulau Pinang","Langkawi"),
                     eg = c(213,52,81,143,343,42,81),
                     x = rep(18,7),
                     y = rep(80,7))

# Elevation 

# ele_df = event_df %>% group_by(event) %>% mutate(diff = height_sealvl_m-lag(height_sealvl_m),
#                                                    diff = ifelse(is.na(diff),0,diff),
#                                                    pos_gain = ifelse(diff>0,diff,0),
#                                                    pos_loss = ifelse(diff<0,diff,0),
#                                                    cs = cumsum(diff)) %>% 
#   summarise(total_dif= sum(diff),
#             total_pos_gain = sum(pos_gain),
#             total_pos_loss = sum(pos_loss)) %>% 
#   mutate(teks_gain = paste0("+",total_pos_gain,"m"),
#          teks_loss = paste0(total_pos_loss,"m"),
#          ) %>% 
#   select(event,teks_gain,teks_loss)

# Combine

event_df = left_join(event_df,teks_df) %>% #left_join(.,ele_df) %>% 
  mutate(event = factor(event,levels = c("twincity","kuching","borneo","score","klscm","pb","lihm")))

# 4.0 Labels
title = "Course Profile of <span style = 'color: #14213d;'>Malaysia's </span><br><span style = 'color: red;'>Half Marathon</span> in 2023"
subtitle = "As running events are starting to get more popular across the country,<br>we take a look at the elevation on some of the selected Half Marathon events.<br>"
caption = paste0("<span><br><span><span>Data: plotaroute.com [DEM] | Subject to available routes on the website | Credits to contributors</span><br>",
                 "<span>* Elevation Gain ignores small changes in elevation under 10m<br>",
                 "<span style='font-family: \"fa-brands\"'>&#xf099; </span> @zubairwh | ",
                 "<span style='font-family: \"fa-brands\"'>&#xf09b; </span> abdullahzubairwan ")

# 5.0 Plot

z2 = ggplot(event_df,aes(x = distance_km,y = height_sealvl_m,color = event,fill = event))+
  geom_area(fill = "#003566",color = 'black')+
  geom_text(
    # aes(x = x, y = y,label = paste0(lokasi," (",teks_gain," | ",teks_loss,")")),
    aes(x = x, y = y,label = paste0(lokasi,"\nElevation Gain: +",eg,"m")),
    size = 5,
    color = 'black',
    # fontface = 'bold',
    # bg.color = 'grey10'
  )+
  facet_wrap(~event,ncol = 1,strip.position = 'left',labeller = as_labeller(img_list))+
  scale_y_continuous(position = "right",breaks = c(50))+
  theme_bw()+
  theme(
    
    #titles,sub,etc
    plot.title = element_markdown(size = 40,face = 'bold',family = 'Signika'),
    plot.subtitle = element_markdown(size = 18,family = 'Signika'),
    plot.caption = element_textbox_simple(size = 15,color = 'black'),
    axis.title = element_markdown(size = 15,face = 'bold',family = 'Roboto'),
    axis.text = element_markdown(size = 10,face = 'bold',family = 'Roboto'),
    
    #panels
    panel.grid.minor = element_blank(),
    # panel.grid.major = element_blank(),
    axis.line.x.top = element_blank(),
    
    # Strips
    strip.background = element_rect(fill = NA,color = NA),
    strip.text.y.left = element_markdown(size = 20,angle = 0),
    
    #Background
    # plot.background = element_rect(fill = "#2a9d8f"),
    
    #Margins
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5),"inches"),
    
    #Legends
    legend.position = 'none'
    
  )+
  labs(y = "Height Above Sea Level (m)",
       x = "Distance (km)",
       title = title,subtitle = subtitle,caption = caption)



ggsave(
  plot = z2,
  filename = paste0("mys_hm_cp.jpg"),
  width = 12, height = 15,
  device = 'jpeg',
  units = "in",
  scale = 1
)
