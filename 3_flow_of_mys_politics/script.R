# Libraries
library(rio)
library(tidyverse)
library(ggtext)
library(ggimage)
library(showtext)
library(sysfonts)
library(extrafont)
# Setting Font
font_add_google("Mulish", family = "Mulish")
fonttable()
showtext_opts(dpi = 300)
# Setting Icon
## must install font awesome brands locally
font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
# Setting Caption
caption = paste0("<span style='font-family: \"fa-brands\"'>&#xf099; </span> @zubairwh | ",
                 "<span style='font-family: \"fa-brands\"'>&#xf09b; </span> abdullahzubairwan ")

data_list <- import_list("data/mys_parties.xlsx")
# Get the years
get_year = names(data_list)[3:7]
# Create empty dataframe
parties_df = data.frame()
# Gather all data together
for(i in 1:length(get_year)){
  temp = data_list[[get_year[i]]]
  temp$year = get_year[i]
  parties_df = bind_rows(parties_df,temp)
}
#
rm(temp)
# Set Line Colours for Party
fill_col_party = c('MCA' = "gold", 
                   'UMNO' = "#d8232b", 
                   'MIC' = "purple", 
                   'AMANAH' = "#ee9b00", 
                   'PKR' = "#33D7E2",
                   'DAP' = '#E03475',
                   'PAS' = "#0CA220", 
                   'BERSATU' = "blue",
                   'GERAKAN' = "#7D7D7B")
# Minor Data Processing
parties_df2 = parties_df %>% separate(year,into = c('year','area'),sep = "-") %>%
  mutate(year = as.integer(year))
# Set Party Tags Labelling
parties_text_label = parties_df2 %>% filter(year == 2008) %>% mutate(year = 2010)
parties_text_label2 = data.frame(Party = c("BERSATU","AMANAH"),year = c(2018.5,2017.5),Value = c(0.85,0.85)) 
parties_text_label = bind_rows(parties_text_label,parties_text_label2)

# Set Flags Location
parties_flag_loc = parties_df2 %>% filter(year == 2008) %>% select(Party,Value,year)
parties_flag_loc2 = data.frame(Party = c('BERSATU','AMANAH'),Value = c(0.7,0.7),year = c(2018.5,2017.5))
parties_flag_loc = bind_rows(parties_flag_loc,parties_flag_loc2)
parties_flag_loc = parties_flag_loc %>% mutate(src = paste0("partiflag/",Party,".png"))

rm(parties_text_label2,parties_flag_loc2)

# Plot
z = ggplot(parties_df2,aes(x = year,y = Value,group = Party)) +
  geom_point(aes(color = Party))+
  geom_line(linewidth = 1.2,position=position_dodge(w=0.02),aes(color = Party))+
  # Add Party Flag
  geom_image(data = parties_flag_loc,aes(x = year,y = Value,image = src),size = 0.07)+
  scale_y_reverse()+
  # Adding Borders between Y Axis Labels
  geom_hline(yintercept = 2.5,color = 'black')+
  geom_hline(yintercept = 1.5,color = 'black')+
  scale_color_manual(values = fill_col_party)+
  # Adding Party Tags
  geom_label(data = parties_text_label,aes(x = year,y = Value,label = Party),size = 3,color = 'black')+
  theme_bw()+
  theme(plot.title = element_text(size = 30,face = 'bold',family = 'Mulish'),
        plot.subtitle = element_markdown(),
        plot.caption = element_textbox_simple(size = 15,color = 'black'),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.94,0.8),
        legend.box.background = element_rect(color="black", linewidth=1),
    
        )+
  guides(color = guide_legend(override.aes = list(size = 0.1)))+
  # Relabel Y Axis
  annotate("text", x = 2007, y = 1.1, label = "Government",angle = 90,size = 4) +
  annotate("text", x = 2007, y = 2, label = "Unity Government",angle = 90,size = 4) +
  annotate("text", x = 2007, y = 3, label = "    Opposition",angle = 90,size = 4) +
  coord_cartesian(xlim = c(2008,max(parties_df2$year)), clip = "off")+
  # Setting X Axis Labels
  scale_x_continuous(breaks = c(2008,2013,2018,2020,2022),
                     labels = c("GE12\n(2008)","GE13\n(2013)","GE14\n(2018)","Sheraton*\n(2020)","GE15\n(2022)"))+
  # Adding Informative Text into Plot
  annotate("text", x = 2016, y = 0.85, label = "Break Away\nfrom PAS",size = 3) +
  geom_segment(aes(x = 2017,y = 0.8, xend = 2016.5,yend = 0.8), arrow = arrow(length=unit(.2, 'cm')))+
  annotate("text", x = 2020, y = 0.85, label = "Break Away\nfrom UMNO/BN",size = 3) +
  geom_segment(aes(x = 2019,y = 0.8, xend = 2019.5,yend = 0.8), arrow = arrow(length=unit(.2, 'cm')))+
  # Other Labels
  labs(x = "\nYear (Election/Event)",
       y = "Status\n\n",
       title = "The Shift in Malaysian Political's Allegiance",
       subtitle = "\nThere are no permanent enemies, only common interests. These past 10 years have seen many drama between political parties in <span style = 'color: red;'>Peninsular Malaysia</span><br>that resulted them to shift alliance. Will this instability continue into the next General Election?<br>",
       caption = caption)


# Save
ggsave(
  plot = z,
  filename = paste0("mys_politics_shift.jpg"),
  width = 12, height = 7,
  device = 'jpeg',
  units = "in",
  scale = 1
)



