# 1.0 Library
library(readxl)
library(tidyverse)
library(geomtextpath)
library(ggtext)

# 2.0 Data
# Read all sheets into a list
sheets <- excel_sheets("superschools.xlsx")  # Get sheet names
data_list <- lapply(sheets, function(sheet) read_excel("superschools.xlsx", sheet = sheet))
names(data_list) <- sheets

# 3.0 Process Data
selected_list <- data_list[1:9]
combined_df <- bind_rows(selected_list, .id = "Year")
vi_2017 = tibble("Year" = '2017',"Team" = "VI",`Cup Result` = "Champion")
didnotparticipate_df = tibble("Year" = "2024","Team" = "STJ",`Cup Result` = "Did Not Participate")
combined_df = bind_rows(combined_df,vi_2017,didnotparticipate_df) %>% arrange(Year,`League Pos`)
# Vis Settings
team_levels = c("STAR","MCKK","SEMASHUR","SMSS","SDAR","SAS", #pioneers
                "VI","EC", #joined 2018-19
                "RMC","KYS","SAHC","STJ", # joined 2020
                "SYED PUTRA","SESMA","KE7","HSBM", #joined 2023
                "ACS","MRSMBP","SYP","INTEGOMB","SAINA" # joined 2024
                )
combined_df <- combined_df %>% mutate(Team = factor(Team,levels = team_levels),
                                      Year = gsub("-Group","",Year),
                                      `Cup Result` = ifelse(Year == "2020","Group Stage",`Cup Result`),
                                      Year = (Year = ifelse(Year == "2020","2020*",Year))
                                      )

league_winner_df = combined_df %>% filter(Year %in% c("2013","2014","2016","2017"),`League Pos` == 1)
cancelled_df = combined_df %>% filter(Year=="2020*")

# 4.0 Vis
plot = ggplot(combined_df,aes(x = Year,y = fct_rev(Team),fill = `Cup Result`)) +
  geom_tile(color = 'white',width=.9, height=.9)+
  # Geom Point for 2017
  geom_point(inherit.aes=FALSE, 
             data=vi_2017, 
             mapping=aes(x=Year, y=fct_rev(Team)), shape=8, color="grey20", size=1)+
  scale_fill_manual(values = c(
    "Champion" = '#FFBC1F',
    "Runner Up" = '#ef476f',
    "Semifinalist" = '#05C793',
    "Knockout Stage" = '#26547c',
    "Group Stage" = "grey",
    "Did Not Participate" = "grey0"
  ),
  limits = c("Champion","Knockout Stage","Runner Up","Group Stage","Semifinalist","Did Not Participate")
  )+
  labs(
    title = "",
    x = "",    
    y = "", 
    fill = "Achievement",
    caption = ""
  ) +
  scale_y_discrete(position = "right")+
  theme_bw()+
  coord_fixed(clip = "off") +
  theme(
        plot.title=element_text(face="bold"),
        legend.position = "bottom",
        legend.text=element_text(size=8),
        legend.justification = "left",
        legend.box.margin = margin(5, 0, 0, 0),  # Push legend further down
        axis.text.y= element_text(size=8,hjust = 0,face = 'bold'),
        axis.text.x = element_text(size=10, angle=90,face = 'bold'),
        axis.ticks=element_blank(),
        panel.background = element_blank()
        )+
  guides(fill = guide_legend(title.position = "top",title.hjust = 0.5))

# Captions and Writeup
main_title = "The Malaysia<br>Super   Schools<br>Rugby Fifteens<br>"

para_1 = "Originally known as the <strong>Malaysia Super Six Schools Rugby Fifteen</strong>, the tournament started with six pioneer teams. Following VI's surprise victory in 2017's Cup Challenge, the tournament <strong>expanded in 2018</strong> onwards and was rebranded."

para_2 = "From 2013 to 2017, it followed a league format, where standings determined which teams competed for the <strong>Cup, Plate, and Spoon titles.</strong> With more teams joining the tournament, it transitioned into a Cup-style format with group stages (pools) leading to knockout rounds."
para_3 = "<strong>The 2025 season marks the milestone 10th edition of the tournament</strong>, where a past champion has the opportunity to secure a third title, or a new team will rise to claim their first championship."
para_4 = "From its humble beginnings to becoming a true test of school rugby excellence, the Malaysia Super Schools Rugby Fifteens continues to grow, producing future national talents and fostering a deep passion for the sport."

info_1 = "
📌 <strong>Most Consistent</strong> - STAR ; 3 Grand Finals, 4 Semi Finals and 1 Playoff<br>
📌 <strong>Most Championship (2)</strong> - MCKK,VI,SEMASHUR and STAR<br>
📌 <strong>Most Grand Final Appearance (4)</strong> - MCKK & SEMASHUR<br>
💡 <strong>2017</strong> - VI did not compete in the SSR 2017 League but was invited to Cup Playoffs <br>
⚠️ <strong>2020</strong> season was suspended due to Covid-19"

author = "
Source: Compiled from official media updates of Malaysia Super Schools Rugby Fifteens, 2013-2024.<br>
Github: abdullahzubairwan"

title<-paste0("<span style='font-size:35pt;'>**",main_title,"**</span><br><br>",
              "<span style='font-size:11pt;'>",para_1,"<span><br><br>",
              "<span style='font-size:11pt;'>",para_2,"<span><br><br>",
              "<span style='font-size:11pt;'>",para_3,"<span><br><br>",
              "<span style='font-size:11pt;'>",para_4,"<span><br><br><br>",
              "<span style='font-size:11pt;'>",info_1,"<span><br><br><br><br><br>",
              "<span style='font-size:9pt;color:#545454;'>",author,"<span>")


# Final Plot
ggplot()+
  #add in tile heatmap
  annotation_custom(ggplotGrob(plot), xmin=5, xmax=7, ymin=1.25, ymax=4.75)+
  #add plot title
  geom_textbox(mapping=aes(x=3.25, y=4.7, label=title), hjust=0, vjust=1, box.size=NA, fill=NA,  width = unit(4, "inch"))+
  scale_x_continuous(limits=c(3,7), expand=c(0,0))+
  scale_y_continuous(limits=c(1,5), expand=c(0,0))+
  coord_equal()+
  theme_void()


ggsave("ssr15.png", bg="white" , height=10, width=10, units="in")
