# Vis
# https://www.kaggle.com/datasets/cashncarry/fifaworldranking
library(tidyverse)
library(ggtext)
library(ggimage)
library(ggrepel)
library(showtext)
#Data
itl_rank <- read_csv("data/fifa_ranking-2023-07-20.csv")
# Use monthly rank for 2018 onwards
mys_rank = itl_rank %>% filter(country_full == "Malaysia") %>% filter(rank_date > "2018-01-01")
mys_rank_add = data.frame(rank = c(134,137),rank_date = c("2023-09-21","2023-10-27")) %>% mutate(rank_date = as.Date(rank_date))
mys_rank = bind_rows(mys_rank,mys_rank_add)
# Use annual rank prior to 2018
mys_annual_rank = read_csv("data/rank_annual_mys.csv") %>% filter(!Date %in% c("Dec 2019","Dec 2018"))

# To control which year to project the rank on plot [geom_label_repel]
df_rank_col = data.frame(Year = c('1993','1995','1997','2002','2007','2009','2010','2015','2016'),
                                  # ,'2017','2019'),
                         rank_cols = c("white","#ef271b","#70e000","#ef271b","#ef271b","#ef271b","#70e000","#ef271b","#70e000"))
                                       # ,"#ef271b","#70e000"))

mys_annual_rank = mys_annual_rank %>% separate(Date,into = c('Mon','Year'),remove = F) %>% 
  left_join(df_rank_col,by = 'Year') %>% 
  mutate(rank_disp = ifelse(is.na(rank_cols),NA_integer_,Rank),
         rank_date = as.Date(paste0(Date," 01"),format = "%b %Y %d")) %>% 
  select(-c(Mon,Year))
names(mys_annual_rank) = c('date','rank','total_points','country_full','rank_cols','rank_disp','rank_date')

mys_rank = bind_rows(mys_rank,mys_annual_rank) %>% 
  mutate(rank_disp = ifelse(rank_date %in% c("2018-03-15","2019-11-28","2023-10-27"),rank,rank_disp),
         rank_cols = case_when(
           rank_date %in% c("2019-11-28","2023-10-27") ~ "#70e000",
           rank_date == "2018-03-15" ~ "#ef271b",
           TRUE ~ rank_cols
         )
  )

# Additional Plot Requirements
## Fonts
font_add_google("Poppins", family = "Poppins")
showtext_auto()
showtext_opts(dpi = 300)

## Text and Image
df_img_teks = data.frame(rank_date = c("2000-01-01","2004-01-01","2005-01-01","2008-01-01","2010-01-01","2013-01-01","2014-01-01",'2020-01-01',"2018-01-01","2022-01-01"),
                         rank = c(200,95,200,200,95,200,95,200,95,95),
                         img = c("img/gg_ahr.png","img/gg_btb.png","img/gg_nrb.png","img/gg_bst.png","img/gg_krg.png","img/gg_dsh.png","img/gg_oks.png","img/gg_nvg.png","img/gg_tch.png","img/gg_kpg.png"),
                         teks = c("2001-04","2004-05","2005-07","2007-2008","2004(C) /2009-13","2014-15","2014(C) /2015-2017","2017","2017-22","2022-"))

df_img_teks = df_img_teks %>% mutate(rank_date = as.Date(rank_date),
                                     teks_pos = rank+15)
# Key Events
df_key_events = data.frame(event = c("(2002) Underwhelming;\n4th in the\nAFF Championship\nunder Harris",
                                     "(2004) Attempt;\n3rd in the\n AFF Championship \nunder Bisckei",
                                     "(2007) Struggle;\nMalaysia lost\nall matches as\nCo-Host of Asian Cup",
                                     "(2008) Disaster;\nMalaysia eliminated in\nthe Group Stage of\nthe AFF Championship\nfor the first time",
                                     "(2010) Triumph;\nMalaysia won the AFF Suzuki Cup",
                                     "(2015) Humiliation;\nMalaysia recorded\ntheir worst lost of\n10-0 to UAE",
                                     "(2016) Frustration;\nMalaysia knocked\nout in the grouping\nof AFF Championship",
                                     "(2018) Rock Bottom;\nWinless Streak sees\nMalaysia drop to\ntheir worst ranking",
                                     "(2019) Revival;\nMalaysia steadily\nclimbs the ladder",
                                     "(2022) Impact;\nMalaysia Qualified to\nAsia Cup 2024\nfor the First Time"
                                     
                                     ),
                           rank_date = c("2002-05-01","2005-01-01","2006-01-01","2009-06-01","2010-12-01","2013-01-01","2016-01-01","2020-01-01","2018-01-01","2022-06-01"),
                           teks_pos = c(145,145,175,175,125,175,185,178,125,125))
df_key_events$rank_date = as.Date(df_key_events$rank_date)

## Arrows to point out text to line 
df_arrows = mys_rank %>% filter(rank_date %in% c("2007-12-01","2008-12-01","2010-12-01","2015-12-01","2018-03-15","2019-11-28")) %>% 
  select("x" = rank_date,"y" = rank) %>% arrange(x)
# Coordinates of where the arrow should end
df_arrows$xend = c("2006-01-01","2008-06-01","2010-12-01","2013-01-01","2018-12-01","2019-05-01")
df_arrows$yend = c(165,170,130,165,180,125)
df_arrows$x = as.Date(df_arrows$x)
df_arrows$xend = as.Date(df_arrows$xend)

# Plot

a = ggplot(mys_rank,aes(x = rank_date,y = rank))+
  geom_line(linewidth = 1)+
  scale_y_reverse(limits = c(230,65))+
  scale_x_date(date_labels = "%Y",date_breaks = "4 year")+
  # ggrepel::geom_label_repel(aes(label = rank_disp,fill = rank_cols))+
  geom_curve(
    aes(x = x, y = y, xend = xend, yend = yend),
    data = df_arrows,
    arrow = arrow(length = unit(0.03, "npc"))
  )+
  geom_label(aes(label = rank_disp,fill = rank_cols))+
  scale_fill_identity()+
  geom_text(data = df_img_teks,aes(x = rank_date,y = teks_pos,label = teks))+
  geom_text(data = df_key_events,aes(x = rank_date,y = teks_pos,label = event))+
  theme_classic()+
  theme(
    plot.title = element_markdown(face = 'bold',family = "Poppins",size = 35),
    plot.subtitle = element_text(size = 20,family = "Poppins"),
    axis.text = element_text(face = 'bold',size = 20),
    axis.title = element_text(face = 'bold',size = 15),
    panel.background = element_rect(fill = "#FFD100",colour = "#FFD100"),
    plot.background = element_rect(fill = "#FFD100"),
    axis.line = element_line(color = 'black'),
    plot.margin = unit(c(1,1,1,1), "cm")
    
  )+
  labs(title = paste0("How far can Malaysia Football Rise?"),
       subtitle = "Only Kosovo, Canada, Eq Guinea and Gambia have made better Rank recovery since Malaysia's lowest Rank in early 2018 on the same timeframe
       
       "
       # subtitle = "<img src='img/FAM.png' width='75'/>"
  )+
  ylab('Rank')+xlab("")+
  geom_image(data = df_img_teks,aes(x = rank_date,y = rank,image = img),position = position_jitter(width=0, height=0.1),
             size = 0.1,
             asp=9.5/6)
# geom_image(data = df_fam_img,aes(x = rank_date,y = rank,image = img),position = position_jitter(width=0, height=0.1),
#            size = 0.1,
#            asp=9.5/6)

b = a + 
  # Teks 1994
  annotate(geom = 'text',x =as.Date("1996-01-01"),y= 68,fontface=2, label = "(1994) Tragedy;\nCorruption Scandal prompts\nMalaysian Football's downfall",color = 'black') 


ggsave(plot = b,filename = "mys_rank_manager_2.jpg",
       width = 22,height = 13,units = 'in',scale = 1)

# png("mys_rank.png",width = 20,height = 12,res = 300,units = "in")
# print(b)
# dev.off()

