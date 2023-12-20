# Vis
# https://www.kaggle.com/datasets/cashncarry/fifaworldranking
library(tidyverse)
library(ggtext)
library(ggimage)
library(ggrepel)
library(showtext)
#Data
itl_rank <- read_csv("data/fifa_ranking-2023-07-20.csv")
mys_rank = itl_rank %>% filter(country_full == "Malaysia") %>% filter(rank_date > "2020-01-01")
mys_rank_add = data.frame(rank = c(134,137),rank_date = c("2023-09-21","2023-10-27")) %>% mutate(rank_date = as.Date(rank_date))
mys_rank = bind_rows(mys_rank,mys_rank_add)

mys_annual_rank = read_csv("data/rank_annual_mys.csv")

# To control which year to project the rank on plot
df_rank_col = data.frame(Year = c('1993','1995','1997','2002','2009','2010','2015','2017','2019'),
                         rank_cols = c("white","#ef271b","#70e000","#ef271b","#ef271b","#70e000","#ef271b","#ef271b","#70e000"))

mys_annual_rank = mys_annual_rank %>% separate(Date,into = c('Mon','Year'),remove = F) %>% 
  left_join(df_rank_col,by = 'Year') %>% 
  mutate(rank_disp = ifelse(is.na(rank_cols),NA_integer_,Rank),
         rank_date = as.Date(paste0(Date," 01"),format = "%b %Y %d")) %>% 
  select(-c(Mon,Year))
names(mys_annual_rank) = c('date','rank','total_points','country_full','rank_cols','rank_disp','rank_date')

mys_rank = bind_rows(mys_rank,mys_annual_rank) %>% 
  mutate(rank_disp = ifelse(rank_date %in% c("2023-10-27"),rank,rank_disp),
         rank_cols = ifelse(rank_date %in% c("2023-10-27"),"green",rank_cols)
  )

# Additional Plot Requirements
font_add_google("Poppins", family = "Poppins")
showtext_auto()
showtext_opts(dpi = 300)
df_img_teks = data.frame(rank_date = c("2005-01-01","2008-01-01","2010-01-01","2013-01-01","2016-06-01",'2020-01-01',"2018-01-01","2022-01-01"),
                         rank = c(165,175,130,175,200,190,140,125),
                         img = c("img/gg_nrb.png","img/gg_bst.png","img/gg_krg.png","img/gg_dsh.png","img/gg_oks.png","img/gg_nvg.png","img/gg_tch.png","img/gg_kpg.png"))

df_img_teks$rank_date = as.Date(df_img_teks$rank_date)
# 
df_arrows = data.frame(x = c("2017-12-01"),y = 175,xend = "2018-12-01", yend = 190)
df_arrows$x = as.Date(df_arrows$x)
df_arrows$xend = as.Date(df_arrows$xend)
# df_fam_img = data.frame(rank_date = "2000-01-01",rank = 180,img = "img/fam.png")
# df_fam_img$rank_date = as.Date(df_fam_img$rank_date)
# Plot

a = ggplot(mys_rank,aes(x = rank_date,y = rank))+
  geom_line(linewidth = 1)+
  # scale_y_reverse(limits = c(215,65))+
  scale_y_reverse(limits = c(240,65))+
  scale_x_date(date_labels = "%Y",date_breaks = "4 year")+
  ggrepel::geom_label_repel(aes(label = rank_disp,fill = rank_cols))+
  scale_fill_identity()+
  theme_classic()+
  theme(
    # axis.ticks = element_blank(),
    plot.title = element_text(face = 'bold',family = "Poppins",size = 35),
    plot.subtitle = element_markdown(hjust = 0.5),
    axis.text = element_text(face = 'bold',size = 15),
    axis.title = element_text(face = 'bold',size = 15),
    panel.background = element_rect(fill = "#FFD100",colour = "#FFD100"),
    plot.background = element_rect(fill = "#FFD100"),
    axis.line = element_line(color = 'black'),
    plot.margin = unit(c(1,1,1,1), "cm")
    
  )+
  labs(title = "The Ups and Downs of Malaysian Football"
       # subtitle = "<img src='img/FAM.png' width='75'/>"
       )+
  ylab('Rank')+xlab("")+
  geom_image(data = df_img_teks,aes(x = rank_date,y = rank,image = img),position = position_jitter(width=0, height=0.1),
             size = 0.1,
             asp=9.5/6)+
  geom_curve(
    aes(x = x, y = y, xend = xend, yend = yend),
    data = df_arrows,
    arrow = arrow(length = unit(0.03, "npc"))
  )
  # geom_image(data = df_fam_img,aes(x = rank_date,y = rank,image = img),position = position_jitter(width=0, height=0.1),
  #            size = 0.1,
  #            asp=9.5/6)

b = a + 
  # Teks 1994
  annotate(geom = 'text',x =as.Date("1996-01-01"),y= 68,fontface=2, label = "(1994) Tragedy;\nCorruption Scandal prompts\nMalaysian Football's downfall",color = 'black') 
  # # Teks NRB
  # annotate(geom = 'text',x =as.Date("2005-01-01"),y= 185,fontface=2, label = "(2007) Struggle;\nMalaysia lost\nall matches as\nCo-Host of Asian Cup",color = 'black') +
  # # Teks BST
  # annotate(geom = 'text',x =as.Date("2008-01-01"),y= 195,fontface=2, label = "(2008) Struggle;\nMalaysia eliminated in\nthe Group Stage of\nthe AFF Championship",color = 'black') +
  # # Teks KRG
  # annotate(geom = 'text',x =as.Date("2010-01-01"),y= 115,fontface=2, label = "(2010) Triumph;\nMalaysia won the AFF Suzuki Cup",color = 'black') +
  # # Teks DSH 
  # annotate(geom = 'text',x =as.Date("2013-01-01"),y= 195,fontface=2, label = "(2015) Stagnation;\nMalaysia recorded\ntheir worst lost of\n10-0 to UAE",color = 'black') +
  # # Teks OKS
  # annotate(geom = 'text',x =as.Date("2016-06-01"),y= 220,fontface=2, label = "(2016) Frustration;\nMalaysia knocked\nout in the grouping\nof AFF Championship",color = 'black') +
  # #
  # annotate(geom = 'text',x =as.Date("2020-01-01"),y= 210,fontface=2, label = "(2017) Rock Bottom;\nWinless Streak sees\nMalaysia drop to\ntheir worst ranking",color = 'black') +
  # annotate(geom = 'text',x =as.Date("2018-01-01"),y= 120,fontface=2, label = "(2018) Revival;\nMalaysia steadily climbs\nthe ladder",color = 'black') +
  # annotate(geom = 'text',x =as.Date("2022-01-01"),y= 106,fontface=2, label = "(2022) Impact;\nMalaysia Qualified to\nAsia Cup 2024",color = 'black')

ggsave(plot = b,filename = "mys_rank2a.jpg",
       width = 22,height = 12,units = 'in',scale = 1)

# png("mys_rank.png",width = 20,height = 12,res = 300,units = "in")
# print(b)
# dev.off()

