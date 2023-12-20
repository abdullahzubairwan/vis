library(geomtextpath)
library(ggimage)
library(cropcircles)
library(magick)
library(glue)

plot_image_label<-function(image,
                           label,
                           font_color="black", 
                           position="top",
                           hjust=0.2){
  
  #crop the image into a circle shape
  cropped_image = cropcircles::circle_crop(image)
  
  t = seq(0, 1, length.out = 100) * pi
  
  #set up params based on top or bottom
  if(position=="top"){
    data = data.frame(x = cos(t),y = sin(t))
    vjust=1.1
    ymax=1.2
    ymin=-0.9}
  
  else if(position=="bottom"){
    data=data.frame(x = cos(t),y = sin(t)*-1)
    vjust=-0.1
    ymax=0.9
    ymin=-1.2}
  
  #plot
  ggplot() +
    geom_image(aes(x=0, y=0, image = cropped_image), asp=2.4/2.1, size=.7) +
    scale_x_continuous(limits = c(-1.2, 1.2))+
    scale_y_continuous(limits=c(ymin, ymax))+
    geom_textpath(data = data, aes(x,y,label =label), linecolor=NA, color=font_color,
                  size = 14.5,  fontface="bold", vjust = vjust, hjust=hjust)+
    coord_equal()+
    theme_void()
}

img_nrb = "img/nrb.jpg"
gg1 = plot_image_label(image=img_nrb, label="Norizan Bakar",position = 'bottom')

img_rjgp = "img/krg.jpg"
gg2 = plot_image_label(image=img_rjgp, label="K Rajagopal")

img_dsh = "img/dsh.jpg"
gg3 = plot_image_label(image=img_dsh, label="Dollah Salleh",position = "bottom")

img_nvg = "img/nvg.jpg"
gg4 = plot_image_label(image=img_nvg, label="Nelo Vingada",position = "bottom")

img_tch = "img/tch.jpg"
gg5 = plot_image_label(image=img_tch, label="Tan Cheng Hoe")

img_kpg = "img/kpg.jpg"
gg6 = plot_image_label(image=img_kpg, label="Kim Pan Gon")

img_bst = "img/bst.jpg"
gg7 = plot_image_label(image=img_bst, label="B Sathianathan",position = 'bottom')

img_oks = "img/oks.jpg"
gg8 = plot_image_label(image=img_oks, label="Ong Kim Swee")

img_ahr = "img/ahr.jpg"
gg9 = plot_image_label(image=img_ahr, label="Allan Harris",position = 'bottom')

img_btb = "img/btb.jpg"
gg10 = plot_image_label(image=img_btb, label="Bertalan Bicskei")

ggsave(plot = gg1,filename = "img/gg_nrb.png")
ggsave(plot = gg2,filename = "img/gg_krg.png")
ggsave(plot = gg3,filename = "img/gg_dsh.png")
ggsave(plot = gg4,filename = "img/gg_nvg.png")
ggsave(plot = gg5,filename = "img/gg_tch.png")
ggsave(plot = gg6,filename = "img/gg_kpg.png")
ggsave(plot = gg7,filename = "img/gg_bst.png")
ggsave(plot = gg8,filename = "img/gg_oks.png")
ggsave(plot = gg9,filename = "img/gg_ahr.png")
ggsave(plot = gg10,filename = "img/gg_btb.png")
