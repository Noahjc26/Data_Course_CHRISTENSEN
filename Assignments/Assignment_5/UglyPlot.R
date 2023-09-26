library(tidyverse)
library(cowplot)
library(ggimage)

plotimage <- image_read2("./ScreenShot2.png")
p <- ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,color=Species)) +
  geom_col(aes(y=Sepal.Width,fill=Species),width=.1,position = "jitter") +
  geom_abline(slope = -.5,intercept = 3,linewidth=1,alpha=.3) +
  geom_area(alpha=.3) +
  geom_point(show.legend = FALSE) +
  labs(x="Sepal.Length",
       title = "S        e          p        a        l      .      l    e        n        g        t        h  ", 
       y="Sepal.Length",
       color="SEPAL",
       fill="sepal.length")+
  scale_fill_manual(
    labels=c('Sepal Length', "Sepal.Length","sepal.Length"),
    values=c("blue", "cyan4","green")) +
  scale_color_manual(
    labels=c('Sepal.Length', 'Sepal.Length','Speal.Length'),
    values = c("purple", "purple2", "purple3")) +
  annotate("segment", x = -5, xend = 4, y = 14, yend = 5, colour = "red", size=2, arrow=arrow()) +
  annotate("segment", x = 4, xend = 5, y = 12, yend = 8, colour = "red", size=2, arrow=arrow()) +
  annotate("segment", x = 10, xend = 8, y = 10, yend = 6, colour = "red", size=2, arrow=arrow()) +
  annotate("segment", x = 5, xend = 6, y = 15, yend = 7, colour = "red", size=2, arrow=arrow()) +
  geom_point( size=50,shape=1, color="red2") +
  draw_image(plotimage,scale = 6,vjust = -3,hjust = 2) +
   annotate("text", x = 2, y = 10, 
            label = "Doctors HATE when you 
            know this one graphing trick 😱
            
            What plots next will SHOCK you!!!
            " , color="red", 
          size=10 , angle=2, fontface="bold")

p + theme(
  aspect.ratio=1.1,
  axis.title = element_text(size = 11),
  axis.title.x = element_text(hjust = 1,family = "serif",angle = 0,size=20,color = "gold1",vjust=1),
  strip.text = element_text(("Sepil.Length")),
  plot.tag = element_text(),
  panel.background = element_rect(fill = FALSE,color = 'yellow',linetype = 0),
  panel.ontop = FALSE,
  panel.grid = element_blank(),
  plot.background = element_rect(fill='gold4'),
  legend.background = element_rect(fill = "brown4",color="red3",linewidth = 5),
  legend.text = element_text(family = "Helvetica",vjust = 1,size=50,color="red4"),
  axis.text.x = element_text(vjust = 10,size = 1),
  axis.text.y = element_text(hjust=5,size=25,color='white'),
  axis.title.y = element_text(size=6,vjust=-200))




