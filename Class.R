library(tidyverse)
library(palmerpenguins)
library(ggimage)

#### command-shift-m (THE PIPE)

#same thing
glimpse(penguins)

penguins %>% glimpse()
  

1:10 %>% max()
max(c(1,2,3,4,5))

c(1,2,3,4,5) %>% max()

letters %>% grep("e")

grep()

penguins %>% dim()
penguins %>% names







p <-  penguins %>%
  ggplot(aes(x=species, y=bill_length_mm, fill=species)) +
  geom_violin() +
  geom_point(alpha=0.25,shape=4) +
  facet_wrap(~sex) +
  geom_image(aes(image=""))

p
p + theme(axis.title.x = element_text(face = 'bold',
                                    color='hotpink',
                                    size=16,
                                    hjust = 0,
                                    vjust = -50,
                                    angle = 180,),
        legend.background = element_rect(fill='green'),
        legend.title = element_text(color='pink',
                                    angle=90,
                                    size = 120,
                                    face='italic'
                                    ),
        strip.text = element_text(color='yellow'),
        panel.grid = element_line(size=5,
                                  color='black'),
        panel.background = element_rect(fill = 'yellow'),
        plot.background = element_rect(fill='black')
        ) %>% theme_set()
    
    
    