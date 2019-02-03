library('tidyverse')
library('extrafont')

loadfonts()

teal <- '#219096'
green <- '#52a34f'
blue <- '#3d69af'
red <- '#d34d43'
purple <- '#a55197'
gray <- '#919599'
light_gray <- '#eff1f2'
annotation_color <- 'gray40'
highlight_color <- '#e2a106'

my_theme <- function() {
   theme_minimal() +
   theme(text = element_text(family = 'Ubuntu Light'),
         axis.title = element_text(family = 'FreeSans', size = 9),
         axis.text = element_text(family = 'FreeSans'),
         plot.caption = element_text(size = 8, color = annotation_color),
         panel.grid = element_line(color = annotation_color, size = 0.1),
         legend.position = 'bottom',
         legend.box.spacing = unit(0, 'pt'),
         legend.title.align = 0,
         legend.justification = 'left')
}
