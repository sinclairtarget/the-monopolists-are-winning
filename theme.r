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
dark_gray <- 'gray20'
annotation_color <- 'gray40'
highlight_color <- '#e2a106'

my_theme <- function(for_map = FALSE) {
   t <- theme_minimal() +
        theme(text = element_text(family = 'Ubuntu Light'),
              axis.title = element_text(family = 'FreeSans', size = 11),
              axis.text = element_text(family = 'FreeSans', size = 10),
              plot.caption = element_text(size = 9, color = annotation_color),
              panel.grid = element_line(color = annotation_color, size = 0.1),
              legend.position = 'bottom',
              legend.box.spacing = unit(0, 'pt'),
              legend.title.align = 0,
              legend.justification = 'left',
              legend.text = element_text(family = 'FreeSans', size = 11))

    if (for_map) {
        # Map-specific theme tweaks
        t <- t + theme(axis.title = element_blank(),
                       aspect.ratio = 0.77)
    }

    t
}

thematic_label <- function(text, x, y, hjust = 0, vjust = 0.5, size = 3.25) {
    annotate(geom = 'label',
             label = text,
             x = x,
             y = y,
             hjust = hjust,
             vjust = vjust,
             size = size,
             color = annotation_color,
             label.size = NA,
             label.r = unit(0, 'lines'),
             label.padding = unit(0.3, 'lines'),
             fill = light_gray)

}

thematic_segment <- function(x, xend, y, yend) {
    annotate(geom = 'segment', x = x, xend = xend, y = y, yend = yend,
             color = dark_gray, size = 0.4, linetype = 'dotted')
}
