library('scales')
library('stringr')

source('lib/concentration-data.r')

# Cut dataframes down to what we need for this plot
subset <- function(df) {
    df <- df %>%
          filter(nchar(NAICS.id) == 4) %>%    # Only look at industry groups
          filter(!is.na(VAL_PCT))

    df <- by_top4(df)
    df %>%
    select(NAICS.id, NAICS.label, VAL_PCT, YEAR, RCPTOT.tot, SECTOR.label) %>%
    # RCPTOT is in thousands, let's do billions
    mutate(RCPTOT.tot = RCPTOT.tot / 1000000) %>%
    spread(YEAR, VAL_PCT, sep='.')
}

df.2007 <- read_all_concentration_data('concentration-data/2007', '*Z6.csv')
df.2012 <- read_all_concentration_data('concentration-data/2012', '*Z6.csv')
df <- inner_join(subset(df.2007),
                 subset(df.2012),
                 by=c('NAICS.id', 'NAICS.label', 'SECTOR.label'),
                 suffix=c('.2007', '.2012'))

add_percent_sym <- function(lab) {
    paste0(lab, '%')
}

color_guide <- guide_legend(title = 'NAICS Sector',
                            ncol = 3,
                            title.position = 'top')

# ---- plot.concentration ----
ggplot(df, aes(x = YEAR.2007, y = YEAR.2012)) +
    stat_function(fun = function(x) x, color='gray40', linetype='dashed') +
    geom_point(aes(size = RCPTOT.tot.2012,
                   color = str_wrap(SECTOR.label, 40),
                   fill = str_wrap(SECTOR.label, 40)),
               shape = 21, alpha = 0.5) +
    scale_x_continuous(labels = add_percent_sym, limits = c(0, 100)) +
    scale_y_continuous(labels = add_percent_sym, limits = c(0, 100)) +
    scale_size_continuous(labels = comma) +
    labs(title = 'Many Industries Have Become More Concentrated',
         subtitle = 'Change in Concentration Among NAICS Industries',
         x = 'Industry Revenue Captured by Top Four Firms (2007)',
         y = 'Industry Revenue Captured by Top Four Firms (2012)',
         caption = 'Source: US Census Bureau Economic Census') +
    guides(fill = color_guide,
           color = color_guide,
           size = guide_legend(title = 'Industry Revenue ($bn)',
                               title.position = 'top')) +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 7, lineheight = 1.1),
          legend.box = 'vertical',
          legend.key.height = unit(20, 'pt')) +
    annotate('text', x = 10,
                     y = 95,
                     label = 'More Concentrated',
                     size = 3,
                     color = 'gray40') +
    annotate('text', x = 90,
                     y = 6,
                     label = 'Less Concentrated',
                     size = 3,
                     color = 'gray40')
