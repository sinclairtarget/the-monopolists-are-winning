library('scales')
library('stringr')

source('lib/concentration-data.r')
source('theme.r')

theme_set(my_theme())

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

df.highlight <- df %>%
                filter(NAICS.id == '5242')

highlight_label <- paste('The "Agencies, Brokerages, and other Insurance"',
                         '\nindustry became 34 percentage points more',
                         '\nconcentrated between 2007 and 2012.')

# ---- plot.concentration ----
ggplot(df, aes(x = YEAR.2007, y = YEAR.2012)) +
    stat_function(fun = function(x) x, color='gray40', linetype='dashed') +
    geom_point(aes(size = RCPTOT.tot.2012), shape = 21,
                                            alpha = 0.5,
                                            color = teal,
                                            fill = teal) +
    geom_point(data = df.highlight,
               size = 7,
               shape = 1,
               color = annotation_color) +
    scale_x_continuous(labels = add_percent_sym, limits = c(0, 100)) +
    scale_y_continuous(labels = add_percent_sym, limits = c(0, 100)) +
    scale_size_continuous(labels = comma) +
    labs(title = 'Many Industries Have Become More Concentrated',
         subtitle = 'Change in Concentration Among NAICS Industries (2007-2012)',
         x = 'Industry Revenue Captured by Top Four Firms (2007)',
         y = 'Industry Revenue Captured by Top Four Firms (2012)',
         caption = 'Source: US Census Bureau Economic Census') +
    guides(size = guide_legend(title = 'Industry Revenue ($bn)',
                               title.position = 'top')) +
    theme(legend.box = 'vertical',
          legend.key.height = unit(20, 'pt')) +
    thematic_label(highlight_label, x = 2, y = 64) +
    thematic_segment(x = 8.4, xend = 8.4, y = 44.5, yend = 59.5) +
    thematic_label('Became More Concentrated', x = 12.5, y = 94, hjust = 0.5) +
    thematic_label('Became Less Concentrated', x = 87.5, y = 6, hjust = 0.5)
