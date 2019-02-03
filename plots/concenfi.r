source('lib/concentration-data.r')

# Cut dataframes down to what we need for this plot
subset <- function(df) {
    df <- df %>%
          filter(nchar(NAICS.id) == 4) %>%    # Only look at industry groups
          filter(!is.na(VAL_PCT))

    df %>%
    select(NAICS.id, NAICS.label, CONCENFI.id, CONCENFI.label,
           VAL_PCT, YEAR, SECTOR.label) %>%
    filter(CONCENFI.id != '001') %>%          # Ignore sector summary row
    spread(YEAR, VAL_PCT, sep='.')
}

df.2007 <- read_all_concentration_data('concentration-data/2007', '*Z6.csv')
df.2012 <- read_all_concentration_data('concentration-data/2012', '*Z6.csv')
df <- inner_join(subset(df.2007),
                 subset(df.2012),
                 by=c('NAICS.id', 'NAICS.label', 'SECTOR.label',
                      'CONCENFI.id', 'CONCENFI.label'),
                 suffix=c('.2007', '.2012'))

concenfi.labels <- c('Top 4 Firms', 'Top 8 Firms',
                     'Top 20 Firms', 'Top 50 Firms')

# We are interested in how much the concentration changed
df <- mutate(df, change = YEAR.2012 - YEAR.2007,
                 CONCENFI.id = factor(CONCENFI.id, labels = concenfi.labels))

# ---- plot.concenfi ----
ggplot(df, aes(x = CONCENFI.id, fill = CONCENFI.id, y = change)) +
    stat_function(fun = function(x) 0, color = 'gray40', linetype='dashed') +
    geom_violin(draw_quantiles = c(0.5), alpha = 0.7, size = 0.2) +
    scale_y_continuous(breaks = seq(-10, 10, 2), limits = c(-10, 10)) +
    scale_fill_manual(values = c(teal, red, green, purple)) +
    theme(legend.position = 'none') +
    labs(title = 'Concentration Has Grown Across All Measures',
         subtitle =
             'Change in Concentration Among All NAICS Industries (2007-2012)',
         x = 'Firm Size Range',
         y = 'Percentage Point Change in Revenue Captured (2007-2012)',
         caption = 'Source: US Census Bureau Economic Census')
