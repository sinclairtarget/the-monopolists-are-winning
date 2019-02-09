source('lib/concentration-data.r')
source('theme.r')

theme_set(my_theme())

# Shape each dataframe down to what we need for this plot
shape <- function(df) {
    df <- df %>%
          filter(nchar(NAICS.id) == 4) %>%    # Only look at industry groups
          filter(!is.na(VAL_PCT))

    df <- df %>%
          select(NAICS.id, NAICS.label, CONCENFI.id,
                 VAL_PCT, SECTOR.label) %>%
          filter(CONCENFI.id != '001')          # Ignore sector summary row

    #
    df <- df %>%
          rename(CONCENFI = CONCENFI.id) %>%
          spread(CONCENFI, VAL_PCT, sep = '.') %>%
          mutate(SHARE.804 = CONCENFI.804,
                 SHARE.808 = CONCENFI.808 - CONCENFI.804,
                 SHARE.820 = CONCENFI.820 - CONCENFI.808,
                 SHARE.850 = CONCENFI.850 - CONCENFI.820)
}

df.2007 <- read_all_concentration_data('concentration-data/2007', '*Z6.csv')
df.2012 <- read_all_concentration_data('concentration-data/2012', '*Z6.csv')

df.2007 <- shape(df.2007)
df.2012 <- shape(df.2012)

df <- inner_join(df.2007,
                 df.2012,
                 by=c('NAICS.id', 'NAICS.label', 'SECTOR.label'),
                 suffix=c('.2007', '.2012'))

concenfi.labels <- c('804'='Top 4 Firms', '808'='Next 4 Firms',
                     '820'='Next 12 Firms', '850'='Next 30 Firms')

# We are interested in how much the concentration changed
df <- df %>%
      mutate(CHANGE.804 = SHARE.804.2012 - SHARE.804.2007,
             CHANGE.808 = SHARE.808.2012 - SHARE.808.2007,
             CHANGE.820 = SHARE.820.2012 - SHARE.820.2007,
             CHANGE.850 = SHARE.850.2012 - SHARE.850.2007) %>%
      select(NAICS.id, NAICS.label, SECTOR.label,
             CHANGE.804, CHANGE.808, CHANGE.820, CHANGE.850) %>%
      gather('CONCENFI.id', 'CHANGE',
             CHANGE.804, CHANGE.808, CHANGE.820, CHANGE.850) %>%
      mutate(CONCENFI.id = factor(gsub('CHANGE.', '', CONCENFI.id),
                                  labels = concenfi.labels))

# ---- plot.concenfi ----
ggplot(df, aes(x = CONCENFI.id, y = CHANGE)) +
    stat_function(fun = function(x) 0, color = gray, linetype='dashed') +
    geom_violin(aes(fill = CONCENFI.id),
                color = 'transparent',
                alpha = 0.4) +
    geom_violin(aes(color = CONCENFI.id),
                fill = NA,
                draw_quantiles = c(0.5),
                size = 0.3) +
    scale_y_continuous(breaks = seq(-10, 10, 2), limits = c(-10, 10)) +
    scale_color_manual(values = c(teal, red, green, purple)) +
    scale_fill_manual(values = c(teal, red, green, purple)) +
    theme(legend.position = 'none') +
    labs(title = 'Concentration Growth Driven by Top Four, When They Succeed',
         subtitle =
             'Change in Concentration by Firm Class, All NAICS Industries (2007-2012)',
         x = 'Firm Class (by Total Revenue)',
         y = 'Percentage Point Change in Revenue Captured (2007-2012)',
         caption = 'Source: US Census Bureau Economic Census') +
    annotate(geom = 'label',
             label = 'Median',
             x = 3.5,
             y = 2.5,
             color = annotation_color,
             size = 3,
             label.size = NA,
             label.r = unit(0, 'lines'),
             fill = light_gray) +
    annotate(geom = 'segment', x = 3.66, xend = 3.82, y = 2.5, yend = 2.5,
             color = 'gray20', size = 0.4, linetype = 'dotted') +
    annotate(geom = 'segment', x = 3.82, xend = 3.82, y = 2.5, yend = 0.46,
             color = 'gray20', size = 0.4, linetype = 'dotted')

