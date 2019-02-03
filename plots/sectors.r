source('lib/concentration-data.r')

df <- read_all_concentration_data('concentration-data/2012', '*Z6.csv')

# Only look at sectors
df <- df %>%
      filter(nchar(NAICS.id) == 2) %>%
      filter(!is.na(VAL_PCT))

# Look at top four firms
df <- by_top4(df)

# Add highlight column
df <- mutate(df, is_information = ifelse(NAICS.id == '51', 'yes', 'no'))

# ---- plot.sectors ----
ggplot(df, aes(x = reorder(NAICS.label, VAL_PCT), y = VAL_PCT)) +
    geom_bar(aes(fill = is_information), stat = 'identity', alpha = 0.8) +
    geom_label(aes(label = NAICS.label),
               hjust = 'left',
               size = 3,
               nudge_y = 2,
               label.size = 0,
               fill = light_gray,
               label.r = unit(0, 'lines')) +
    geom_text(aes(label = ifelse(is_information == 'yes', VAL_PCT, '')),
              hjust = 'right',
              size = 3,
              nudge_y = -2) +
    scale_y_continuous(limits = c(0, 50), breaks=seq(0, 50, 10)) +
    scale_fill_manual(values = c('no' = gray, 'yes' = teal)) +
    coord_flip() +
    labs(title = 'Technology and Media Lead in Concentration',
         subtitle = 'Market Concentration by NAICS Sector',
         x = 'NAICS Sector',
         y = '% of Total Revenue Captured by Top Four Firms',
         caption = 'Source: US Census Bureau Economic Census') +
    theme(axis.text.y = element_blank(),
          legend.position = 'none',
          panel.grid.major.y = element_blank())
