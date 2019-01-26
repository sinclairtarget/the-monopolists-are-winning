library('scales')

source('lib/concentration-data.r')

df <- read_all_concentration_data('concentration-data/2012', '*Z6.csv')

# Only look at industry groups
df <- df %>%
      filter(nchar(NAICS.id) == 4) %>%
      filter(!is.na(VAL_PCT))

# Look at top four firms
df <- by_top4(df)

# RCPTOT is in thousands, let's do trillions
df <- mutate(df, RCPTOT.tot = RCPTOT.tot / 1000000000)

ggplot(df, aes(x = VAL_PCT, y = RCPTOT.tot)) +
    geom_point(aes(size = EMP.tot, color = VAL_PCT)) +
    scale_x_continuous(limits = c(0, 100), breaks=seq(0, 100, 10)) +
    scale_size_continuous(labels = comma) +
    scale_color_gradient(low = hsv(181/360, 1, 0.5),
                         high = hsv(181/360, 1, 0.1)) +
    guides(color = FALSE) +
    labs(title = 'NAICS Industry Group Concentration and Size',
         x = '% of Total Revenue Captured by Top Four Firms',
         y = 'Annual Revenue (Trillions of $)',
         size = 'Total Employees') +
    theme(legend.position = c(0.82, 0.75))
