library('scales')

source('read-concentration-data.r')

df <- read_all_concentration_data('concentration-data', '*Z6.csv')
df <- df %>%
      # Only look at industry groups
      filter(nchar(NAICS.id) == 4) %>%
      # Discard anything focused on a particular tax category
      filter(is.na(OPTAX.id) | (OPTAX.id != 'T' & OPTAX.id != 'Y')) %>%
      # Discard rows with missing stats for disclosure reasons
      filter(VAL_PCT != 'D')

# Look at top four firms
top4 <- filter(df, CONCENFI.id == '804')
tot <- df %>%
       filter(CONCENFI.id == '001') %>%
       select(NAICS.id, RCPTOT, EMP, PAYANN, ESTAB)

df <- inner_join(top4, tot, 'NAICS.id', suffix=c('.top4', '.tot'))

# These columns should all be numbers
df <- mutate(df, VAL_PCT = as.double(VAL_PCT),
                 EMP.tot = as.double(EMP.tot),
                 RCPTOT.tot = as.double(RCPTOT.tot))

# RCPTOT is in thousands, let's do trillions
df <- mutate(df, RCPTOT.tot = RCPTOT.tot / 1000000000)

# Need NAICS.label to be a factor to make ggplot happy
df <- mutate(df, NAICS.label = as.factor(NAICS.label))

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
ggsave('static-graphs/raw/sizes.png')
