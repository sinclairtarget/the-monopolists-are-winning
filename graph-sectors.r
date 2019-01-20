source('read-concentration-data.r')

df <- read_all_concentration_data('concentration-data', '*Z6.csv')
df <- df %>%
      # Only look at sectors
      filter(nchar(NAICS.id) == 2) %>%
      # Discard anything focused on a particular tax category
      filter(is.na(OPTAX.id) | (OPTAX.id != 'T' & OPTAX.id != 'Y')) %>%
      # Discard rows with missing stats for disclosure reasons
      filter(VAL_PCT != 'D')

# VAL_PCT should really be a number
df <- mutate(df, VAL_PCT = as.double(VAL_PCT))

# Look at top four firms
top4 <- filter(df, CONCENFI.id == '804')
tot <- df %>%
       filter(CONCENFI.id == '001') %>%
       select(NAICS.id, RCPTOT, EMP, PAYANN, ESTAB)

df <- inner_join(top4, tot, 'NAICS.id', suffix=c('.top4', '.tot'))

# Add highlight column
df <- mutate(df, is_information = ifelse(NAICS.id == '51', 'yes', 'no'))

# Need NAICS.label to be a factor to make ggplot happy
df <- mutate(df, NAICS.label = as.factor(NAICS.label))

ggplot(df, aes(x = reorder(NAICS.label, VAL_PCT), y = VAL_PCT)) +
    geom_bar(aes(fill = is_information), stat = 'identity') +
    geom_text(aes(label = NAICS.label),
              hjust = 'left',
              size = 3,
              nudge_y = 2) +
    geom_text(aes(label = ifelse(is_information == 'yes', VAL_PCT, '')),
              hjust = 'right',
              size = 3,
              nudge_y = -2) +
    scale_y_continuous(limits = c(0, 100), breaks=seq(0, 100, 10)) +
    scale_fill_manual(values = c('no' = 'slategray3', 'yes' = 'orange2')) +
    coord_flip() +
    labs(title = 'Market Concentration by NAICS Sector',
         x = 'NAICS Sector',
         y = '% of Total Revenue Captured by Top Four Firms') +
    theme(axis.text.y = element_blank(),
          legend.position = 'none',
          panel.grid.major.y = element_blank())
ggsave('static-graphs/raw/sectors.png')
