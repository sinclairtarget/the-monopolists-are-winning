library('tidyverse')
library('scales')
library('ggalt')
source('theme.r')

theme_set(my_theme())

states <- c('Alabama' ='AL',
            'Alaska' = 'AK',
            'Arizona' = 'AZ',
            'Arkansas' = 'AR',
            'California' = 'CA',
            'Colorado' = 'CO',
            'Connecticut' = 'CT',
            'Delaware' = 'DE',
            'Florida' = 'FL',
            'Georgia' = 'GA',
            'Hawaii' = 'HI',
            'Idaho' = 'ID',
            'Illinois' = 'IL',
            'Indiana' = 'IN',
            'Iowa' = 'IA',
            'Kansas' = 'KS',
            'Kentucky' = 'KY',
            'Louisiana' = 'LA',
            'Maine' = 'ME',
            'Maryland' = 'MD',
            'Massachusetts' = 'MA',
            'Michigan' = 'MI',
            'Minnesota' = 'MN',
            'Mississippi' = 'MS',
            'Missouri' = 'MO',
            'Montana' = 'MT',
            'Nebraska' = 'NE',
            'Nevada' = 'NV',
            'New Hampshire' = 'NH',
            'New Jersey' = 'NJ',
            'New Mexico' = 'NM',
            'New York' = 'NY',
            'North Carolina' = 'NC',
            'North Dakota' = 'ND',
            'Ohio' = 'OH',
            'Oklahoma' = 'OK',
            'Oregon' = 'OR',
            'Pennsylvania' = 'PA',
            'Rhode Island' = 'RI',
            'South Carolina' = 'SC',
            'South Dakota' = 'SD',
            'Tennessee' = 'TN',
            'Texas' = 'TX',
            'Utah' = 'UT',
            'Vermont' = 'VT',
            'Virginia' = 'VA',
            'Washington' = 'WA',
            'West Virginia' = 'WV',
            'Wisconsin' = 'WI',
            'Wyoming' = 'WY')

df.pop <- read_csv('population-data/populations.csv')
df.pop <- df.pop %>%
          gather('year', 'pop', -state) %>%
          filter(!is.na(state)) %>%
          mutate(state = states[state],
                 year = as.integer(year))

df <- read_csv('business-formation-data/BF4Q_ST.csv')
df <- df %>%
      gather(state, n, -Time) %>%
      mutate(year = as.integer(gsub(' Q[0-9]$', '', Time)),
             quarter = as.factor(gsub('^[0-9]* Q', '', Time))) %>%
      select(-Time) %>%
      # Only have data for all quarters between these years
      filter(year > 2004 & year <= 2015)

# Group quarters so we get annual count
df <- df %>%
      group_by(state, year) %>%
      summarize(n = sum(n)) %>%
      ungroup()

# Join with pop data to get per-capita business formation
# We want business formations per 1000 residents
df <- inner_join(df, df.pop, by = c('state', 'year'))
df <- df %>%
      mutate(rate = n / pop * 1000) %>%
      filter(year == 2005 | year == 2015) %>%
      select(state, year, rate) %>%
      spread(year, rate, sep='.') %>%
      rename(rate.2005 = year.2005, rate.2015 = year.2015) %>%
      mutate(rate.diff = rate.2015 - rate.2005)

# Want to highlight ID, MT, WY, UT, AZ, CO, NV
highlight_states <- c('ID', 'MT', 'WY', 'UT', 'AZ', 'CO', 'NV')
df <- df %>%
      mutate(highlight = as.factor(ifelse(state %in% highlight_states,
                                    state,
                                    NA)))

# Order state factor
df <- mutate(df, state =
        factor(state, levels = df$state[order(df$rate.diff, decreasing=TRUE)]))

df.labels <- df %>% filter(!is.na(highlight))

rate_label <- function(rate) {
    sprintf("%0.2f", round(rate, digits = 2))
}

label_padding <- unit(1.8, 'pt')

# ---- plot.entrepreneurship ----
ggplot(df, aes(x = rate.2005, xend = rate.2015, y = state)) +
    geom_dumbbell(aes(color = ifelse(is.na(highlight), NA, 'yes')),
                  colour_x = gray,
                  size_x = 1, size_xend = 1.5, alpha = 0.8) +
    geom_label(data = df.labels, aes(label = rate_label(rate.diff)),
               hjust = 'left',
               size = 3,
               x = 3.18,
               color = red,
               label.size = NA,
               fill = light_gray,
               label.padding = label_padding,
               label.r = unit(0, 'lines')) +
    scale_x_continuous(limits = c(0.5, 3.25),
                       breaks = seq(0, 3.25, 0.25)) +
    scale_color_manual(values = c(red), na.value = gray) +
    labs(title = 'Business Formation Rate Fell Furthest in Western/Mountain States',
         subtitle = 'Per Capita Business Formation by State, 2005 vs 2015',
         caption = 'Source: US Census Bureau Business Formation Statistics',
         x = 'Businesses Formed Per 1000 Residents') +
    geom_label(aes(x = 1.1,
                   y = 50,
                   label = '2015'),
                   label.size = NA,
                   label.r = unit(0, 'lines'),
                   label.padding = label_padding,
                   size = 3,
                   color = annotation_color,
                   fill = light_gray) +
    geom_label(aes(x = 3.02,
                   y = 50,
                   label = '2005'),
                   label.size = NA,
                   label.r = unit(0, 'lines'),
                   label.padding = label_padding,
                   size = 3,
                   color = annotation_color,
                   fill = light_gray) +
    theme(axis.title.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = 'none')
