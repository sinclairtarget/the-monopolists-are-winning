library('tidyverse')
library('sf')

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
      mutate(rate.diff = rate.2015 - rate.2005) %>%
      select(state, rate.diff) %>%
      rename(Code = state)

df.geo <- st_read('geo/equiv_area/US_HexBinStates_EPSG2163.shp')
df <- inner_join(df.geo, df, by='Code') %>%
      mutate(Code = as.factor(Code))

scl <- function(x) { x * 100000 }

ggplot(df) +
    geom_sf(aes(fill = rate.diff)) +
    coord_sf(datum = NA) +                        # No grid lines/graticules
    geom_sf_text(aes(label = Code), alpha = 0.7) +
    annotate(geom = 'label',
             x = scl(-10),
             y = scl(10),
             hjust = 0,
             label = paste('In 2015, there were 1.6 fewer',
                           'businesses formed\nin Idaho (per 1000 residents)',
                           'than in 2005.',
                           separator = ''),
             color = annotation_color,
             size = 3,
             label.size = NA,
             label.r = unit(0, 'lines'),
             label.padding = unit(0.5, 'lines'),
             fill = light_gray) +
    annotate(geom = 'segment', x = scl(-14), xend = scl(-10), y = scl(10),
             yend = scl(10), color = 'gray20', size = 0.4,
             linetype = 'dotted') +
    annotate(geom = 'segment', x = scl(-14), xend = scl(-14), y = scl(10),
             yend = scl(-0.25), color = 'gray20', size = 0.4,
             linetype = 'dotted') +
    scale_fill_continuous(low = red, high = gray) +
    guides(fill = guide_colorbar(
             title = 'Change in Business Formation per 1000 Residents',
             title.position = 'top',
             barwidth = 10)) +
    labs(title = 'Business Formation Fell Furthest in Western/Mountain States',
         subtitle = 'Per Capita Business Formation by State, 2005 vs 2015',
         caption = 'Source: US Census Bureau Business Formation Statistics') +
    theme(axis.title = element_blank(),
          aspect.ratio = 0.77)

