library('tidyverse')
library('scales')

df <- read_csv('business-formation-data/BF4Q_ST.csv')
df <- df %>%
      gather(state, n, -Time) %>%
      mutate(year = as.integer(gsub(' Q[0-9]$', '', Time)),
             quarter = as.factor(gsub('^[0-9]* Q', '', Time)),
             state = as.factor(state)) %>%
      select(-Time) %>%
      # Only have data for all quarters between these years
      filter(year > 2004 & year <= 2015) %>%
      mutate(year = as.factor(year))

# Group quarters so we get annual count
df <- df %>% group_by(state, year) %>% summarize(n = sum(n))

# We want to highlight CA, NY, WA, TX
df <- df %>%
      mutate(highlight = as.factor(ifelse(state %in% c('NY', 'CA', 'WA', 'TX'),
                                    state,
                                    NA)))

 to_label <- df %>%
             filter(!is.na(highlight) & year == 2015)

# ---- plot.formation ----
ggplot(df, aes(x = year, y = n, color = highlight)) +
    geom_line(data=filter(df, is.na(highlight)), aes(group = state)) +
    geom_line(data=filter(df, !is.na(highlight)), aes(group = state), size = 1) +
    geom_label(data=to_label, aes(label = state)) +
    scale_y_continuous(limits = c(0, 70000),
                       breaks = seq(0, 70000, 10000),
                       labels = comma) +
    guides(color = FALSE) +
    labs(title = 'Americans Are Starting Fewer Businesses Than a Decade Ago',
         subtitle = 'Business Formations by Year',
         captions = 'Source: US Census Bureau Business Formation Statistics',
         x = 'Year',
         y = 'Number of Businesses')
