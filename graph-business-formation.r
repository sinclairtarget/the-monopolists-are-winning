library('tidyverse')
library('scales')

df <- read_csv('business-formation-data/BF4Q_ST.csv')
df <- df %>%
      gather(state, n, -Time) %>%
      filter(grepl('*Q4$', Time)) %>%
      mutate(Time = as.factor(gsub(' Q[0-9]$', '', Time))) %>%
      rename(time = Time) %>%
      mutate(state = as.factor(state))

# We want to highlight CA, NY, WA, TX
df <- df %>%
      mutate(highlight = as.factor(ifelse(state %in% c('NY', 'CA', 'WA', 'TX'),
                                    state,
                                    NA)))
df

to_label <- df %>%
            filter(!is.na(highlight) & time == 2015)

ggplot(df, aes(x = time, y = n, color = highlight)) +
    geom_line(data=filter(df, is.na(highlight)), aes(group = state)) +
    geom_line(data=filter(df, !is.na(highlight)), aes(group = state), size = 1) +
    geom_label(data=to_label, aes(label = state)) +
    scale_y_continuous(limits = c(0, 15000),
                       breaks=seq(0, 15000, 2500),
                       labels = comma) +
    guides(color = FALSE) +
    labs(title = 'Q4 Business Formations by Year',
         x = 'Year',
         y = 'Number of Businesses')
ggsave('static-graphs/raw/business-formation.png')
