library('tidyverse')
library('scales')
library('geofacet')

source('lib/complaints-data.r')
source('theme.r')

theme_set(my_theme())

df.business <- read_csv('business-formation-data/BF4Q_ST.csv')
df.business <- df.business %>%
      gather(state, n, -Time) %>%
      filter(grepl('*Q4$', Time)) %>%
      mutate(Time = as.integer(gsub(' Q[0-9]$', '', Time))) %>%
      rename(year = Time) %>%
      mutate(state = as.factor(state))

df.complaints <- read_complaints('consumer-complaints.csv')

# We want to graph complaints in each state separately by year
df.complaints <- count(df.complaints, state, year)

# Let's graph the proportional change with 2011 as the baseline
baseline <- df.complaints %>%
            group_by(state) %>%
            filter(year == min(year)) %>%
            select(state, n)
df.complaints <- inner_join(df.complaints,
                            baseline,
                            'state',
                            suffix=c('', '.baseline'))
df.complaints <- mutate(df.complaints, n.change = n / n.baseline)

df <- inner_join(df.complaints, df.business, by=c('state', 'year'),
                 suffix=c('.complaints', '.business'))

# ---- plot.complaints ----
ggplot(df.complaints, aes(x = year, y = n.change)) +
    geom_area(fill = teal, alpha = 0.4) +
    geom_line(color = teal) +
    scale_x_discrete(limits = c(2011, 2018)) +
    facet_geo(~ state) +
    labs(title = 'CFPB Complaints Have Risen Across the Country',
         subtitle = 'CFPB Complaints By State (2011-2018)',
         x = 'Year',
         y = 'Proportional Rise in Complaints Since 2011 Baseline',
         caption = 'Source: CFPB Complaints Database') +
    theme(legend.position = 'none',
          axis.text.x = element_text(hjust=c(0, 0.8), size=6),
          axis.text.y = element_text(size=6),
          panel.spacing.x = unit(3, 'mm'))
