library('tidyverse')
library('scales')

df.business <- read_csv('business-formation-data/BF4Q_ST.csv')
df.business <- df.business %>%
      gather(state, n, -Time) %>%
      filter(grepl('*Q4$', Time)) %>%
      mutate(Time = as.integer(gsub(' Q[0-9]$', '', Time))) %>%
      rename(year = Time) %>%
      mutate(state = as.factor(state))

df.complaints <- read_csv('consumer-complaints.csv')
df.complaints <- df.complaints %>%
      rename(date.received = 'Date received',
             product = Product,
             subproduct = 'Sub-product',
             issue = Issue,
             subissue = 'Sub-issue',
             complaint.narrative = 'Consumer complaint narrative',
             company.response = 'Company public response',
             company = Company,
             state = State,
             tags = Tags) %>%
      select(date.received,
             product,
             subproduct,
             issue,
             subissue,
             complaint.narrative,
             company.response,
             company,
             state,
             tags) %>%
      mutate(state = as.factor(state),
             date.received = as.Date(date.received, '%m/%d/%Y'),
             year = as.integer(format(date.received, '%Y')))

# Let's get rid of anything that isn't one of the 50 states
states <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI',
            'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI',
            'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC',
            'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT',
            'VT', 'VA', 'WA', 'WV', 'WI', 'WY')
df.complaints <- filter(df.complaints, state %in% states)

# Let's also get rid of anything later than 2018
df.complaints <- filter(df.complaints, year < 2019)

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
ggplot(df.complaints, aes(x = year, y = n.change, color = state)) +
    geom_line() +
    scale_x_discrete(limits = c(2011, 2018)) +
    facet_wrap(~ state) +
    labs(title = 'CFPB Complaints Have Risen Across the Country',
         subtitle = 'CFPB Complaints By State (2011-2018)',
         x = 'Year',
         y = 'Proportional Rise in Complaints Since 2011 Baseline',
         caption = 'Source: CFPB Complaints Database') +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle=60, hjust=1),
          panel.spacing.x = unit(3, 'mm'))
