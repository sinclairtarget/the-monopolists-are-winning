library('tidyverse')

read_complaints <- function(filename) {
    df <- read_csv(filename)
    df <- df %>%
          rename(date.received = 'Date received',
                 product = Product,
                 subproduct = 'Sub-product',
                 issue = Issue,
                 subissue = 'Sub-issue',
                 complaint.narrative = 'Consumer complaint narrative',
                 company.response = 'Company public response',
                 company = Company,
                 state = State,
                 tags = Tags,
                 zip = 'ZIP code') %>%
          select(date.received,
                 product,
                 subproduct,
                 issue,
                 subissue,
                 complaint.narrative,
                 company.response,
                 company,
                 state,
                 tags,
                 zip) %>%
          mutate(state = as.factor(state),
                 product = as.factor(product),
                 subproduct = as.factor(subproduct),
                 date.received = as.Date(date.received, '%m/%d/%Y'),
                 year = as.integer(format(date.received, '%Y')))

    # Let's get rid of anything that isn't one of the 50 states (and DC)
    states <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI',
                'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI',
                'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC',
                'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT',
                'VT', 'VA', 'WA', 'WV', 'WI', 'WY', 'DC')
    df <- filter(df, state %in% states)

    # Let's also get rid of anything later than 2018
    df <- filter(df, year < 2019)

    df
}
