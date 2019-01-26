library('tidyverse')

df <- read_csv('consumer-complaints.csv')
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
      mutate(product = as.factor(product))

# Strip down to top 9 product categories
counts <- df %>% count(product) %>% top_n(9)

ggplot(counts, aes(x = product, fill = product, y = n)) +
    geom_col() +
    coord_polar() +
    theme(legend.position = 'none')
ggsave('static-graphs/raw/complaints.png')
