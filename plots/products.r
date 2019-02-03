library('tidyverse')
library('treemapify')

source('lib/complaints-data.r')

df <- read_complaints('consumer-complaints.csv') %>%
      filter(!is.na(product) & !is.na(subproduct))


# Get top five products
df.products <- df %>%
               count(product) %>%
               top_n(5) %>%
               select(product)

# We are interested in counts of product/subproduct combinations
df.subproducts <- df %>%
                  count(product, subproduct)

# Strip down to top five products
df<- inner_join(df.subproducts, df.products, by='product')

ggplot(df, aes(area = n,
               fill = product,
               label = subproduct,
               subgroup = product)) +
    geom_treemap() +
    geom_treemap_subgroup_border() +
    geom_treemap_text(reflow = TRUE) +
    guides(fill = guide_legend(title = 'Product Category')) +
    labs(title = 'Most Complaints Against Debt Collectors and Mortage Providers',
         subtitle = 'Top Five CFPB Complaint Product Categories',
         caption = 'Source: CFPB Complaints Database') +
    theme(legend.position = 'bottom', legend.direction = 'vertical')
