library('tidyverse')
library('treemapify')

source('lib/complaints-data.r')
source('theme.r')

theme_set(my_theme())

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

# ---- plot.products ----
ggplot(df, aes(area = n,
               fill = product,
               label = subproduct,
               subgroup = product)) +
    geom_treemap(alpha = 0.5) +
    geom_treemap_subgroup_border(aes(color = product), size = 1.5) +
    geom_treemap_text(color = 'gray20', reflow = TRUE) +
    scale_color_manual(values = c(teal, red, green, purple, blue)) +
    scale_fill_manual(values = c(teal, red, green, purple, blue)) +
    guides(fill = guide_legend(title = 'Product Category'),
           color = 'none') +
    labs(title = 'Most Complaints Against Debt Collectors and Mortage Providers',
         subtitle = 'Top Five CFPB Complaint Product Categories and Subproducts',
         caption = 'Source: CFPB Complaints Database') +
    theme(legend.position = 'bottom', legend.direction = 'vertical')
