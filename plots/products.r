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
               color = product,
               fill = product,
               label = subproduct,
               subgroup = product)) +
    geom_treemap(alpha = 0.4) +
    geom_treemap_subgroup_border(size = 1.5) +
    geom_treemap_text(color = dark_gray, reflow = TRUE, size = 12) +
    scale_color_manual(values = c(teal, red, green, purple, blue)) +
    scale_fill_manual(values = c(teal, red, green, purple, blue)) +
    labs(title = 'Most Complaints Against Debt Collectors and Mortage Providers',
         subtitle = 'Top Five CFPB Complaint Product Categories and Subproducts',
         caption = 'Source: CFPB Complaints Database') +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank()) +
    # https://github.com/wilkox/treemapify/issues/33#issuecomment-463726364
    thematic_label('Debt Collection', x = 0.27, y = 0.597, hjust = 0.5, vjust = 1) +
    thematic_label('Mortage', x = 0.27, y = 0, hjust = 0.5, vjust = 1) +
    thematic_label('Credit Reporting or Repair Service', x = 0.85, y = 0, hjust = 0.5, vjust = 1) +
    thematic_label('Bank Account or Service', x = 0.85, y = 0.635, hjust = 0.5, vjust = 1) +
    thematic_label('Student Loan', x = 0.85, y = 0.926, hjust = 0.5, vjust = 1) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1))
