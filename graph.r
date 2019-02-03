# Helper script to graph plots stored under /plots
source('theme.r')
theme_set(my_theme())

args = commandArgs(trailingOnly = TRUE)

for (filename in args) {
    source(filename)
    base_filename <- gsub('^.*/', '', filename)
    png_filename <- gsub('.r', '.png', base_filename, fixed = TRUE)
    ggsave(paste('static-graphs/raw/', png_filename, sep = ''))
}
