# Helper script to graph plots stored under /plots
args = commandArgs(trailingOnly = TRUE)

for (filename in args) {
    source(filename)
    base_filename <- gsub('^.*/', '', filename)
    png_filename <- gsub('.r', '.png', base_filename, fixed = TRUE)
    ggsave(paste('static-graphs/raw/', png_filename, sep = ''),
           width = 9, height = 9)
}
