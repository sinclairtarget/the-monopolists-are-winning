library('tidyverse')

read_concentration_data <- function(filename) {
    # Add a column full of NA if the column doesn't already exist
    default_col <- function(df, cols) {
        to_add <- cols[!cols %in% names(df)]
        if (length(to_add) > 0) {
            df[to_add] <- NA
        }

        df
    }

    df <- read_csv(filename)
    df <- default_col(df, c('OPTAX.id', 'OPTAX.display-label'))
    df <- df %>%
          select(NAICS.id,
                 'NAICS.display-label',
                 CONCENFI.id,
                 'CONCENFI.display-label',
                 OPTAX.id,
                 'OPTAX.display-label',
                 RCPTOT,
                 VAL_PCT,
                 EMP,
                 PAYANN,
                 ESTAB) %>%
          rename('NAICS.label' = 'NAICS.display-label',
                 'CONCENFI.label' = 'CONCENFI.display-label',
                 'OPTAX.label' = 'OPTAX.display-label') %>%
          mutate(NAICS.id = as.character(NAICS.id))
    df
}

read_all_concentration_data <- function(dir = ".", pattern = NULL) {
    files = list.files(dir, pattern)
    df <- NULL
    for (file in files) {
        path <- paste(dir, file, sep='/')
        df <- rbind(df, read_concentration_data(path))
    }
    df
}
