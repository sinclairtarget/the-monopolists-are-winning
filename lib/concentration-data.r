library('tidyverse')

read_concentration_data <- function(filename) {
    # Add a column full of NA if the column doesn't already exist
    default_col <- function(df, cols) {
        to_add <- cols[!cols %in% names(df)]
        if (length(to_add) > 0) {
            df[to_add] <- NA
        }

        df }

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
          mutate(NAICS.id = as.character(NAICS.id),
                 NAICS.label = as.factor(NAICS.label),
                 VAL_PCT = as.double(VAL_PCT),
                 EMP = as.double(EMP),
                 RCPTOT = as.double(RCPTOT))

    # Ignore rows that are specific to certain tax statuses
    df <- filter(df, is.na(OPTAX.id) | (OPTAX.id != 'T' & OPTAX.id != 'Y'))

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

by_top4 <- function(df) {
    top4 <- filter(df, CONCENFI.id == '804')
    tot <- df %>%
           filter(CONCENFI.id == '001') %>%
           select(NAICS.id, RCPTOT, EMP, PAYANN, ESTAB)
    inner_join(top4, tot, 'NAICS.id', suffix=c('.top4', '.tot'))
}
