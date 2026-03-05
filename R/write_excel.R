library(pacnvegetation)
library(tidyverse)

pacn_bish_crosswalk <- read_csv(file = "data/pacn_bish_crosswalk_with_synonyms.csv")

#pacn_bish_crosswalk_no_commas <- pacn_bish_crosswalk |>
#  mutate(across(where(is.character), ~ str_replace_all(., ",", ";")))

write_excel_csv(pacn_bish_crosswalk, "data/pacn_bish_crosswalk_write_excel.csv")

write.csv()








#write.csv(pacn_bish_crosswalk_no_commas, "data/pacn_bish_crosswalk_no_commas.csv")

is_win1252 <- function(x) {
  # x: character vector
  y <- iconv(x, from = "", to = "Windows-1252", sub = NA)
  !is.na(y)
}
bad_matrix <- as.data.frame(
  lapply(pacn_bish_crosswalk, function(col) !is_win1252(as.character(col)))
)

# Any bad cells?
any(bad_matrix, na.rm = TRUE)

# Row indices with at least one bad character
bad_rows <- which(apply(bad_matrix, 1, any, na.rm = TRUE))
bad_rows

# Column names with at least one bad character
bad_cols <- names(pacn_bish_crosswalk)[sapply(bad_matrix, any, na.rm = TRUE)]
bad_cols

# Check ----
# View all problematic cells (row, column, value)
which(bad_matrix, arr.ind = TRUE)

# Example: look at first problematic row
head(pacn_bish_crosswalk[bad_rows, bad_cols], 20)

# Extract actual offending characters in a vector x
find_offending_chars <- function(x) {
  x <- as.character(x)
  bad <- !is_win1252(x)
  x_bad <- x[bad]
  
  # Split into individual characters and test each
  chars <- unique(unlist(strsplit(x_bad, "")))
  bad_chars <- chars[!is_win1252(chars)]
  bad_chars
}

offending_chars <- unique(unlist(
  lapply(pacn_bish_crosswalk[bad_cols], find_offending_chars)
))
offending_chars
