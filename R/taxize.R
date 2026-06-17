##############################################-
#  ITIS TSN EXTRACTION 
#   - 
#  
#   
#   
##############################################-

library(tidyverse)
library(readxl)
library(taxize)

##############################################-
# 1. LOAD YOUR INPUT DATA----
##############################################-

species_tbl <- read_excel("data/ITIS_TSN_extraction_start.xlsx") |>
  rowid_to_column(var = "temp_id")

#test_list <- c("Chloris barbata", 
#               "Arthrothamnus tirucalli",
#               "Melicope retusa")

#test_spp_tbl <- species_tbl |>
#  filter(Scientific_Name %in% test_list)

##############################################-
# 2. CREATE CHUNKS ----
##############################################-

# Define rows per chunk
chunk_size <- 100

# Split the tibble into a list of chunks
chunks <- species_tbl %>% 
  group_split(chunk_id = (row_number() - 1) %/% chunk_size)


# Force HTTP/1.1 globally for curl — avoids the HTTP2 framing error
httr::set_config(httr::config(http_version = 2)) # 2 = HTTP 1.1 in libcurl's enum
# (http_version values: 0 = default, 1 = HTTP 1.0, 2 = HTTP 1.1, 3 = HTTP 2)


# Handle partially completed 
file.exists("data/tsn_progress.rds")

if (file.exists("data/tsn_progress.rds")) {
  all_chunks <- readRDS("data/tsn_progress.rds")
  done_ids <- all_chunks$temp_id
  chunks <- lapply(chunks, \(ch) filter(ch, !temp_id %in% done_ids))
  chunks <- chunks[sapply(chunks, nrow) > 0]
} else {
  all_chunks <- tibble()
}

#all_chunks_backup2 <- all_chunks
#saveRDS(all_chunks_backup, "data/backup_all_chunks.rds")
#chunks_backup2 <- chunks
#saveRDS(chunks_backup, "data/backup_chunks.rds")


############################################################-
# 3. get TSN function with retry + error handling -----
############################################################-
get_tsn_from_spp <- function(spp, max_tries = 4, base_sleep = 2) {
  for (attempt in seq_len(max_tries)) {
    result <- tryCatch(
      taxize::get_tsn_(spp, rows = NA, accepted = FALSE),
      error = function(e) e
    )
    
    if (!inherits(result, "error")) {
      return(result)
    }
    
    message(sprintf("  attempt %d failed for '%s': %s", 
                    attempt, spp, conditionMessage(result)))
    Sys.sleep(base_sleep * attempt)  # linear backoff: 2s, 4s, 6s, 8s
  }
  
  # All retries exhausted — return NA so the loop continues
  warning(sprintf("All %d attempts failed for '%s'", max_tries, spp))
  return(NA)
}

##############################################-
# 4. PROCESS BATCHES, SAVING AS YOU GO ----
##############################################-


for (x in seq_along(chunks)) {
  
  tsn_chunk <- tryCatch(
    chunks[[x]] |>
      rowwise() |>
      mutate(TSN = get_tsn_from_spp(Scientific_Name)) |>
      ungroup(),
    error = function(e) {
      message(sprintf("Chunk %d failed entirely: %s", x, conditionMessage(e)))
      NULL
    }
  )
  
  if (!is.null(tsn_chunk)) {
    all_chunks <- bind_rows(all_chunks, tsn_chunk)
    
    # Save intermediate progress so a crash doesn't lose everything
    saveRDS(all_chunks, "data/tsn_progress.rds")
    print(paste("chunk", x, "complete —", nrow(all_chunks), "rows saved"))
  }
  
  Sys.sleep(1)  # polite pause between chunks
}

##############################################-
# 5. Keep only exact matches with scientific name (not var. subsp. etc) ----
##############################################-
library(dplyr)
library(purrr)
library(stringr)

#' Filter a nested TSN list-column to exact matches of Scientific_Name,
#' keeping rows with TSN=NULL (set to NA), and preserving nested tibbles.
#'
#' @param df A tibble with a list-column 'TSN' (nested tibbles or NULL).
#' @param tsn_col Name of list-column with nested TSN tibbles.
#' @param sci_col Name of column with target scientific names per row.
#' @param name_col_inside_tsn Column name inside nested TSN tibble that holds the taxon name.
#' @param set_null_to_na If TRUE, TSN=NULL rows are kept and set to NA.
#' @param keep_nonmatches_as_na If TRUE, rows whose TSN exists but has no exact match are kept with TSN=NA. If FALSE, such rows are dropped.
#' @param squish_ws If TRUE, trim/condense whitespace before matching.
#' @param ignore_case If TRUE, match case-insensitively.
filter_tsn_exact_nested <- function(df,
                                    tsn_col = "TSN",
                                    sci_col = "Scientific_Name",
                                    name_col_inside_tsn = "scientificName",
                                    set_null_to_na = TRUE,
                                    keep_nonmatches_as_na = FALSE,
                                    squish_ws = TRUE,
                                    ignore_case = FALSE) {
  
  # Normalize Scientific_Name once for speed
  target <- df[[sci_col]]
  if (squish_ws)  target <- stringr::str_squish(target)
  if (ignore_case) target <- stringr::str_to_lower(target)
  
  df %>%
    mutate(
      .target = target,
      
      # Build the replacement nested object per row: tibble of matches, NA, or NULL
      .matched_tsn = purrr::map2(.data[[tsn_col]], .target, function(tsn_tbl, tgt) {
        
        # Case A: original TSN is NULL -> keep row, set to NA
        if (is.null(tsn_tbl)) {
          if (set_null_to_na) return(NA) else return(NULL)
        }
        
        # Case B: TSN is a nested tibble -> filter to ALL exact matches
        nm <- tsn_tbl[[name_col_inside_tsn]]
        if (squish_ws)  nm <- stringr::str_squish(nm)
        if (ignore_case) nm <- stringr::str_to_lower(nm)
        
        out <- tsn_tbl[nm == tgt, , drop = FALSE]
        
        if (nrow(out) == 0L) {
          if (keep_nonmatches_as_na) return(NA) else return(NULL)
        } else {
          # Keep *all* exact matches (1..n rows) inside the nested tibble
          out
        }
      }),
      
      # SAFE single-boolean flag for filter()
      # - TRUE if element is a data.frame with >0 rows
      # - TRUE if element is a scalar NA
      # - FALSE otherwise
      .keep_flag = purrr::map_lgl(.matched_tsn, function(x) {
        if (is.data.frame(x)) {
          nrow(x) > 0L
        } else if (is.atomic(x) && length(x) == 1L) {
          is.na(x)
        } else {
          FALSE
        }
      })
    ) %>%
    filter(.keep_flag) %>%
    mutate(!!tsn_col := .matched_tsn) %>%
    select(-.target, -.matched_tsn, -.keep_flag)
}


look <- filter_tsn_exact_nested(all_chunks)


all_tsn_final <- look %>%    
  mutate(
    taxize_tsn = map_chr(TSN, function(x) {
      
      # Case 1: TSN is NA (TSN was originally NULL or had no matches)
      if (is.atomic(x) && length(x) == 1 && is.na(x)) {
        return(NA_character_)
      }
      
      # Case 2: TSN is a nested tibble
      if (is.data.frame(x)) {
        
        # One exact match inside nested tibble
        if (nrow(x) == 1) {
          return(x$tsn[[1]])
        }
        
        # More than one exact match – keep the nested tibble, but mark MULTIPLE
        if (nrow(x) > 1) {
          return("MULTIPLE")
        }
      }
      
      # Fallback (should never happen)
      NA_character_
    })
  )


##############################################-
# 6. SAVE ----
##############################################-

ITIS_TSN_extraction_taxize <- all_tsn_final |>
  select(Species_ID, Scientific_Name, Type, TSN = taxize_tsn)

how_many_got_tsn <- ITIS_TSN_extraction_taxize |>
  filter(!is.na(as.numeric(TSN)))

how_many_na <- ITIS_TSN_extraction_taxize |>
  filter(is.na(TSN))

readr::write_excel_csv(ITIS_TSN_extraction_taxize, "data/taxize/ITIS_TSN_extraction_taxize.csv")

multiples <- all_tsn_final |>
  filter(taxize_tsn == "MULTIPLE")

plant_names_with_multiple_tsn_matches <- multiples |> 
  unnest(TSN) |> 
  select(Species_ID, Scientific_Name, Type, TSN = taxize_tsn, multiple_tsn = tsn) 

readr::write_excel_csv(plant_names_with_multiple_tsn_matches, "data/taxize/plant_names_with_multiple_tsn_matches.csv")
