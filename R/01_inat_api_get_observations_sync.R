library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)

# helper for printing NULL nicely
`%||%` <- function(x, y) if (is.null(x)) y else x

get_inat_obs_multi_place <- function(place_ids,
                                     taxon_id,
                                     created_d1 = NULL,   # upload start date "YYYY-MM-DD"
                                     created_d2 = NULL,   # upload end date   "YYYY-MM-DD"
                                     per_page = 200,
                                     max_pages = 250) {
  base <- "https://api.inaturalist.org/v1/observations"
  
  get_page <- function(place_id, page) {
    q <- list(
      place_id = place_id,
      taxon_id = taxon_id,
      per_page = per_page,
      page     = page
    )
    
    # created (upload) date filters
    if (!is.null(created_d1)) q$created_d1 <- created_d1
    if (!is.null(created_d2)) q$created_d2 <- created_d2
    
    resp <- GET(base, query = q)
    dat <- content(resp, as = "text", encoding = "UTF-8") |>
      fromJSON(flatten = TRUE)
    dat$results
  }
  
  start_time <- Sys.time()
  
  all_results <- map(place_ids, function(pid) {
    pages <- vector("list", max_pages)
    
    for (p in seq_len(max_pages)) {
      res <- get_page(pid, p)
      if (length(res) == 0) {
        pages <- pages[seq_len(p - 1)]
        break
      }
      if (length(res) > 0) {
        res$place_id_query <- pid
      }
      pages[[p]] <- res
    }
    
    out <- bind_rows(pages)
    
    message(
      "place_id ", pid, ": ", nrow(out), " records retrieved",
      if (!is.null(created_d1) || !is.null(created_d2))
        paste0(" (created_d1 = ", created_d1 %||% "NULL",
               ", created_d2 = ", created_d2 %||% "NULL", ")")
    )
    
    out
  })
  
  out_all <- bind_rows(all_results)
  
  end_time <- Sys.time()
  elapsed <- end_time - start_time
  message("Total time: ", round(as.numeric(elapsed, units = "secs"), 1), " seconds")
  
  out_all
}

park <- c('American Memorial Park','Haleakala National Park', 'Hawaii Volcanoes National Park','Kalaupapa National Historical Park', 'Kaloko-Honokohau National Historical Park','War in the Pacific National Historical Park')
parkName <- c('AMME','HALE','HAVO','KALA','KAHO','WAPA')
placeID <- c(97397,56788,7222,95256,95255,95342)
pacn_inat_places <- data.frame(park,parkName,placeID)
write_csv(pacn_inat_places, "data/pacn_inat_places.csv")

# get table from last sync
inat_obs_old <- readRDS("data/inat_park_obs.rds")

# Updated observations ----
ids <- inat_obs_old$id
base <- "https://api.inaturalist.org/v1/observations"

chunk_ids <- function(x, size = 200) split(x, ceiling(seq_along(x) / size))
id_chunks <- chunk_ids(ids, size = 200)

freq_table <- sort(table(inat_obs_updated$last_updated_since), decreasing = TRUE)
mode_value <- names(freq_table)[freq_table == max(freq_table)]
updated_since <- mode_value
#updated_since <- "2025-11-01T00:00:00Z"   # your cutoff

get_chunk <- function(v, max_tries = 5) {
  ids_str <- paste(v, collapse = ",")
  
  for (i in seq_len(max_tries)) {
    res <- GET(
      base,
      query = list(
        id            = ids_str,
        updated_since = updated_since
      ),
      user_agent("jacob_gross@nps.gov/early-detection-of-new-plant-species-in-national-parks")  # polite UA
    )
    
    status <- status_code(res)
    
    # Respect 429: back off and retry
    if (status == 429) {
      message("429 for IDs chunk starting with ", v[1],
              " (try ", i, "/", max_tries, "). Backing off...")
      Sys.sleep(5 * i)  # exponential backoff
      next
    }
    
    if (status != 200) {
      message("HTTP error ", status, " for IDs chunk starting with ", v[1])
      txt <- content(res, "text", encoding = "UTF-8")
      message("Body (first 200 chars):\n", substr(txt, 1, 200))
      return(tibble())  # or stop() if you’d rather fail
    }
    
    txt <- content(res, "text", encoding = "UTF-8")
    
    # Ensure it looks like JSON
    if (!startsWith(trimws(txt), "{") && !startsWith(trimws(txt), "[")) {
      message("Non-JSON response for IDs chunk starting with ", v[1])
      message("Body (first 200 chars):\n", substr(txt, 1, 200))
      return(tibble())
    }
    
    dat <- fromJSON(txt, flatten = TRUE)
    return(as_tibble(dat$results))
  }
  
  # If all retries failed
  message("Giving up on chunk starting with ", v[1], " after ", max_tries, " tries.")
  tibble()
}

# Current time in UTC
now_utc <- as.POSIXct(Sys.time(), tz = "UTC")

obs_df <- map_dfr(id_chunks, function(v) {
  Sys.sleep(1)  # be nice to the API
  get_chunk(v)
})



key <- "uuid"
# Columns that exist in both tables (and are candidates to be updated)
common_cols <- intersect(names(inat_obs_old), names(obs_df))
common_cols <- setdiff(common_cols, key)

joined <- inat_obs_old %>%
  left_join(
    obs_df %>%
      select(all_of(c(key, common_cols))),
    by = key,
    suffix = c(".old", ".new")
  )

inat_obs_updated <- joined %>%
  mutate(
    !!!set_names(
      map(
        common_cols,
        ~ coalesce(
          joined[[paste0(.x, ".new")]],
          joined[[paste0(.x, ".old")]]
        )
      ),
      common_cols
    )
  ) %>%
  select(-ends_with(".old"), -ends_with(".new"))






# New observations ----

last_updated <- as.character(max(as.Date(inat_obs_old$created_at, na.rm = TRUE))) 
today <- as.character(format(Sys.Date()))
str(today)

# new observations
inat_obs_new <- get_inat_obs_multi_place(
  place_ids = pacn_inat_places$placeID,
  taxon_id  = 211194, # Plantae
  created_d1 = last_updated,
  created_d2 = today
)


#new_inat_obs_2025_2026 <- new_inat_obs
#inat_park_obs <- bind_rows(new_inat_obs_2008_2015, new_inat_obs_2016_2019, new_inat_obs_2020_2022,
#          new_inat_obs_2023_2024, new_inat_obs_2025_2026)

inat_obs_updated_and_new <- bind_rows(inat_obs_updated, inat_obs_new) 

inat_obs_updated_and_new$last_updated_since <- format(now_utc, format = "%Y-%m-%dT%H:%M:%SZ")

dt_hst <- with_tz(now_utc, "Pacific/Honolulu")
dt_hst_stamp <- format(dt_hst, "%Y-%m-%d-%Hh-%Mm-%Ss-HST")

# Backup old .rds
saveRDS(inat_obs_old, file = paste0("data/backup/inat_park_obs_", dt_hst_stamp,".rds"))
# Save
saveRDS(inat_obs_updated_and_new, file = paste0("data/inat_park_obs.rds"))


