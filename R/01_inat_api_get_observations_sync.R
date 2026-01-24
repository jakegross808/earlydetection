library(httr)
library(jsonlite)
library(dplyr)
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


# get info from last sync
inat_park_obs <- readRDS("data/inat_park_obs.rds")

last_updated <- as.character(max(as.Date(inat_observations$created_at, na.rm = TRUE))) 
#last_updated <- format(x = last_updated, "%Y-%m-%d")
today <- as.character(format(Sys.Date()))
str(today)

# new observations
new_inat_obs <- get_inat_obs_multi_place(
  place_ids = pacn_inat_places$placeID,
  taxon_id  = 211194, # Plantae
  created_d1 = last_updated,
  created_d2 = today
)

#new_inat_obs_2025_2026 <- new_inat_obs
#inat_park_obs <- bind_rows(new_inat_obs_2008_2015, new_inat_obs_2016_2019, new_inat_obs_2020_2022,
#          new_inat_obs_2023_2024, new_inat_obs_2025_2026)

inat_park_obs <- bind_rows(inat_park_obs, new_inat_obs)

# Save
saveRDS(inat_park_obs, file = "data/inat_park_obs_20260123_bkup.rds")


