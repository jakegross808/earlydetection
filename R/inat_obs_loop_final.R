library(rinat)
library(dplyr)

#inat_year function gets used in iNatObs function below
inat_year <- function(x, pid, parkabbr){
  #function that loops through a list of years grabbing iNat observations
  tryCatch( #tryCatch used to prevent hard stop if no observations for a year
    expr = {
      message(paste("getting iNat observations for year", x))
      # taxon_id=211194 is for vascular plants
      obs <- rinat::get_inat_obs(taxon_id=211194, year = x, place_id=pid, maxresults = 10000) %>%
        select(-sound_url, -geoprivacy)
      if (nrow(obs) == 10000) {stop("max observations reached in a year,
                                       year needs to be further divided by months")}
      obs$year <- x
      obs$parkabbr <- parkabbr
      obs

    },
    error = function(e){
      message(paste('---no records for year', x))

      # This is just dummy data used to get columns from iNat:
      # maybe can just change this to NA or Null later, if working.
      null_obs <- rinat::get_inat_obs(year = 2023, place_id=56788, maxresults = 3) %>%
        select(-sound_url, -geoprivacy) %>%
        slice(0)
      null_obs

    },
    warning = function(w){
      message('Caught an warning!')
      print(w)

    },
    finally = {
      #message('pau!')
    }
  )
}

park <- c('American Memorial Park','Haleakala National Park', 'Hawaii Volcanoes National Park','Kalaupapa National Historical Park', 'Kaloko-Honokohau National Historical Park','War in the Pacific National Historical Park')
parkName <- c('AMME','HALE','HAVO','KALA','KAHO','WAPA')
placeID <- c(97397,56788,7222,95256,95255,95342)
dfpk <- data.frame(park,parkName,placeID)
dfpk2 <- dfpk[2,]


iNatObs <- function(x){
  park_code <- x["parkName"]
  full_name <- x["park"]
  placeID <- x["placeID"]
  print(full_name)

  # message
  message(paste("start", full_name))
  year_test <- c(2008:2023)

  y <- lapply(year_test, inat_year, pid = placeID , parkabbr = park_code)

  message(paste("end", full_name))

  y

}

# This section should work in theory (loops through (apply to) each park)
# but the rinat::get_inat_obs() function didn't like it for some reason,
# and kept throwing stop errors

#All_PACN_list <- apply(X = dfpk, FUN = iNatObs)
#All_PACN_obs <- bind_rows(All_PACN_list)
# Gives error: 'No encoding supplied: defaulting to UTF-8.
# Error in if (!x$headers$`content-type` == "text/csv; charset=utf-8") { :
#    argument is of length zero'

# So, instead I just split up each park and for whatever reason that worked
AMME <- dfpk[1,]
HALE <- dfpk[2,]
HAVO <- dfpk[3,]
KALA <- dfpk[4,]
KAHO <- dfpk[5,]
WAPA <- dfpk[6,]


AMME_list <- apply(X = AMME, MARGIN = 1, FUN = iNatObs)
AMME_obs <- bind_rows(AMME_list)

HALE_list <- apply(X = HALE, MARGIN = 1, FUN = iNatObs)
HALE_obs <- bind_rows(HALE_list)

HAVO_list <- apply(X = HAVO, MARGIN = 1, FUN = iNatObs)
HAVO_obs <- bind_rows(HAVO_list)

KALA_list <- apply(X = KALA, MARGIN = 1, FUN = iNatObs)
KALA_obs <- bind_rows(KALA_list)

KAHO_list <- apply(X = KAHO, MARGIN = 1, FUN = iNatObs)
KAHO_obs <- bind_rows(KAHO_list)

WAPA_list <- apply(X = WAPA, MARGIN = 1, FUN = iNatObs)
WAPA_obs <- bind_rows(WAPA_list)

all_pacn_plant_obs <- bind_rows(AMME_obs, HALE_obs, HAVO_obs, KALA_obs, KAHO_obs, WAPA_obs)
readr::write_csv(all_pacn_plant_obs, "master_pacn_inat.csv")

master_pacn_inat <- readr::read_csv("master_pacn_inat.csv")

master_dist <- master_pacn_inat %>%
  distinct(scientific_name, parkabbr, .keep_all = TRUE)
