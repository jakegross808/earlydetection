library(rinat)
library(dplyr)

park <- c('American Memorial Park','Haleakala National Park', 'Hawaii Volcanoes National Park','Kalaupapa National Historical Park', 'Kaloko-Honokohau National Historical Park','War in the Pacific National Historical Park')
parkName <- c('AMME','HALE','HAVO','KALA','KAHO','WAPA')
placeID <- c(97397,56788,7222,95256,95255,95342)
dfpk <- data.frame(park,parkName,placeID)
dfpk2 <- dfpk[1:2,]
#make a function that loops through parks and years grabbing observations

# get test obs to get correct column names
test_obs <- rinat::get_inat_obs(year = 2023, place_id=56788, maxresults = 3, quality='research') %>%
  select(-sound_url, geoprivacy)
# clear all rows so just have empty dataframe ready to fill
inat_obs_yearly <- test_obs[FALSE,]
inat_obs_park <- test_obs[FALSE,]

iNatObs <- function(x){
  park_code <- x["parkName"]
  full_name <- x["park"]
  placeID <- x["placeID"]
  print(full_name)

  # message
  print(paste("inat_obs_park (begining) records =", nrow(inat_obs_park)))

  # set which years to pull observations
  #years <- c(1990:lubridate::year(Sys.Date()))
  year_test <- c(1990, 2020)
  y <- lapply(year_test, inat_year)

  for (year in years) {
    print(year)
    year_obs <- rinat::get_inat_obs(year = year, place_id=placeID, maxresults = 10000) %>%
      select(-sound_url, -geoprivacy)
    if (nrow(year_obs) == 10000) {stop("max observations reached in a year,
                                       year needs to be further divided by months")}
    inat_obs_yearly <- bind_rows(inat_obs_yearly, year_obs)

    # message
    print(paste("inat_obs_yearly records =", nrow(inat_obs_yearly)))

  }

  inat_obs_park <- bind_rows(inat_obs_park, inat_obs_yearly)
  print(paste("inat_obs_park (ending) records =", nrow(inat_obs_park)))
  inat_obs_park
}

all_pacn_inat_obs <- apply(X = dfpk, MARGIN = 1, FUN = iNatObs)





date

apply(X = dfpk2, MARGIN = 1, FUN = iNatObs)

#get iNat observations from the function based on the place ID for your park
iNatObsSP <- iNatObs(placeID)
#adding a column to that dataset that is the park's code
iNatObsSP$park <- parkName

#separate the values of the date
iNatObsSP$dates <- as.Date(iNatObsSP$observed_on)
iNatObsSP <- transform(iNatObsSP, date = format(dates, "%d"),
                       month = format(dates, "%m"), year = format(dates, "%Y"))
