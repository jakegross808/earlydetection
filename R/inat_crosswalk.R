library(pacnvegetation)
library(tidyverse)
library(readxl)
library(fuzzyjoin)


# 1. Load Tables ----

pacn_bish_crosswalk <- read_csv(file = "data/pacn_bish_crosswalk.csv")

master_pacn_inat <- readr::read_csv("data/master_pacn_hi_inat.csv") |>
  filter(!parkName %in% c("AMME", "WAPA"))

# 2. Select Fields ----
names(pacn_bish_crosswalk)
names(master_pacn_inat)

inat_obs <- master_pacn_inat |>
  select(created_at, observed_on_string, 
         taxon.name, taxon.rank, taxon.preferred_common_name, 
         uri, location, geoprivacy, 
         positional_accuracy_meters = positional_accuracy, 
         user.login, park, parkName)

# 3. PACN iNaturalist obs ----
veg_crew_obs <- inat_obs |>
  filter(user.login == "pacn_plants") 
table(veg_crew_obs$parkName)

# 4. Known PACN plant names ----
# (Per Park)

## ...Name match issues ----

#***These species show up in iNat as mismatches, but are known to occur:
#*** need addressed in PACN database and/or BISH***

pacn_bish_crosswalk_fix_issues <- pacn_bish_crosswalk |>
  mutate(bish_scientificName = 
           case_match(
             bish_scientificName,
             "Psydrax odorata" ~ "Psydrax odoratus", # in Imada et al. 2025 as Psydrax odorata (G.Forst.) A.C.Sm. & S.P.Darwin
             "Schizachyrium condensatum" ~ "Schizachyrium microstachyum", # in Imada 2025, but not in BISH database export??
             "Phaius tankervilleae" ~ "Calanthe tankervilleae", # Old name according to Imada 2025 - current disposition = "Phaius tankervilleae"
             .default = bish_scientificName) 
  )

# last used PACN scientific name before BISH 2025 update
pacn_spp_full_scientificName_old <- pacn_bish_crosswalk_fix_issues |>
  select(pacn_observation = bish_name, Park) |>
  mutate(pacn_observation = str_replace_all(pacn_observation, "\\b\\w+\\.\\s*", ""))

# accepted scientific name (full name - including var., subsp., etc)
pacn_spp_full_scientificName <- pacn_bish_crosswalk_fix_issues |>
  select(pacn_observation = bish_scientificName, Park) |>
  mutate(pacn_observation = str_replace_all(pacn_observation, "\\b\\w+\\.\\s*", ""))

# accepted scientific name (just genus and species)
pacn_spp_genus_and_species <- pacn_bish_crosswalk_fix_issues |>
  mutate(bish_specificEpithet2 = case_when(bish_specificEpithet == "NULL" ~ "", .default = bish_specificEpithet)) |>
  mutate(genus_and_species = paste(bish_genus, bish_specificEpithet2, sep = " ")) |>
  select(pacn_observation = genus_and_species, Park)

# accepted genus
pacn_spp_genera <- pacn_bish_crosswalk_fix_issues |>
  select(pacn_observation = bish_genus, Park)

# accepted family
pacn_spp_families <- pacn_bish_crosswalk_fix_issues |>
  select(pacn_observation = bish_family, Park)

# accepted order
pacn_spp_orders <- pacn_bish_crosswalk_fix_issues |>
  select(pacn_observation = bish_order, Park)

# accepted class
pacn_spp_classes <- pacn_bish_crosswalk_fix_issues |>
  select(pacn_observation = bish_class, Park)

# combine all names
pacn_observations <- bind_rows(pacn_spp_full_scientificName_old,
                               pacn_spp_full_scientificName, 
                       pacn_spp_genus_and_species, 
                       pacn_spp_genera, 
                       pacn_spp_families, 
                       pacn_spp_orders, 
                       pacn_spp_classes) |>
  distinct() 

# 5. Drop some iNat obs ----


# Determine what records to limit on iNaturalist (for example: drop observations of
# Magnoliopsida (Dicots), and just keep family, genus, species and and finer level stuff.)
inat_obs_all <- inat_obs |>
  mutate(row_id = row_number()) 

inat_obs_filtered <- inat_obs_row |>
  filter(!str_detect(taxon.name, "Argyroxiphium")) |> # Drop silversword, too many weird hybrid names
  filter(taxon.rank %in% c("family", "genus", "species", "subspecies", 
                           "variety", "hybrid", "genushybrid", "form"))

inat_pacn_new_plants_all <- inat_obs_all |>
  anti_join(pacn_observations, by = c("taxon.name" = "pacn_observation", "parkName" = "Park"))

inat_pacn_new_plants_filtered <- inat_obs_filtered |>
  anti_join(pacn_observations, by = c("taxon.name" = "pacn_observation", "parkName" = "Park"))

dropped <- inat_pacn_new_plants_all |>
  anti_join(inat_pacn_new_plants_filtered, by = "row_id")


undocumented_pacn_obs <- inat_pacn_new_plants_filtered
nrow(undocumented_pacn_obs) # 20260114 = 612

undocumented_pacn_plants <- undocumented_pacn_obs |>
  group_by(taxon.name, parkName) |>
  summarise(n = n())
nrow(undocumented_pacn_plants) #20260114 =251

write_csv(undocumented_pacn_obs, "data/undocumented_pacn_obs.csv")
write_csv(undocumented_pacn_plants, "data/undocumented_pacn_plants.csv")

#download that dataset
#write.csv(master_pacn_inat, "C:/Users/sbierker/Desktop/R work/Independent Project/master_pacn_inat.csv", row.names=F)
#write.csv(new_pacn_inat, "C:/Users/sbierker/Desktop/R work/Independent Project/new_pacn_inat.csv", row.names=T)


