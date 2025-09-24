library(pacnvegetation)
library(tidyverse)

library(readxl)
#library(fuzzyjoin)
#library(stringdist)

# 1. Load data ----
# Collect the 3 species lists 
# 1. POH = Bishop Museum's plants of Hawaii list, 
# 2. pacn_veg_spp = PACN vegetation species list
# 3. iNat_veg_NPS = iNaturalist plant observation list for Hawaii parks

## BISH list ----
# list provided by Bishop Museum's Timothy Gallaher <timothy.gallaher@bishopmuseum.org>
# bishop_csv_path <- "C:/Users/JJGross/LocalDocuments/Databases_copied_local/BISH_spp/POH_Names_Table.csv"
# bishop <- read_csv(bishop_csv_path)
# write_csv(bishop, "data/POH_Names_Table.csv")

# spp_POH <- read_csv("data/POH_Names_Table.csv")  
# fix encoding issue in csv version
# spp_POH$scientificName <- iconv(spp_POH$scientificName, from = "UTF-8", to = "UTF-8", sub = " ") # Replace invalid chars with a space

# Excel version: 
spp_POH_excel <- readxl::read_excel("data/POH_Names_Table.xlsx")

std_spp_POH_excel <- spp_POH_excel |>
  # Drop NULL from specificEpithet so just Genus can be displayed when needed
  mutate(specificEpithet = str_replace(specificEpithet, "NULL", "")) |>
  # Drop everything after species from scientific name
  mutate(std_name = paste0(genus, " ", specificEpithet)) |>
  # All family names to be matched:
  mutate(std_name = case_when(genus == "NULL" & specificEpithet == "" ~ family,
                   .default = std_name)) |>
  # trim leading and trailing white space and other invisible characters
  mutate(std_name = str_squish(std_name)) |>
  mutate(hybrid = str_detect(scientificName, fixed("× "))) |>
  mutate(std_name = case_when(hybrid == TRUE ~ scientificName,
                              .default = std_name))


## PACN list ----
# use previous PACN species list provided in data folder (created by R/PACN_spp_DB_list.R)
# not there is similar function in pacnvegetation package:
# read_spp_db(veg_species_db_full_path) #has repeats for every park
spp_PACN <- read_csv("data/PACN_spp_list.csv") 

spp_PACN <- spp_PACN |>
  filter(taxonRank != "forma") |>
  filter(Code != "SNAG") |>
  filter(Omit_in_NPSpecies == FALSE)

#--- delete distinct() when script finalized so all species in all parks are used----
#*** delete this when done ****
#spp_PACN <- spp_PACN %>%
#  distinct(Scientific_name, .keep_all = TRUE) 
  

## iNat list ----
spp_INAT <- read_csv("data/master_pacn_hi_inat.csv") 

spp_INAT_dist <- spp_INAT %>%
  distinct(scientific_name, parkabbr, .keep_all = TRUE)


# 2. Match spp PACN -> BISH ----

# Changes to spp_PACN:
std_spp_PACN <- spp_PACN |> 
  # Bishop does not have plant species list outside Hawaii so drop AMME, WAPA, NPSA
  filter(!Park %in% c("AMME", "WAPA", "NPSA")) |>
  # Drop everything past species from scientific name:
  mutate(std_name = paste0(Genus, " ", Species)) |>
  # Drop the 'sp.' and 'spp.' in 'Genus spp.' names because POH just uses Genus name only
  mutate(std_name = case_when(Species == "spp." ~ Genus,
                              .default = std_name)) |>
  # for subspecies, use "subsp." instead of "ssp."
  mutate(std_name = str_replace(std_name, " X ", " × ")) |>
  mutate(std_name = str_squish(std_name))





# All records in spp_PACN$Scientific_name should eventually match spp_POH$scientificName

# anti_join = all rows in x that do not have corresponding values in y



no_match_bish <- anti_join(x = std_spp_PACN, y = std_spp_POH_excel, by = "std_name") |>
  arrange(Taxonomic_Family, Scientific_name)

# Check to see why records in 'no_match_bish' aren't matching, and make notes

#####no_match_bish$std_name <- iconv(no_match_bish$std_name, from = "UTF-8", to = "UTF-8", sub = " ") # Replace invalid chars with a space

readr::write_excel_csv(no_match_bish, file = "no_match_bish_excel.csv")

# Some hybrids not matching correctly because names sometimes have genus sometimes not
# Can try to use fuzzy join on hybrids possibly...
#no_match_fuzzy <- stringdist_anti_join(x = std_spp_PACN, 
#                                 y = std_spp_POH_excel, 
#                                 by = join_by(std_name), 
#                                 max_dist = 2,
#                                 distance_col = "distance")




match_bish <- left_join(x = std_spp_PACN, y = std_spp_POH_excel, by = join_by(std_name == scientificName))
#................... still needs addressed .....................................

# Find repeats within spp_POH$scientificName - need to ensure joins match correct rows since there are repeats:
duplicate_rows <- std_spp_POH_excel %>%
  group_by(scientificName) %>%
  filter(n() > 2) %>%
  ungroup()

#*** need to address Authority/Authorship which is causing multiple rows to be matched in POH ****
no_match_auth <- anti_join(x = std_spp_PACN, y = std_spp_POH_excel, by = join_by(std_name == scientificName, Authority == scientificNameAuthorship))
#...............................................................................




# 2. Match spp PACN(BISH) -> iNat ----

# Take Genus and Family from accepted scientific names and make it a separate 
# record in Accepted_scientificName - this way it will not show as new observation
# from iNat 
length(unique(match_bish$Accepted_scientificName))

names(match_bish)

match_bish_select <- match_bish |>
  select(Species_ID:std_name, std_name_POH = std_name.y, Accepted_scientificName, acceptedNameUsageID, )

pacn_bish_accepted <- match_bish_select |>
  left_join(spp_POH_excel, by = join_by(acceptedNameUsageID == taxonID))

new_family <- pacn_bish_accepted |>
  filter(Taxonomic_Family != family) |>
  select(Species_ID, Taxonomic_Family, std_name, std_name_POH, scientificName, family) |>
  filter(family != "NULL")
  
length(unique(pacn_bish_accepted$family))
length(unique(pacn_bish_accepted$order))

orders <- pacn_bish_accepted |>
  select(order, family, scientificName, vernacularName, Nativeness) |>
  distinct() |>
  arrange(order, family, scientificName)
readr::write_excel_csv(orders, file = "orders.csv")

no_match_inat <- anti_join(x = Accepted, y = spp_INAT_dist, by = join_by(Accepted_scientificName == scientific_name))

match_inat <- inner_join(x = Accepted, y = spp_INAT_dist, by = join_by(Accepted_scientificName == scientific_name))

new_plants <- anti_join(x = spp_INAT_dist, y = Accepted, by = join_by(scientific_name == Accepted_scientificName))






### adding in iNat ####
#import iNat data that was previously downloaded
master_pacn_inat <- read.csv("C:/Users/sbierker/Desktop/R work/Independent Project/master_pacn_inat.csv")
#make data match in same way as the other two times
master_pacn_inat[c('genus', 'species', 'extra')] <- str_split_fixed(master_pacn_inat$Scientific_name, ' ', 3)
master_pacn_inat$Scientific_name <- paste(master_pacn_inat$genus, master_pacn_inat$species, sep=" ")
master_pacn_inat <- master_pacn_inat %>% select(-genus,-species,-extra)
master_pacn_inat$Scientific_name <- gsub("X ", "x ", master_pacn_inat$Scientific_name)
master_pacn_inat <-mutate(master_pacn_inat,Scientific_name=sapply(strsplit(master_pacn_inat$Scientific_name, split=' spp.', fixed=TRUE),function(x) (x[1])))
master_pacn_inat <-mutate(master_pacn_inat,Scientific_name=sapply(strsplit(master_pacn_inat$Scientific_name, split=' x', fixed=TRUE),function(x) (x[1])))
master_pacn_inat <-mutate(master_pacn_inat,Scientific_name=sapply(strsplit(master_pacn_inat$Scientific_name, split=' X', fixed=TRUE),function(x) (x[1])))
master_pacn_inat <- master_pacn_inat[grepl(" ", master_pacn_inat$Scientific_name),] #delete scientific names with only genus - make sure to specifically run!

#make row that says where this data is from
master_pacn_inat$inat <- 'inat'

#make copy of raw_spp_data just a preference honestly
raw_spp_data2 <- raw_spp_data %>%
  select(Accepted_scientificNameCorrect, ParkName)
raw_spp_data2$im <- 'im'

#merge inat data by new names of im species
fullSP <- merge(master_pacn_inat, raw_spp_data2, by.x=c('Scientific_name', 'ParkName'), by.y=c('Accepted_scientificNameCorrect', 'ParkName'), all = T)

#ifelse to get source column
fullSP$source <- ifelse(is.na(fullSP$im)&fullSP$inat=='inat', 'inat',
                        ifelse(is.na(fullSP$inat)&fullSP$im=='im', 'im', 'both'))


#select by source so that only those from inat are left and make a column that indicates if the inat obs is a species that is only found on inat (new)
new_pacn_inat <- fullSP %>%
  subset(select = -c(inat, im))
new_pacn_inat$new <- ifelse(new_pacn_inat$source=='inat', 'TRUE', 'FALSE')
master_pacn_inat <- new_pacn_inat
master_pacn_inat <- filter(master_pacn_inat, source!='im')

#download that dataset
write.csv(master_pacn_inat, "C:/Users/sbierker/Desktop/R work/Independent Project/master_pacn_inat.csv", row.names=F)

#another species list with just species only found on inat
new_pacn_inat <- filter(new_pacn_inat, source=='inat')
write.csv(new_pacn_inat, "C:/Users/sbierker/Desktop/R work/Independent Project/new_pacn_inat.csv", row.names=F)



#### extra just to check things ####
#dataset with only distinct species, not all observations
new_pacn_dist <- new_pacn_inat %>%
  distinct(Scientific_name, ParkName, .keep_all = TRUE) %>%
  select(Scientific_name,ParkName, new)
new_pacn_dist$new='TRUE'


write.csv(new_pacn_inat, "C:/Users/sbierker/Desktop/R work/Independent Project/new_pacn_inat.csv", row.names=T)


