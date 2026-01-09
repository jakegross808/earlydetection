library(pacnvegetation)
library(tidyverse)
library(readxl)
library(fuzzyjoin)


# 1. Match accepted names -> iNat ----

pacn_bish_crosswalk <- read_csv(file = "data/pacn_bish_crosswalk.csv")
master_pacn_inat <- readr::read_csv("data/master_pacn_hi_inat.csv")

names(pacn_bish_crosswalk)

#inat_crosswalk <- pacn_bish_crosswalk 
  #rename(poh_accepted_name = POH_scientificName,
  #       poh_accepted_status = POH_taxonomicStatus)

# Take Genus and Family from accepted scientific names and make it a separate 
# record in Accepted_scientificName - this way it will not show as new observation
# from iNat 

pacn_spp_full_scientificName_old <- pacn_bish_crosswalk |>
  select(pacn_observation = bish_name) |>
  mutate(pacn_observation = str_replace_all(pacn_observation, "\\b\\w+\\.\\s*", ""))

pacn_spp_full_scientificName <- pacn_bish_crosswalk |>
  select(pacn_observation = bish_scientificName) |>
  mutate(pacn_observation = str_replace_all(pacn_observation, "\\b\\w+\\.\\s*", ""))

pacn_spp_genus_and_species <- pacn_bish_crosswalk |>
  mutate(bish_specificEpithet2 = case_when(bish_specificEpithet == "NULL" ~ "", .default = bish_specificEpithet)) |>
  mutate(genus_and_species = paste(bish_genus, bish_specificEpithet2, sep = " ")) |>
  select(pacn_observation = genus_and_species)

pacn_spp_genera <- pacn_bish_crosswalk |>
  select(pacn_observation = bish_genus)

pacn_spp_families <- pacn_bish_crosswalk |>
  select(pacn_observation = bish_family)

pacn_spp_orders <- pacn_bish_crosswalk |>
  select(pacn_observation = bish_order)

pacn_spp_classes <- pacn_bish_crosswalk |>
  select(pacn_observation = bish_class)

pacn_observations <- bind_rows(pacn_spp_full_scientificName_old,
                               pacn_spp_full_scientificName, 
                       pacn_spp_genus_and_species, 
                       pacn_spp_genera, 
                       pacn_spp_families, 
                       pacn_spp_orders, 
                       pacn_spp_classes) |>
  distinct() 

view(pacn_observations$pacn_observation)

# plants from iNat that do not match pacn_observations may be new introductions 
# to park 

inat_pacn_new_plants <- master_pacn_inat |>
  filter(!scientific_name %in% pacn_observations$pacn_observation)


new_plants <- anti_join(x = spp_INAT_dist, y = pacn_bish_accepted, by = join_by(scientific_name == scientificName))

new_plants2 <- new_plants |>
  separate_wider_delim(scientific_name, " ", names = c('inat_genus', 'inat_species', 'extra'), too_many = "merge", too_few = "align_start")



no_match_inat <- anti_join(x = Accepted, y = spp_INAT_dist, by = join_by(Accepted_scientificName == scientific_name))

match_inat <- inner_join(x = Accepted, y = spp_INAT_dist, by = join_by(Accepted_scientificName == scientific_name))















# OLD delete Match accepted names -> iNat ----

# Take Genus and Family from accepted scientific names and make it a separate 
# record in Accepted_scientificName - this way it will not show as new observation
# from iNat 

# Drop most of 
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


# plants from iNat that do not match pacn_bish_accepted may be new introductions 
# to park 
new_plants <- anti_join(x = spp_INAT_dist, y = pacn_bish_accepted, by = join_by(scientific_name == scientificName))

new_plants2 <- new_plants |>
  separate_wider_delim(scientific_name, " ", names = c('inat_genus', 'inat_species', 'extra'), too_many = "merge", too_few = "align_start")



no_match_inat <- anti_join(x = Accepted, y = spp_INAT_dist, by = join_by(Accepted_scientificName == scientific_name))

match_inat <- inner_join(x = Accepted, y = spp_INAT_dist, by = join_by(Accepted_scientificName == scientific_name))








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


