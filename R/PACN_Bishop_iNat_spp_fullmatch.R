library(pacnvegetation)
library(tidyverse)
library(readxl)

library(fuzzyjoin)

#library(powerjoin)
#library(stringdist)

# 1. Load data ----
# Collect the 3 species lists 
# 1. POH = Bishop Museum's plants of Hawaii list, 
# 2. PACN = PACN vegetation species list
# 3. iNat = iNaturalist plant observation list for Hawaii parks

## POH lists ----
# lists provided by Bishop Museum's Timothy Gallaher <timothy.gallaher@bishopmuseum.org>

spp_POH_excel <- readxl::read_excel("data/POH_Names_Table.xlsx")
POH1 <- spp_POH_excel |>
  select(POH_name = scientificName, 
         POH_Auth = scientificNameAuthorship, 
         POH_Accepted_Name = Accepted_scientificName) 

#spp_POH_excel2 <- readxl::read_excel("data/POH Nomenclator 20251119.xlsx")
#POH2 <- spp_POH_excel2 |>
#  select(POH_name = scientificName, 
#         POH_Auth = scientificNameAuthorship, 
#         POH_Accepted_Name = `Accepted Name`, 
#         POH_Accepted_Name_Auth = `Accepted Name Author`)
  

## PACN list ----
# use previous PACN species list provided in data folder (created by R/PACN_spp_DB_list.R)
# note there is similar function in pacnvegetation package:
# read_spp_db(veg_species_db_full_path) #has repeats for every park
spp_PACN <- read_csv("data/PACN_spp_list.csv") 


# generate all possible combinations with dplyr
# test <- dplyr::full_join(POH, PACN, by = character()) %>%
  # calculate string editting distance and use to filter
  # dplyr::filter(stringdist::stringdist(scientificName, Scientific_name) < 3)







  

## iNat list ----
spp_INAT <- read_csv("data/master_pacn_hi_inat.csv") 

spp_INAT_dist <- spp_INAT %>%
  distinct(scientific_name, parkabbr, .keep_all = TRUE)


# 2. Match spp PACN -> BISH ----

# Changes to spp_PACN:
PACN <- spp_PACN |> 
  # Bishop does not have plant species list outside Hawaii so drop AMME, WAPA, NPSA
  filter(!Park %in% c("AMME", "WAPA", "NPSA")) |>
  distinct(Scientific_name, .keep_all = TRUE) |>
  filter(Code != "SNAG") |>
  # Drop the 'sp.' and 'spp.' in 'Genus spp.' names because POH just uses Genus name only
  mutate(std_name = case_when(Species == "spp." ~ Genus,
                              .default = Scientific_name)) |>
  # for subspecies, use "subsp." instead of "ssp."
  mutate(std_name = str_replace(std_name, " ssp. ", " subsp. ")) |>
  mutate(std_name = str_replace(std_name, " X ", " × ")) |>
  mutate(std_name = str_squish(std_name)) |>
  # Change NAs to blanks in Authority and rename to PACN_Authority
  mutate(PACN_Auth = case_when(is.na(Authority) ~ "",
                              .default = Authority)) |>
  # Remove records with no species name
  filter(!is.na(std_name)) |>
  filter(!stringr::str_detect(std_name, "Unknown ")) |>
  filter(!stringr::str_detect(std_name, "Alien ")) 
  


# Work with subset of columns 
s_PACN <- PACN |>
  select(PACN_name = std_name, PACN_Auth) 

POH_fuzzyjoin <- fuzzyjoin::stringdist_left_join(x = s_PACN, y = POH1, 
                                                by = c("PACN_name" = "POH_name"), 
                                                max_dist = 5, distance_col = "distance"
                                                )


## Match YES ----
POH1_matches <- POH_fuzzyjoin |>
  filter(distance == 0) 

POH1_matches_dup_list <- POH1_matches |>
  count(PACN_name) |> 
  filter(n > 1)

### no dups----
# ie GOOD TO GO

POH1_matches_dups_no <- POH1_matches |>
  filter(!PACN_name %in% POH1_matches_dup_list$PACN_name) |>
  mutate(auth_stringdist =stringdist::stringdist(PACN_Auth, POH_Auth, method = "lv")) |>
  mutate(name_match_category = "sp match and no duplicates")

### dups ----

POH1_matches_dups <- POH1_matches |>
  filter(PACN_name %in% POH1_matches_dup_list$PACN_name) |>
  mutate(auth_stringdist =stringdist::stringdist(PACN_Auth, POH_Auth, method = "lv")) |>
  group_by(PACN_name) |>
  slice_min(auth_stringdist) |> # chooses closes matching Author/Authority
  distinct() |> # removes exact copies
  mutate(name_match_category = "sp match and best auth match")

POH_PACN_cw <- bind_rows(POH1_matches_dups_no, POH1_matches_dups)




## Match NO ----
POH1_no_matches <- POH_fuzzyjoin |>
  filter(is.na(distance)) |>
  distinct () |>
  mutate(name_match_category = "no match in BISH database")
  
   

## Match UNSURE ----
POH1_unsure_matches <- POH_fuzzyjoin |>
  filter(distance > 0) |>
  filter(!PACN_name %in% POH_PACN_cw$PACN_name) |>
  group_by(PACN_name) |>
  slice_min(distance) |>
  distinct (PACN_name, PACN_Auth, .keep_all = TRUE) |>
  mutate(name_match_category = "best guess from fuzzy join - needs checked")

## Compute totals  
nrow(POH_PACN_cw)  
nrow(POH1_no_matches)
nrow(POH1_unsure_matches)
nrow(POH_PACN_cw) + nrow(POH1_no_matches) + nrow(POH1_unsure_matches)

POH_PACN_cw_final <- bind_rows(POH_PACN_cw, POH1_no_matches, POH1_unsure_matches)

POH_PACN_cw_final_dups <- POH_PACN_cw_final |>
  group_by(PACN_name) |>
  count()




## export sp_to_review ----

sp_to_review <- POH_PACN_cw_final |>
  filter(name_match_category == "no match in BISH database" |
           name_match_category == "best guess from fuzzy join - needs checked")

readr::write_excel_csv(sp_to_review, file = "sp_to_review.csv")








#.....-----
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




# 3. Match accepted names -> iNat ----

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


