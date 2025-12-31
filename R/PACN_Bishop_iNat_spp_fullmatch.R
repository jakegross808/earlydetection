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


spp_POH_excel <- readxl::read_excel("data/BISH_Taxon_Table_12_18_2025.xlsx")

# # checks to compare previous BISH spreadsheet
# spp_POH_excel_old <- readxl::read_excel("data/POH_Names_Table.xlsx")
# identical(x = spp_POH_excel_old, spp_POH_excel)
# # 1. Columns in new but not in "old"
# cols_in_new_only <- setdiff(names(spp_POH_excel), names(spp_POH_excel_old))
# print(cols_in_new_only)
# # 2. Columns in "old" but not in new
# cols_in_old_only <- setdiff(names(spp_POH_excel_old), names(spp_POH_excel))
# print(cols_in_old_only)
# POH_old <- spp_POH_excel_old |>
#   select(POH_name = scientificName, 
#          POH_Auth = scientificNameAuthorship, 
#          POH_Accepted_Name_ID = Accepted_scientificName) 

levels(as.factor(spp_POH_excel$taxonomicStatus))

POH_all_taxon_statuses <- spp_POH_excel |>
  select(POH_ID = taxonID,
         POH_name = scientificName, 
         POH_Auth = scientificNameAuthorship, 
         POH_taxon_status = taxonomicStatus,
         POH_Accepted_Name_ID = acceptedNameUsageID)

POH_accepted_taxon_only <- spp_POH_excel |>
  dplyr::filter(taxonomicStatus == "Accepted") 

POH_crosswalk <- POH_all_taxon_statuses |>
  dplyr::left_join(POH_accepted_taxon_only, 
                   by = join_by(POH_Accepted_Name_ID == taxonID))

# put prefix in front of all column names:  
prefix <- "POH_"
# Use rename_with to apply a function to column names
POH1 <- POH_crosswalk %>%
  rename_with(
    ~ ifelse(
      startsWith(., prefix), # Check if name starts with prefix
      .,                     # If yes, keep current name (the dot . refers to the current column name)
      paste0(prefix, .)       # If no, add the prefix
    )
  )

### Synonyms ----
synonyms_draft <- POH_accepted_taxon_only |> 
  select(taxonID, scientificName, scientificNameAuthorship) |>
  right_join(spp_POH_excel, by = join_by(taxonID == acceptedNameUsageID)) |>
  mutate(synonym = scientificName.y) |>
  # if synonym same as scientific name then make blank
  # this could occur if a different authority is listed but same species
  mutate(synonym = case_when(scientificName.x == synonym ~ "",
                             .default = synonym)) |>
  mutate(species_name_only = word(scientificName.y, 1, 2)) |>
  dplyr::add_count(species_name_only, name = "synonym_count") |>
  distinct(taxonID, scientificName.x, synonym, synonym_count) |>
  arrange(-synonym_count) |>
  group_by(taxonID, scientificName.x) |>
  # ignore NA and blanks in synonyms
  summarise(synonyms = paste(synonym[synonym != "" & !is.na(synonym)], collapse = ", "), .groups = "drop", ) 

abbrev_repeat_genus <- function(s) {
  parts <- strsplit(s, ",")[[1]]
  parts <- trimws(parts)
  
  seen <- character(0)
  
  parts2 <- vapply(parts, function(p) {
    # split into first word + rest
    w <- strsplit(p, "\\s+", perl = TRUE)[[1]]
    first <- w[1]
    rest  <- if (length(w) > 1) paste(w[-1], collapse = " ") else ""
    
    # only abbreviate if word seen before
    if (first %in% seen && nchar(first) > 1) {
      first_out <- paste0(substr(first, 1, 1), ".")
    } else {
      first_out <- first
      seen <<- c(seen, first)
    }
    
    paste(first_out, rest)
  }, FUN.VALUE = character(1))
  
  paste(parts2, collapse = ", ")
}

synonyms_draft$syn_abbr <- vapply(synonyms_draft$synonyms, abbrev_repeat_genus, FUN.VALUE = character(1))

synonyms_alphabetical <- synonyms_draft %>%
  mutate(
    all_synonyms_alphabetical = str_split(synonyms, ",") |>           # list-column
      purrr::map(~ sort(str_trim(.x))) |>                 # sort inside each list
      purrr::map_chr(~ str_c(.x, collapse = ", "))         # back to character
  )

truncate_no_split <- function(x, max_chars = 250) {
  sapply(x, function(txt) {
    # NA or short strings stay as-is
    if (is.na(txt) || nchar(txt) <= max_chars) return(txt)
    
    # Take first max_chars characters
    cut <- substr(txt, 1, max_chars)
    
    # Find last space in that chunk
    last_space <- max(gregexpr(" ", cut)[[1]])
    
    # If no space, just return the chunk; otherwise up to last space
    if (last_space == -1) {
      cut
    } else {
      substr(cut, 1, last_space - 1)
    }
  }, USE.NAMES = FALSE)
}

# truncate the synonyms abbreviation column to 250 characters:
synonyms_alphabetical$syn_abbr_250 <- truncate_no_split(synonyms_alphabetical$syn_abbr, 250)
synonyms_alphabetical$new_nchar <- nchar(synonyms_alphabetical$syn_abbr_250)
synonyms_final <- synonyms_alphabetical |>
  select(POH_Accepted_Name_ID = taxonID, 
         POH_Accepted_Name = scientificName.x, 
         POH_all_synonyms_alphabetical = all_synonyms_alphabetical, 
         POH_syn_abbr_250 = syn_abbr_250)


  

## PACN list ----
# use previous PACN species list provided in data folder (created by R/PACN_spp_DB_list.R)
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
  #distinct(Scientific_name, .keep_all = TRUE) |>
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
  


# Work with subset of columns (optional)
PACN_s <- PACN |> 
  rename(PACN_name = std_name)
  #select(PACN_name, PACN_Auth, Bishop_DB_Match) 

POH_fuzzyjoin <- fuzzyjoin::stringdist_left_join(x = PACN_s, y = POH1, 
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
  #distinct() |> # removes exact copies
  mutate(name_match_category = "sp match and best auth match")

PACN_POH_crosswalk <- bind_rows(POH1_matches_dups_no, POH1_matches_dups) |>
  select(-POH_acceptedNameUsageID,
         -POH_namePublishedInYear,
         -distance,
         -auth_stringdist,
         -name_match_category)

PACN_POH_crosswalk_with_synonyms <- PACN_POH_crosswalk |>
  left_join(y = synonyms_final, by = join_by(POH_Accepted_Name_ID)) |>
  select(-POH_Accepted_Name)

write.csv(PACN_POH_crosswalk_with_synonyms, "data/PACN_POH_crosswalk_with_synonyms.csv")
write.csv(PACN_POH_crosswalk, "data/PACN_POH_crosswalk.csv")


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

sp_to_review2 <- POH_PACN_cw_final |>
  filter(name_match_category == "no match in BISH database" |
           name_match_category == "best guess from fuzzy join - needs checked")

readr::write_excel_csv(sp_to_review, file = "sp_to_review.csv")





# 3. Match accepted names -> iNat ----

# Take Genus and Family from accepted scientific names and make it a separate 
# record in Accepted_scientificName - this way it will not show as new observation
# from iNat 

# Drop most of 
match_bish_select <- POH_PACN_cw_final |>
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


