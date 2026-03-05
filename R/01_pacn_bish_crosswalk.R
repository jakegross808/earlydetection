library(pacnvegetation)
library(tidyverse)
library(readxl)
library(fuzzyjoin)

# 1. Load data ----

# read the 3 species lists 

# A. spp_bish = Bishop Museum's Plants of Hawaii (POH) Database export
#    database export produced by Bishop Museum's Timothy Gallaher <timothy.gallaher@bishopmuseum.org>

# B. spp_pacn = PACN vegetation species list (PACN_veg_species_list.accdb)
#    created using R/PACN_spp_DB_list.R

# C. spp_inat = iNaturalist plant observation list for Hawaii parks
#    created using R/inat_obs_loop_final.R

## A. BISH list----

spp_bish <- readxl::read_excel("data/BISH_Taxon_Table_12_18_2025.xlsx")

### prep bish list ----
bish_all_taxon_statuses <- spp_bish |>
  select(bish_id  = taxonID,
         bish_name = scientificName, 
         bish_name_auth = scientificNameAuthorship, 
         bish_name_status = taxonomicStatus,
         bish_accepted_name_id = acceptedNameUsageID)

bish_accepted_only <- spp_bish |>
  dplyr::filter(taxonomicStatus == "Accepted") 

bish_crosswalk <- bish_all_taxon_statuses |>
  dplyr::left_join(bish_accepted_only, 
                   by = join_by(bish_accepted_name_id == taxonID)) |>
  select(-acceptedNameUsageID) |> # drop as it is now repeat of "bish_accepted_name_id"
  rename(bish_accepted_status = taxonomicStatus) # this column should all be "Accepted"

# put prefix "bish" in front of all remaining column names:  
prefix <- "bish_"
# Use rename_with to apply prefix to all column names
bish <- bish_crosswalk %>%
  rename_with(
    ~ ifelse(
      startsWith(., prefix), # Check if name starts with prefix
      .,                     # If yes, keep current name (the dot . refers to the current column name)
      paste0(prefix, .)       # If no, add the prefix
    )
  )

### create synonyms columns ----
synonyms_draft <- bish_accepted_only |> 
  select(taxonID, scientificName, scientificNameAuthorship) |>
  right_join(spp_bish, by = join_by(taxonID == acceptedNameUsageID)) |>
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

# if genus already listed in synonym, then abbreviate with first letter 
# for example: "Panicum maximum var. coloratum, P. maximum var. glaucum"
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

# apply function above to synonyms column 
synonyms_draft$syn_abbr <- vapply(synonyms_draft$synonyms, abbrev_repeat_genus, FUN.VALUE = character(1))

# make column with full list of synonym alphabetical
synonyms_alphabetical <- synonyms_draft %>%
  mutate(
    all_synonyms_alphabetical = str_split(synonyms, ",") |>           # list-column
      purrr::map(~ sort(str_trim(.x))) |>                 # sort inside each list
      purrr::map_chr(~ str_c(.x, collapse = ", "))         # back to character
  )

# truncate column with abbreviated list of synonym to 250 characters of less
# this may be needed for ArcGIS field maps...
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

# apply the function to truncate synonyms abbreviation column to 250 characters:
synonyms_alphabetical$syn_abbr_250 <- truncate_no_split(synonyms_alphabetical$syn_abbr, 250)
synonyms_alphabetical$new_nchar <- nchar(synonyms_alphabetical$syn_abbr_250)

# final synonyms table
synonyms <- synonyms_alphabetical |>
  select(bish_accepted_name_id = taxonID, 
         bish_scientificName = scientificName.x, 
         bish_all_synonyms_alphabetical = all_synonyms_alphabetical, 
         bish_synonyms_abbreviated = syn_abbr_250)
  

## B. PACN list ----
# use previous PACN species list provided in data folder (created by R/PACN_spp_DB_list.R)
# read_spp_db(veg_species_db_full_path) 
# note that there will be repeat records for every park the species is present
spp_pacn <- read_csv("data/PACN_spp_list.csv")

# 2. Crosswalks ----

# Changes to spp_PACN:
pacn <- spp_pacn |> 
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
  mutate(pacn_auth = case_when(is.na(Authority) ~ "",
                              .default = Authority)) |>
  # Remove records with no species name
  filter(!is.na(std_name)) |>
  filter(!stringr::str_detect(std_name, "Unknown ")) |>
  filter(!stringr::str_detect(std_name, "Alien ")) |>
  rename(pacn_name = std_name)
  
## fuzzy join ----
pacn_bish_fuzzyjoin <- fuzzyjoin::stringdist_left_join(x = pacn, y = bish, 
                                                by = c("pacn_name" = "bish_name"), 
                                                max_dist = 5, distance_col = "distance"
                                                )


### match YES ----
fuzzy_matches <- pacn_bish_fuzzyjoin |>
  filter(distance == 0) 

# matches with duplicates
fuzzy_matches_dup_list <- fuzzy_matches |>
  count(pacn_name, Park) |> 
  filter(n > 1) |>
  distinct(pacn_name, n)
  # records with >1 typically have same scientific name but different authority

### ...no duplicates----
# GOOD TO GO

fuzzy_matches_dups_no <- fuzzy_matches |>
  filter(!pacn_name %in% fuzzy_matches_dup_list$pacn_name) |>
  mutate(auth_stringdist = stringdist::stringdist(pacn_auth, bish_name_auth, method = "lv")) |>
  mutate(name_match_category = "sp match and no duplicates") |>
  mutate(pacn_new_bish_db_match = "Yes")

### ...duplicates ----

fuzzy_matches_dups <- fuzzy_matches |>
  filter(pacn_name %in% fuzzy_matches_dup_list$pacn_name) |>
  mutate(auth_stringdist =stringdist::stringdist(pacn_auth, bish_name_auth, method = "lv")) 

fuzzy_matches_dups_no_pacn_auth <- fuzzy_matches_dups |>
  select(pacn_name, pacn_auth, bish_name, bish_name_auth, bish_accepted_name_id, bish_scientificName) |>
  filter(pacn_auth == "")

# some pacn records do not have an authority and there are duplicates records
# in bish (same species name but different authority)
# this could result in incorrect assignment if wrong authority is selected 
# which could happen when wrong authority is shorter than correct authority since
# the pacn authority is blank (i.e. shorter sting distance). 

# get pacn records missing authority matched with 2 or more distinct bish names
fuzzy_authority_errors <- fuzzy_matches_dups_no_pacn_auth |>
  group_by(pacn_name, pacn_auth) |>
  summarise(n = n_distinct(bish_scientificName)) |>
  filter(n > 1)

if (nrow(fuzzy_authority_errors) == 0) {
  print("passed check for species authority issues")
} else {
  print(paste(nrow(fuzzy_authority_errors), "species records with authority issues detected"))
}

# Species records with authority issues will appear here, if any.
review_pacn_species_authority_issues <- fuzzy_matches_dups_no_pacn_auth |>
  filter(pacn_name %in% fuzzy_authority_errors$pacn_name) |>
  distinct(pacn_name, bish_name_auth, bish_scientificName)
# Do not continue until species authority issues are fixed...

# check these records for high fuzzy matching distance between authority: 
fuzzy_matches_author <- fuzzy_matches_dups |>
  group_by(pacn_name) |>
  slice_min(auth_stringdist) |> # chooses closest matching Author/Authority
  mutate(name_match_category = "sp match and best auth match") |>
  mutate(pacn_new_bish_db_match = "Yes")

## match NO ----
fuzzy_no_matches <- pacn_bish_fuzzyjoin |>
  filter(is.na(distance)) |>
  distinct () |>
  mutate(name_match_category = "no match in BISH database") |>
  mutate(pacn_new_bish_db_match = "No")

## match UNSURE ----
pacn_bish_crosswalk_partial <- bind_rows(fuzzy_matches_dups_no, fuzzy_matches_author, fuzzy_no_matches) 
  
fuzzy_unsure_matches <- pacn_bish_fuzzyjoin |>
  filter(distance > 0) |>
  filter(!pacn_name %in% pacn_bish_crosswalk_partial$pacn_name) |>
  group_by(pacn_name) |>
  slice_min(distance) |>
  distinct (pacn_name, pacn_auth, .keep_all = TRUE) |>
  mutate(name_match_category = "best guess from fuzzy join - needs checked") |>
  mutate(pacn_new_bish_db_match = "No")

pacn_bish_crosswalk <- bind_rows(pacn_bish_crosswalk_partial, fuzzy_unsure_matches) |>
  select(-bish_namePublishedInYear,
         -distance,
         -auth_stringdist,
         -name_match_category)

# check if all pacn veg species records are accounted for:
anti_joing_check1 <- anti_join(pacn, pacn_bish_crosswalk)
anti_joing_check2 <- anti_join(pacn_bish_crosswalk, pacn)
anti_joing_check3 <- anti_join(pacn_bish_crosswalk, pacn, by = join_by(pacn_name))
anti_joing_check4 <- anti_join(pacn, pacn_bish_crosswalk, by = join_by(pacn_name))

# unmatched totals: 
nrow(fuzzy_unsure_matches)  
nrow(fuzzy_no_matches)

# total names changed (on 12/31/2025: 259 names changed, 534 records changed)
names_changed <- pacn_bish_crosswalk |>
  filter(bish_name != bish_scientificName) 
# records changed:
nrow(names_changed)
# names changed:
nrow(distinct(.data = names_changed, bish_scientificName))


## finalize pacn-bish crosswalk----
pacn_bish_crosswalk_with_synonyms <- pacn_bish_crosswalk |>
  left_join(y = synonyms, by = join_by(bish_accepted_name_id)) 

write.csv(pacn_bish_crosswalk_with_synonyms, "data/pacn_bish_crosswalk_with_synonyms.csv", row.names = FALSE)
write.csv(pacn_bish_crosswalk, "data/pacn_bish_crosswalk.csv", row.names = FALSE)



