pacn_bish_crosswalk_with_synonyms2 <- read_csv("data/pacn_bish_crosswalk_with_synonyms.csv")
pacn_bish_crosswalk2 <- read_csv("data/pacn_bish_crosswalk.csv")

# total names changed (on 12/31/2025: 259 names changed, 534 records changed)
names_changed <- pacn_bish_crosswalk2 |>
  filter(bish_name != bish_scientificName) 
# records changed:
nrow(names_changed)
# names changed:
nrow(distinct(.data = names_changed, bish_scientificName))


names_changed_All <- pacn_bish_crosswalk_with_synonyms2 |>
  filter(bish_name != bish_scientificName.x) |>
  select(Park,
         Park_common_name,
         old_order = Taxonomic_Order,
         old_family = Taxonomic_Family,
         old_genus = Genus,
         old_scientific_name = bish_name,
         old_authority = bish_name_auth,
         new_scientific_name = bish_scientificName.x,
         new_authority = bish_scientificNameAuthorship,
         new_genus = bish_genus, 
         new_family = bish_family,
         new_order = bish_order,
         bishop_vernacular_name = bish_vernacularName,
         bishop_synonyms = bish_synonyms_abbreviated
         )

write_excel_csv(names_changed_All, "data/new_plant_names.csv")
write.csv(names_changed_All, "data/new_plant_names.csv", row.names = FALSE)


names_changed_HAVO <- names_changed_All |>
  filter(Park == "HAVO")

names_changed_KAHO <- names_changed_All |>
  filter(Park == "KAHO")

