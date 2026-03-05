# Steps to get an updated PACN species list (which itself uses NPSpecies):

# Local Path to Veg Spp database 
# skip step and use previously downloaded dataset if no access to Veg_species_db
veg_species_db_folder <-"C:/Users/JJGross/LocalDocuments/Databases_copied_local/Veg_species_db"
# If only one database in folder, this will grab full path:
veg_species_db_full_path <- list.files(veg_species_db_folder,full.names = TRUE)
veg_species_db_full_path
raw_spp_data_orig <- pacnvegetation::read_spp_db(veg_species_db_full_path) #grab original copy for comparisons
readr::write_csv(raw_spp_data_orig, "data/PACN_spp_list.csv")
