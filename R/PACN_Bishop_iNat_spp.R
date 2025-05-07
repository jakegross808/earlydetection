library(pacnvegetation)
library(tidyverse)

# Steps to get an updated PACN species list (which itself uses NPSpecies):

# Local Path to Veg Spp database 
# skip step and use previously downloaded dataset if no access to Veg_species_db
veg_species_db_folder <-"C:/Users/JJGross/Documents/Databases_copied_local/Veg_species_db"
# If only one database in folder, this will grab full path:
veg_species_db_full_path <- list.files(veg_species_db_folder,full.names = TRUE)
veg_species_db_full_path
raw_spp_data_orig <- pacnvegetation::read_spp_db(veg_species_db_full_path) #grab original copy for comparisons
readr::write_csv(raw_spp_data_orig, "data/PACN_spp_list.csv")

# Otherwise use previous test PACN species list provided in data folder
pacn_spp_list <- read_csv("data/PACN_spp_list.csv")

raw_spp_data <- read_spp_db(veg_species_db_full_path) #has repeats for every park
raw_spp_data <- raw_spp_data %>% rename_at('Park', ~'ParkName') #make names match with future dfs

#delete var and spp on list to make names match


#make scientific names match and delete rows with alien and unknowns in them
raw_spp_data$Scientific_name <- gsub("X ", "x ", raw_spp_data$Scientific_name)
raw_spp_data <- raw_spp_data[!grepl("Unknown", raw_spp_data$Scientific_name),]
raw_spp_data <- raw_spp_data[- grepl("Alien", raw_spp_data$Scientific_name),]
raw_spp_data <- subset(raw_spp_data, !is.na(Scientific_name))

#split scientific name into three columns based on the spaces in the string of characters:
raw_spp_data[c('genus', 'species', 'extra')] <- str_split_fixed(raw_spp_data$Scientific_name, ' ', 3)
#make Scientific_name the genus and species columns so that all other var. spp. etc are gone
raw_spp_data$Scientific_name <- paste(raw_spp_data$genus, raw_spp_data$species, sep=" ")
#delete extra rows (but could be important to keep the extra row just in case that is useful)
raw_spp_data <- raw_spp_data %>% select(-genus,-species,-extra)

#some still have spp. and x as their 'species', so delete those parts of the Scientific_name
raw_spp_data <-mutate(raw_spp_data,Scientific_name=sapply(strsplit(raw_spp_data$Scientific_name, split=' spp.', fixed=TRUE),function(x) (x[1])))
raw_spp_data <-mutate(raw_spp_data,Scientific_name=sapply(strsplit(raw_spp_data$Scientific_name, split=' X', fixed=TRUE),function(x) (x[1])))
raw_spp_data <-mutate(raw_spp_data,Scientific_name=sapply(strsplit(raw_spp_data$Scientific_name, split=' x', fixed=TRUE),function(x) (x[1])))
#now delete scientific names with only genus - make sure to specifically run!
raw_spp_data <- raw_spp_data[grepl(" ", raw_spp_data$Scientific_name),]  

#now raw_spp_data has all the scientific names (Scientific_name) for each park (ParkName) - one of each observation for species and park combinations. all scientific names with just the genus are deleted
#not sure if this is the best way to make the names more uniform. Should I be keeping the var. and spp. etc. It also deletes names for hybrids. I want to make this code universal, but I'm having trouble figuring out the best way to do so. 

#bring in bishop data to compare the accepted scientific names they have with the scientific names on the eaw_spp_data df by matching the Scientific_name of raw_spp_data to the Scientific_name of bishop1 (bishop data)
library(dplyr)
library(readxl)


spp_anti_join <- anti_join(raw_spp_data_orig, raw_spp_data)

bishop_csv_path <- "C:/Users/JJGross/Documents/Databases_copied_local/BISH_spp/POH_Names_Table.csv"
bishop <- read_csv(bishop_csv_path)
write_csv(bishop, "data/POH_Names_Table.csv")
bishop <- read_csv("data/POH_Names_Table.csv")

# read_excel
bish_spp_path <- "data/POH_Names_Table.xlsx"
bish <- readxl::read_excel(bish_spp_path)

# just use Hawaii Parks only (since POH is just for Hawaii)
pacn <- raw_spp_data
pacn <- raw_spp_data_orig |>
  filter(Park %in% c("HALE", "HAVO", "KALA", "KAHO", "PUHE", "PUHO"))

#dplyr::anti_join()

# compare BISH scientific names to rsp scientific names
pacn <- pacn %>%
  #select(Scientific_name) %>%
  distinct()

bishop <- bish %>%
#  select(scientificName) %>%
  distinct()

# all pacn spp with match in bish

match_yes <- pacn |>
  semi_join(bishop, join_by(Scientific_name == scientificName)) %>%
  arrange(Scientific_name)

match_no <- pacn |>
  anti_join(bishop, join_by(Scientific_name == scientificName)) %>%
  arrange(Scientific_name)


#format data - rename scientific name column to match RSP (raw_spp_data) 
# and delete duplicates; add row (bishop) that indicates the source
bishop1 <- bishop %>%
  select(scientificName, Accepted_scientificName) %>%
  rename_at('scientificName', ~'Scientific_name')%>%
  distinct(Scientific_name, .keep_all = TRUE)
bishop1$bishop <- 'bishop'

#make scientific names and A match through different formatting options 
# (same way as the raw_spp_data)
bishop1$Scientific_name <- gsub("X ", "x ", bishop1$Scientific_name)
bishop1$Scientific_name <- gsub("<d7> ", "", bishop1$Scientific_name)
bishop1 <- bishop1[grepl(" ", bishop1$Scientific_name),]

bishop1[c('genus', 'species', 'extra')] <- str_split_fixed(bishop1$Scientific_name, ' ', 3)
bishop1$Scientific_name <- paste(bishop1$genus, bishop1$species, sep=" ")
bishop1 <- bishop1 %>% select(-genus,-species,-extra)

bishop1 <-mutate(bishop1,Scientific_name=sapply(strsplit(bishop1$Scientific_name, split=' spp.', fixed=TRUE),function(x) (x[1])))
bishop1 <-mutate(bishop1,Scientific_name=sapply(strsplit(bishop1$Scientific_name, split=' x', fixed=TRUE),function(x) (x[1])))
bishop1 <-mutate(bishop1,Scientific_name=sapply(strsplit(bishop1$Scientific_name, split=' X', fixed=TRUE),function(x) (x[1])))
bishop1 <- bishop1[grepl(" ", bishop1$Scientific_name),] #delete scientific names with only genus - make sure to specifically run!



#same way/thing with the accepted names
bishop1$Accepted_scientificName <- gsub("X ", "x ", bishop1$Accepted_scientificName)
bishop1$Accepted_scientificName <- gsub("<d7> ", "", bishop1$Accepted_scientificName)
bishop1 <- bishop1[grepl(" ", bishop1$Accepted_scientificName),]

bishop1[c('genus', 'species', 'extra')] <- str_split_fixed(bishop1$Accepted_scientificName, ' ', 3)
bishop1$Accepted_scientificName <- paste(bishop1$genus, bishop1$species, sep=" ")
bishop1 <- bishop1 %>% select(-genus,-species,-extra)

bishop1 <-mutate(bishop1,Accepted_scientificNamee=sapply(strsplit(bishop1$Accepted_scientificName, split=' spp.', fixed=TRUE),function(x) (x[1])))
bishop1 <-mutate(bishop1,Accepted_scientificName=sapply(strsplit(bishop1$Accepted_scientificName, split=' x', fixed=TRUE),function(x) (x[1])))
bishop1 <-mutate(bishop1,Accepted_scientificName=sapply(strsplit(bishop1$Accepted_scientificName, split=' X', fixed=TRUE),function(x) (x[1])))
bishop1 <- bishop1[grepl(" ", bishop1$Accepted_scientificName),] #delete scientific names with only genus - make sure to specifically run!


#make copy of im data to not mess up the original df - only keep distinct names and only keep scientific name column
IMlol <- raw_spp_data %>%
  distinct(Scientific_name, .keep_all = TRUE)%>%
  select(Scientific_name)
IMlol$im <- 'im' #make row that indicates this data is from IM

#combine im and bishop data in a way that places NAs in the row if one df has the species and the other doesnt
alllol <- merge(IMlol, bishop1, by = "Scientific_name", all = TRUE)
#get rid of all NAs in IM column meaning those species on bishop's list that aren't in IM's list are deleted
alllol <- subset(alllol, !is.na(im))

#merge list with accepted names to main RSD in RSD1 (now have all the species for each park again, not distinct species names)
#raw_spp_data1 <- merge(raw_spp_data, alllol, by = "Scientific_name", all = TRUE)

#Accepted_scientificNameCorrect is the right scientific name and Scientific_name is the old scientific names
#this code is saying if there is na value in the accepted scientific name, use the original scientific name, and if there isnt use the accepted scientific name column values
alllol$Accepted_scientificNameCorrect <- ifelse(is.na(alllol$Accepted_scientificName), alllol$Scientific_name, alllol$Accepted_scientificName)
#make things a little easier to see
alllol <- select(alllol,Scientific_name, Accepted_scientificNameCorrect)

#merge correct name list with the original IM data by the old scientific name column
raw_spp_data <- merge(raw_spp_data, alllol, by = "Scientific_name", all = TRUE)

#make the NAs in the ASNcorrect column what the names in scientific name column are - then delete the scientific name column
raw_spp_data <- mutate(raw_spp_data, Scientific_nameUpdated=coalesce(Accepted_scientificNameCorrect, Scientific_name))
#makes this a smaller file so that it can actually be combined with the inat data witout crashing my R lol
raw_spp_data <- distinct(raw_spp_data, Accepted_scientificNameCorrect, ParkName)
#WOOOOOOOOOOOO YEAHHHHHHHHH 

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


