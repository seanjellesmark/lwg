# This script deals with extra analysis for chapter 1. requested by reviewers

# Read the species counts
bbs_species<-read.table("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/BBS.csv", header=TRUE, sep="\t", skip=0, comment.char="", 
                        check.names=FALSE, quote="", 
                        na.strings=c(""))
bbs_species<-bbs_species%>%
  rename(year=1)%>%
  rename(species=Species)
bbs_species$year <- gsub('"',"", bbs_species$year )
bbs_species$year<-as.integer(bbs_species$year)


# Read in BBS habitat data

bbs_habitat_all<-read.csv("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/habAll.csv", header=TRUE)%>%
  rename(Id=1)%>%
  rename(year=Year)%>%
  rename(Country=County)%>%
  rename(County=Column.4)%>%
  select(everything(), -c(Column.5, Column.21, Column.22, Column.26 ))


# Habitat composition of grids. 

# Transform the S1:S10 bbs_species data into long tidy format so that transect counts can be directly attached to transect habitat data

bbs_transect_counts <- bbs_species %>% 
  pivot_longer(cols = S1:S10,
               names_to = "Transect",
               values_to = "count") %>% 
  mutate(Transect = str_remove(Transect, "S")) 

# Summarise all bands into one transect value for each grid/species/year/earlylate combination

bbs_transect_sum_counts <- bbs_transect_counts %>% 
  group_by(Transect, Gridref, year, species, EarlyLate) %>% 
  summarise(count_sum = sum(count, na.rm = TRUE))

# Numeric so it can be joined with the habitat data

bbs_transect_sum_counts$Transect <- as.numeric(bbs_transect_sum_counts$Transect)


# Important to keep the true zero's (where a grid was observed but none of the species found). We therefore match species counts to each habitat transect
# and not the other way around as the species data only contains positive observations. If we use the full_join option we end up with something similar to that 
# of expand. This is not done as we don't have any info on neither the habitat or the counts for these year x transect combinations
# and we need the habitat data for the filtering process.

bbs_transect_counts_hab <- left_join(bbs_habitat_all, bbs_transect_sum_counts, by = c("Gridref", "year", "Transect"))

# Get rid of duplicates. The Day, Month and DateYear values differ which means some of the counts are attached twice thus creating false double counts 

bbs_transect_counts_hab <- bbs_transect_counts_hab %>% 
  select(everything(), - c(Month, Day, DateYear)) %>% 
  distinct()

# add altitude data
bbs_transect_counts_hab <- left_join(bbs_transect_counts_hab, altitude_data, by = "Gridref") %>% 
  filter(mean_altitude <= 250)


# Create trend for lapwing using counts from grass transects alone. We use lapwings to finish the script and then run for remaining species

overlapping_grid_reserves <- read.csv("C:/Users/seanj/OneDrive - University College London/GIS/UK_1kmGrid_reserves_Intersect.csv") %>% 
  select(Gridref)

bbs_transect_counts_hab$count <- as.double(bbs_transect_counts_hab$count)
  
lapwing_grass<-bbs_transect_counts_hab%>%
  filter(species=="L." | is.na(species))%>%
  filter(PLevel1 == "C" | SLevel1 =="C") %>%
  group_by(Gridref) %>% 
  filter(sum(is.na(count))<24) %>% 
  mutate(count=if_else(is.na(count), 0, count))%>%
  mutate(count = if_else(count > 10, 0, count)) %>% 
  group_by(Gridref, year, species)%>%
  summarise(max_count=max(count))

lapwing_grass <- anti_join(lapwing_grass, overlapping_grid_reserves, by = "Gridref")

# Create trim object - UPDATE; Something is off. 410 grids whereas we get 464 grids in the benchmark approach. 
# UPDATE. I was probably wrong about something being off
# What is more likely happening is that before we used grids which contained grassland but counts were counted in other habitat types within these grids. 
# We now lose the grids which contains grassland but no positive observations within grassland transects

lapwing_transect <- trim(max_count ~ Gridref + year, data=lapwing_grass, model=3, serialcor = FALSE, overdisp = TRUE) 
lapwing_bbs_transect<-index(lapwing_transect, "both")
plot(lapwing_bbs_transect, main="Lapwing - Model 3 transect") # Quite the difference. We are facing another problem now. The number of transects that
# goes into each grid is not constant and I don't think this makes sense. Suddenly we have waaay too much influence from "wrong" habitat defs
lapwing_results_transect<-results(lapwing_transect)
lapwing_imputed_transect<-imputed(lapwing_results_transect)
lapwing_observed_transect<-observed(lapwing_results_transect)
lapwing_n_transect<-n_sites(lapwing_results_transect, species_name = "Lapwing")

# 

### try the same with redshank


redshank_grass<-bbs_transect_counts_hab%>%
  filter(species=="RK" | is.na(species))%>%
  filter(PLevel1 == "C" | SLevel1 =="C") %>%
  group_by(Gridref) %>% 
  filter(sum(!is.na(count))>1) %>% 
  mutate(count=if_else(is.na(count), 0, count))%>%
  mutate(count = if_else(count > 10, 0, count)) %>% 
  group_by(Gridref, year, species)%>%
  summarise(max_count=max(count))

redshank_grass <- anti_join(redshank_grass, overlapping_grid_reserves, by = "Gridref")


redshank_transect <- trim(max_count ~ Gridref + year, data=redshank_grass, model=3, serialcor = FALSE, overdisp = TRUE) 
redshank_bbs_transect<-index(redshank_transect, "both")
plot(redshank_bbs_transect, main="redshank - Model 3 transect")
redshank_results_transect<-results(redshank_transect)
redshank_imputed_transect<-imputed(redshank_results_transect)
redshank_observed_transect<-observed(redshank_results_transect)
redshank_n_transect<-n_sites(redshank_results_transect, species_name = "redshank")



### Try out a (potentially) better way of creating trends. We use the bbs_transect_counts_hab data created ~ line 60

# Exclude counts > 10

lapwing_data <- bbs_transect_counts_hab %>% 
  filter(count_sum <= 10 | is.na(count_sum))

# Select Lapwing and NAs. NAs are kept and later replaced by 0 as these have been surveyed but no Lapwings found

lapwing_data <- lapwing_data %>% 
  filter(species =="L." | is.na(species))

# Create EarlyLate groupings
 # transform count_sum to double first as the first dmutate command doesn't work otherwise

lapwing_data$count_sum <- as.double(lapwing_data$count_sum)

lapwing_grid_data <- lapwing_data %>% 
  mutate(count_adj = if_else(is.na(count_sum), 0, count_sum)) %>% 
  group_by(Gridref, year, EarlyLate) %>% 
  summarise(grid_count = sum(count_adj))

lapwing_max_grid <- lapwing_grid_data %>% 
  group_by(Gridref, year) %>% 
  summarise(max_count = max(grid_count))

lapwing_redone <- left_join(bbs_grass_all, lapwing_max_grid, by = c("Gridref", "year"))

# Exclude overlapping grids
lapwing_redone1 <- anti_join(lapwing_redone, overlapping_grids, by = "Gridref")

# Filter lowland
lapwing_redone2 <- lapwing_redone1 %>% 
  filter(mean_altitude <=250)

# Correct missing obs again
lapwing_redone3 <-lapwing_redone2 %>% 
  mutate(max_count = if_else(is.na(max_count), 0, max_count))

# filter grids with only 1 obs. Doesn't work right now

lapwing_redone4 <- lapwing_redone3 %>% 
  group_by(Gridref) %>% 
  filter(length(.) > 1)

# run the trending procedure and see how it performs

lapwing_transect <- trim(max_count ~ Gridref + year, data=lapwing_redone3, model=3, serialcor = FALSE, overdisp = TRUE) 
lapwing_bbs_transect<-index(lapwing_transect, "both")
plot(lapwing_bbs_transect, main="Lapwing - Model 3 transect") # Quite the difference. We are facing another problem now. The number of transects that
# goes into each grid is not constant and I don't think this makes sense. Suddenly we have waaay too much influence from "wrong" habitat defs
lapwing_results_transect<-results(lapwing_transect)
lapwing_imputed_transect<-imputed(lapwing_results_transect)
lapwing_observed_transect<-observed(lapwing_results_transect)
lapwing_n_transect<-n_sites(lapwing_results_transect, species_name = "Lapwing")
