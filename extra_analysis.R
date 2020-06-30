# This script deals with extra analysis requested by reviewers

# Habitat composition of grids. Currently requires ch. 1 script to run

# Transform the S1:S10 bbs_species data into long tidy format so that transect counts can be directly attached to transect habitat data

bbs_transect_counts <- bbs_species %>% 
  pivot_longer(cols = S1:S10,
               names_to = "Transect",
               values_to = "count") %>% 
  mutate(Transect = str_remove(Transect, "S")) 

# Numeric so it can be joined with the habitat data

bbs_transect_counts$Transect <- as.numeric(bbs_transect_counts$Transect)

# Important to keep the true zero's (where a grid was observed but none of the species found). We therefore match species counts to each habitat transect
# and not the otehr way around as the species data only contains positive observations 

# Create a new BBS habitat dataframe without the month and day variable as they don't do anything good

bbs_transect_counts_hab <- left_join(bbs_habitat_all, bbs_transect_counts, by = c("Gridref", "year", "Transect", "Country", "County"))

# Get rid of duplicates

bbs_transect_counts_hab1 <- bbs_transect_counts_hab %>% 
  select(everything(), - c(Month.x, Day.x, DateYear.x, Month.y, Day.y, DateYear.y, Blank)) %>% 
  distinct()

# Try to create trend for lapwing using counts from grass transects alone.
overlapping_grid_reserves <- read.csv("C:/Users/seanj/OneDrive - University College London/GIS/UK_1kmGrid_reserves_Intersect.csv")

bbs_transect_counts_hab1$count <- as.double(bbs_transect_counts_hab1$count)
  
lapwing_grass2<-bbs_transect_counts_hab1%>%
  filter(species=="L." | is.na(species))%>%
  filter(PLevel1 == "C" | SLevel1 =="C") %>%
  group_by(Gridref) %>% 
  filter(sum(!is.na(count))>1) %>% 
  mutate(count=if_else(is.na(count), 0, count))%>%
  mutate(count = if_else(count > 10, 0, count)) %>% 
  group_by(Gridref, year, species)%>%
  summarise(max_count=max(count))

# Create trim object

lapwing_transect <- trim(max_count ~ Gridref + year, data=lapwing_grass2, model=3, serialcor = FALSE, overdisp = TRUE) 
lapwing_bbs_transect<-index(lapwing_transect, "both")
plot(lapwing_bbs_transect, main="Lapwing - Model 3 transect")
lapwing_results_transect<-results(lapwing_transect)
lapwing_imputed_transect<-imputed(lapwing_results_transect)
lapwing_observed_transect<-observed(lapwing_results_transect)
lapwing_n_transect<-n_sites(lapwing_results_transect, species_name = "Lapwing")

# Create index values from trim object  
  

years<-data.frame(year=1994:2018)
  full_data_of_grassland<-left_join(bbs_grass_all, years , by="year")
  aaa<-expand(full_data_of_grassland, Gridref, year)
  aaaa<-left_join(aaa, checker4, by=c("year", "Gridref"))
