# This script deals with extra analysis requested by reviewers

# Habitat composition of grids. Currently requires ch. 1 script to run

# Transform the S1:S10 bbs_species data into long tidy format so that transect counts can be directly attached to transect habitat data
bbs_transect_counts <- bbs_species %>% 
  pivot_



# Initial attempt with Lapwing and just 
grassland <- bbs_habitat_all %>% 
  filter(PLevel1 == "C" | SLevel1 == "C") %>% 
  select(year, Gridref)
