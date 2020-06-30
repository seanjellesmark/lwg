## Script for analysis on reserve conservation actions --------------------------------------
## Read in the excel sheet

# Reading in climatic data in nc format
# Precipitation

library(raster)
precip93 <- raster("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/climate/rainfall18.nc")
precip_df<-as.data.frame(precip93, xy = TRUE)
precip_df<-precip_df %>% 
  mutate(total_precip = if_else(Total.precipitation.amount > 2000, 0, Total.precipitation.amount))
  
str(precip_df)


ggplot() +
  geom_raster(data = precip_df, aes(x = x, y = y, fill = total_precip))

## XL data

xl_data<-"C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/Conservation_interventions.xlsx"

excel_sheets(path = xl_data)

tab_names <- excel_sheets(path = xl_data)

list_all<- lapply(sheets, function(x) read_excel(path = xl_data, sheet = x))

# Convert from list into data frame

df<- bind_rows(list_all) # doesn't quite work because of year 1993. Try another approach

df_doCall <- do.call("rbind", list_all) ## same problem I think


## online version from Youtube video "Combine Multiple Excel Sheets into a Single Table using R Tidyverse"
a<-excel_sheets("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/Conservation_interventions.xlsx") %>% 
  map(~read.xlsx("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/Conservation_interventions.xlsx",.))

## Problem seems to be that I have named the year variables as actual numerical value. Instead I create a function and join them together 

## NEW APPROACH - It doesn't make senseto do it this way. We only need a few of the sheets so let's create a function that 
## extracts an individual sheet and converts it into tidy format

cons_sheet<-function(sheet_name, value_name) {
read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/Conservation_interventions.xlsx", sheet = sheet_name) %>% 
  pivot_longer(cols = 3:28,
               names_to = "year",
               values_to = value_name)

}

test1<-cons_sheet(sheet_name = "fox_control", value_name = "foxes_killed")
test1$


## Before continuing I need to find a clever way to fill out NA's and true zeros. 
## first read in the missing data sheet
status_sheet<-cons_sheet(sheet_name = "status_sheet", value_name = "comments")

#### First try of building a model - using predator fencing as the only conservation intervention ####
# Firt, Load in the data the count data and clean it

# Load in the species data, clean it, add region and exclude species that are not part of the eight selected LWG species
remove_list <- paste(c("Number", "NB I", "Area acquired", "Long-term", "Management agreement", "On lowland wet", "Total"), collapse = '|') # list containing sub_site names to remove


cleaner<-function(x) {
  a<-read_excel("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/Malcolm_analysis.xlsx", sheet = x)
  a<-a[rowSums(is.na(a)) != ncol(a),]
  a<-a%>%
    rename("sub_site"=1)%>%
    filter(!grepl(remove_list, sub_site))
  a<-gather(a, year, count, -sub_site)
  a<-a%>%
    mutate(species=x)
}
slavonian<-cleaner("Slavonian grebe")
shoveler<-cleaner("Shoveler")
black_grebe<-cleaner("Black-necked grebe")
bittern<-cleaner("Bittern")
little_bittern<-cleaner("Little bittern")
little_egret<-cleaner("Little egret")
great<-cleaner("Great white egret")
garganey<-cleaner("Garganey")
common<-cleaner("Common scoter")
marsh<-cleaner("Marsh harrier")
hen<-cleaner("Hen harrier")
black_grouse<-cleaner("Black grouse")
capercaillie<-cleaner("Capercaillie")
spotted<-cleaner("Spotted crake")
corncrake<-cleaner("Corncrake")
crane<-cleaner("Crane")
black_stilt<-cleaner("Black-winged stilt")
avocet<-cleaner("Avocet")
stone<-cleaner("Stone-curlew")
ringed<-cleaner("Ringed plover")
lapwing<-cleaner("Lapwing")
snipe<-cleaner("Snipe")
wood<-cleaner("Wood sandpiper")
black_limosa<-cleaner("Black-t godwit (limosa)")
black_islandica<-cleaner("Black-t godwit (islandica)")
curlew<-cleaner("Curlew")
redshank<-cleaner("Redshank")
red<-cleaner("Red-necked phalarope")
mediterranean<-cleaner("Mediterranean gull")
little_tern<-cleaner("Little tern")
sandwich<-cleaner("Sandwich tern")
common_tern<-cleaner("Common tern")
roseate<-cleaner("Roseate tern")
nightjar<-cleaner("Nightjar")
woodlark<-cleaner("Woodlark")
nightingale<-cleaner("Nightingale")
ring<-cleaner("Ring ouzel")
cetti<-cleaner("Cetti's warbler")
savi<-cleaner("Savi's warbler")
dartford<-cleaner("Dartford warbler")
bearded<-cleaner("Bearded tit")
golden<-cleaner("Golden oriole")
chough<-cleaner("Chough")
cirl<-cleaner("Cirl bunting")
yellow_wagtail<-cleaner("Yellow wagtail")

# Rbind to transform into one dataset and delete all observations containing NA's.

reserve_species<-rbind(slavonian, shoveler, black_grebe, bittern, little_bittern,
                       little_egret, great, garganey, common, marsh, hen, black_grouse,
                       capercaillie, spotted, corncrake, crane, black_stilt, avocet,
                       stone, ringed, lapwing, snipe, wood, black_limosa, black_islandica,
                       curlew, redshank, red, mediterranean, little_tern, sandwich,
                       common_tern, roseate, nightjar, woodlark, nightingale, ring,
                       cetti, savi, dartford, bearded, golden, chough, cirl, yellow_wagtail)

reserve_species<-drop_na(reserve_species)

# Restrict species to the ones we need in this analysis
 
lwg_species <- c("Shoveler", "Curlew", "Lapwing", "Snipe", "Black-t godwit (limosa)", "Garganey", "Yellow wagtail", "Redshank")

lwg_reserve_species<-reserve_species%>%
  filter(species %in% lwg_species)

# Homogenise main and sub site information along area and habitat before acquisition
reserve_information<-read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/RSPB work sheet.xlsx", sheet = "reserve_information")

lwg_reserve_species<-left_join(lwg_reserve_species, reserve_information, by = c("sub_site"))

# Drop observations without proper main and sub site - remember to look these through again with Malcolm for some of the main sites which lack categorisation 
lwg_reserve_species<-lwg_reserve_species[complete.cases(lwg_reserve_species[ , 5]),]

# rearrange columns and delete the old sub_site variable to keep only one cleaned  main site and sub site variable
lwg_reserve_species<-lwg_reserve_species%>%
  select(-c(sub_site))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, everything())

# set counts to integer
lwg_reserve_species$count<-as.integer(lwg_reserve_species$count)
lwg_reserve_species$year<-as.integer(lwg_reserve_species$year)

# add predator fence data

predator_fence <- cons_sheet (sheet_name ="predator_fence", value_name = "fenced")
predator_fence <- left_join (predator_fence, reserve_information, by = "sub_site") 
predator_fence <- predator_fence %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, fenced)

predator_fence$year<-as.integer(predator_fence$year)
# Attach to reserve counts

lwg_reserve <- left_join(lwg_reserve_species, predator_fence, by = c("main_site", "sub_site", "year")) 
lwg_reserve

a<- lwg_reserve %>% 
  filter(!is.na(fenced)) %>% 
  distinct(sub_site)
