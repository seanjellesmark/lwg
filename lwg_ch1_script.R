# Summarise all the different Ch.1 scripts into one 

library(purrr)
library(readxl)
library(dplyr)
library(broom)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(tibble)
library(openxlsx)
library(rtrim)

# Normal counterfactual ----

# Breeding Bird Survey part

# Start off by loading raw BBS bird count data
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


# Filter for grassland  

bbs_grass_all<-bbs_habitat_all%>%
  filter(PLevel1=="C" | SLevel1=="C")%>%
  select(year, Gridref)
bbs_grass_all<-unique(bbs_grass_all)

#bbs_grass_all<-bbs_habitat_all%>%
#  filter(PLevel1=="C" | SLevel1=="C")
#bbs_grass_all<-bbs_grass_all%>%filter(PLevel2==6 | SLevel2==6)%>%
#  select(year, Gridref)
#bbs_grass_all<-unique(bbs_grass_all)


# Add grid altitude data
bbs_mean_altitude_uk<-read.csv("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/BBS GB Mean.csv", header=TRUE)%>%
  rename(Gridref=1)
bbs_median_altitude_uk<-read.csv("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/BBS GB Median.csv", header=TRUE)%>%
  rename(Gridref=1)
bbs_mean_altitude_ni<-read.csv("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/BBS NI mean.csv", header=TRUE)%>%
  rename(Gridref=1)
bbs_median_altitude_ni<-read.csv("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/BBS NI median.csv", header=TRUE)%>%
  rename(Gridref=1)
altitude_data_uk<-left_join(bbs_median_altitude_uk, bbs_mean_altitude_uk, by="Gridref")%>%
  rename(mean_altitude=3)%>%
  rename(median_altitude=2)
altitude_data_ni<-left_join(bbs_median_altitude_ni, bbs_mean_altitude_ni, by="Gridref")%>%
  rename(mean_altitude=3)%>%
  rename(median_altitude=2)
altitude_data_ni$Gridref<-as.character(altitude_data_ni$Gridref)
for (i in 1:nrow(altitude_data_ni)){
  altitude_data_ni$Gridref[i]<-paste("I",altitude_data_ni$Gridref[i], sep="")
}
altitude_data<-rbind(altitude_data_uk, altitude_data_ni)
altitude_data_ni$Gridref<-as.factor(altitude_data_ni$Gridref)
bbs_grass_all<-left_join(bbs_grass_all, altitude_data, by="Gridref")


# Merge the grid data which is used to create null values by accounting for the grids which are observed, with the species data
# to create a full dataset containing all species values plus surveyed squares

bbs_grass_species<-left_join(bbs_grass_all, bbs_species, by = c("year","Gridref"))

#The null values created for surveyed grids are missing county codes so these are added again
# first by deleting the incomplete county and country and then attaching a complete version

bbs_grass_species<-bbs_grass_species%>%
  select(everything(),-c(County, Country))
county_country<-bbs_habitat_all%>%
  select(year, Gridref, County, Country)
county_country<-county_country<-unique(county_country)
bbs_grass_species<-left_join(bbs_grass_species, county_country, by=c("year", "Gridref"))
bbs_grass_species<-bbs_grass_species%>%
  select(year, Gridref, County, Country, everything())

# Exclude squares overlapping with reserves
overlapping_grids<-read.delim("C:/Users/seanj/OneDrive - University College London/GIS/lwg_bbs_overlap.txt",
                              header = TRUE, sep = ",")%>%
  select(Gridref)

bbs_grass_species<-anti_join(bbs_grass_species, overlapping_grids, by=("Gridref"))

# Exclude grids which are not Lowland (over 250m altitude)

bbs_grass_species<-bbs_grass_species%>%filter(mean_altitude<=250)

# Subset any transect count above 10 to 0, as observations above 10 will not be breeding birds according to BBS waders protocol 

bbs_grass_species<-bbs_grass_species%>%mutate(S1=ifelse(S1>10,0,S1),
                                              S2=ifelse(S2>10,0,S2),
                                              S3=ifelse(S3>10,0,S3),
                                              S4=ifelse(S4>10,0,S4),
                                              S5=ifelse(S5>10,0,S5),
                                              S6=ifelse(S6>10,0,S6),
                                              S7=ifelse(S7>10,0,S7),
                                              S8=ifelse(S8>10,0,S8),
                                              S9=ifelse(S9>10,0,S9),
                                              S10=ifelse(S10>10,0,S10))

# Attach country data to each square
county_codes<-read_excel("C:/Users/seanj/OneDrive - University College London/RSPB/Data/county_codes_bbs.xlsx", sheet = "Sheet 1")
county_codes<-county_codes%>%
  rename(Country=COUNTY)%>%
  rename(County=County)
bbs_grass_species<-left_join(bbs_grass_species, county_codes, by=c("County", "Country"))

# Edit from original function - Now creates trim object instead which is then manually plotted to get n site for each species #
# Function that filters the BBS data for a species, attaches that to the grid data and, summarises transects and and adds null-values
# to zero count grids, then selects the maximum annual grid count between the late and the early survey and then calculates 
# a trend based on that data

bbs_trend_creator<-function(species_name, model_type=3, autocorrelation=TRUE, overdispersion=TRUE){
  
  checker4<-bbs_grass_species%>%
    filter(species==species_name | is.na(species))%>%
    mutate(count=rowSums(select(.,S1:S10), na.rm=TRUE))%>%
    group_by(Gridref, year, species)%>%
    summarise(max_count=max(count))
  
  a<-c(1994:2018)
  years<-data.frame(year=a)
  full_data_of_grassland<-left_join(bbs_grass_all, years , by="year")
  aaa<-expand(full_data_of_grassland, Gridref, year)
  aaaa<-left_join(aaa, checker4, by=c("year", "Gridref"))
  
  try_checker<-aaaa%>%
    group_by(Gridref)%>%
    filter(sum(is.na(max_count))<24)
  
  
  bbs_model <- trim(max_count ~ Gridref + year, data=try_checker, model=model_type, serialcor = autocorrelation, overdisp = overdispersion) 
}
# function that creates a wide dataframe for imputed and observed respectively

imputed<-function(results) {
  
  results%>%
    select(site, time, imputed)%>%
    pivot_wider(names_from = "time",
                values_from = "imputed")
}

observed<-function(results){
  results%>%
    select(site, time, observed)%>%
    pivot_wider(names_from = "time",
                values_from = "observed")
}
# create trim, index, plot, result, imputed and observed data frame

lapwing_trim<-bbs_trend_creator(species_name="L.", autocorrelation=FALSE, model_type=3)
lapwing_bbs<-index(lapwing_trim, "both")
plot(lapwing_bbs, main="Lapwing - Model 3")
lapwing_results<-results(lapwing_trim)
lapwing_imputed<-imputed(lapwing_results)
lapwing_observed<-observed(lapwing_results)


curlew_trim<-bbs_trend_creator(species_name="CU", autocorrelation=FALSE, overdispersion=FALSE, model_type=3)
curlew_bbs<-index(curlew_trim, "both")
plot(curlew_bbs, main="Curlew - Model 3")
curlew_results<-results(curlew_trim)
curlew_imputed<-imputed(curlew_results)
curlew_observed<-observed(curlew_results)


redshank_trim<-bbs_trend_creator(species_name="RK", autocorrelation=FALSE,overdispersion=FALSE, model_type=3)
redshank_bbs<-index(redshank_trim, "both")
plot(redshank_bbs, main="Redshank - Model 3")
redshank_results<-results(redshank_trim)
redshank_imputed<-imputed(redshank_results)
redshank_observed<-observed(redshank_results)


snipe_trim<-bbs_trend_creator(species_name="SN", autocorrelation=FALSE,overdispersion=FALSE, model_type=3)
snipe_bbs<-index(snipe_trim, "both")
plot(snipe_bbs, main="Snipe - Model 3")
snipe_results<-results(snipe_trim)
snipe_imputed<-imputed(snipe_results)
snipe_observed<-observed(snipe_results)


yellow_wagtail_trim<-bbs_trend_creator(species_name="YW", autocorrelation=FALSE,overdispersion=FALSE, model_type=3)
yellow_wagtail_bbs<-index(yellow_wagtail_trim, "both")
plot(yellow_wagtail_bbs, main="Yellow Wagtail - Model 3")
yellow_wagtail_results<-results(yellow_wagtail_trim)
yellow_wagtail_imputed<-imputed(yellow_wagtail_results)
yellow_wagtail_observed<-observed(yellow_wagtail_results)


# function that attaches a species name to each index series and calculates upper and lower limits 
# in order to plot them together in ggplot 

plot_prepare<-function(data, species_name, BBS_or_reserve="Counterfactual"){
  data%>%
    mutate(se_positive=imputed+se_imp)%>%
    mutate(se_negative=imputed-se_imp)%>%
    mutate(species=species_name)%>%
    mutate(trend=BBS_or_reserve)
}

lapwing_bbs_ggplot_ready<-plot_prepare(lapwing_bbs, "Lapwing")
curlew_bbs_ggplot_ready<-plot_prepare(curlew_bbs, "Curlew")
redshank_bbs_ggplot_ready<-plot_prepare(redshank_bbs, "Redshank")
snipe_bbs_ggplot_ready<-plot_prepare(snipe_bbs, "Snipe")
yellow_wagtail_bbs_ggplot_ready<-plot_prepare(yellow_wagtail_bbs, "Yellow Wagtail")

five_bbs_species<-rbind(lapwing_bbs_ggplot_ready, curlew_bbs_ggplot_ready, redshank_bbs_ggplot_ready, 
                        snipe_bbs_ggplot_ready, yellow_wagtail_bbs_ggplot_ready)

# RSPB reserve part

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

lwg_species <- c("Shoveler", "Curlew", "Lapwing", "Snipe", "Black-t godwit (limosa)", "Garganey", "Yellow wagtail", "Redshank")

lwg_reserve_species<-reserve_species%>%
  filter(species %in% lwg_species)

# Homogenise main and sub site information along area and habitat before acquisition
reserve_information<-read_excel("C:/Users/seanj/OneDrive - University College London/RSPB/Data/RSPB work sheet.xlsx", sheet = "reserve_information")

lwg_reserve_species<-left_join(lwg_reserve_species, reserve_information, by = c("sub_site"))

# Drop observations without proper main and sub site - remember to look these through again with Malcolm for some of the main sites which lack categorisation 
lwg_reserve_species<-lwg_reserve_species[complete.cases(lwg_reserve_species[ , 5]),]

# rearrange columns and delete the old sub_site variable to keep only one cleaned  main site and sub site variable
lwg_reserve_species<-lwg_reserve_species%>%
  select(-c(sub_site))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, everything())

# set counts to integer format
lwg_reserve_species$count<-as.integer(lwg_reserve_species$count)
lwg_reserve_species$year<-as.integer(lwg_reserve_species$year)

# add 0.1 to all counts NOT DONE
##lwg_reserve_species<-lwg_reserve_species%>%
#  mutate(count=count+0.1)

# plotting reserve trends (I should create a function for this)
curlew_lwg<-lwg_reserve_species%>%
  filter(species=="Curlew")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annualcurlew<-trim(count~site+year, data=curlew_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_curlew<-index(annualcurlew, "both")
plot(index_curlew, main="Reserve Curlew - Model 3")
curlew_reserve_results<-results(annualcurlew)
curlew_reserve_imputed<-imputed(curlew_reserve_results)
curlew_reserve_observed<-observed(curlew_reserve_results)

lapwing_lwg<-lwg_reserve_species%>%
  filter(species=="Lapwing")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annuallapwing<-trim(count~site+year, data=lapwing_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_lapwing<-index(annuallapwing, "both")
plot(index_lapwing, main="Reserve Lapwing - Model 3")
lapwing_reserve_results<-results(annuallapwing)
lapwing_reserve_imputed<-imputed(lapwing_reserve_results)
lapwing_reserve_observed<-observed(lapwing_reserve_results)

redshank_lwg<-lwg_reserve_species%>%
  filter(species=="Redshank")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annualredshank<-trim(count~site+year, data=redshank_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_redshank<-index(annualredshank, "both")
plot(index_redshank, main="Reserve Redshank - Model 3")
redshank_reserve_results<-results(annualredshank)
redshank_reserve_imputed<-imputed(redshank_reserve_results)
redshank_reserve_observed<-observed(redshank_reserve_results)

snipe_lwg<-lwg_reserve_species%>%
  filter(species=="Snipe")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annualsnipe<-trim(count~site+year, data=snipe_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_snipe<-index(annualsnipe, "both")
plot(index_snipe, main="Reserve Snipe - Model 3")
snipe_reserve_results<-results(annualsnipe)
snipe_reserve_imputed<-imputed(snipe_reserve_results)
snipe_reserve_observed<-observed(snipe_reserve_results)

yellow_wagtail_lwg<-lwg_reserve_species%>%
  filter(species=="Yellow wagtail")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annualwagtail<-trim(count~site+year, data=yellow_wagtail_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_yellow_wagtail<-index(annualwagtail, "both")
plot(index_yellow_wagtail, main="Reserve Yellow Wagtail - Model 3")
yellow_wagtail_reserve_results<-results(annualwagtail)
yellow_wagtail_reserve_imputed<-imputed(yellow_wagtail_reserve_results)
yellow_wagtail_reserve_observed<-observed(yellow_wagtail_reserve_results)


# Prepare for ggplot
lapwing_reserve_ggplot_ready<-plot_prepare(index_lapwing, "Lapwing", BBS_or_reserve = "Reserve")
curlew_reserve_ggplot_ready<-plot_prepare(index_curlew, "Curlew", BBS_or_reserve = "Reserve")
redshank_reserve_ggplot_ready<-plot_prepare(index_redshank, "Redshank", BBS_or_reserve = "Reserve")
snipe_reserve_ggplot_ready<-plot_prepare(index_snipe, "Snipe", BBS_or_reserve = "Reserve")
yellow_wagtail_reserve_ggplot_ready<-plot_prepare(index_yellow_wagtail, "Yellow Wagtail", BBS_or_reserve = "Reserve")

five_reserve_species<-rbind(lapwing_reserve_ggplot_ready, curlew_reserve_ggplot_ready, redshank_reserve_ggplot_ready, 
                            snipe_reserve_ggplot_ready, yellow_wagtail_reserve_ggplot_ready)

# Combine and plot reserve and bbs trends
five_species_combined<-rbind(five_reserve_species, five_bbs_species)

plot_five_species_combined<-ggplot(data=five_species_combined, aes(x=time, y=imputed, colour=trend)) + 
  geom_ribbon(aes(ymin=five_species_combined$se_negative, 
                  ymax=five_species_combined$se_positive), 
              fill = "grey70", linetype=3, alpha=0.1)+ylab("Index - 1994 = 1")+xlab("Time")+
  geom_line(linetype = 2)+
  geom_hline(yintercept = 1, linetype=2)+facet_wrap(~species, scales="free")+
  theme_classic()+scale_x_continuous(name = "Time", limits=c(1994,2018), breaks = seq(1994,2019, by = 6))+
  theme(strip.text = element_text(size=20), legend.title = element_blank())+
  theme(legend.position = c(0.8,0.35), legend.text = element_text(size = 25), legend.key.size = unit(2, "cm"),
        axis.title=element_text(size=20,face="bold"), axis.text=element_text(size=20))


plot_five_species_combined

# ggsave(filename = "C:/Users/seanj/OneDrive/Skrivebord/Plots and graphs/UK_trends_normal.png", 
#       plot = plot_five_species_combined, width = 40, height = 20, dpi = 1000, units = "cm")
## Welch Two Sample t-test
t.test(index_lapwing$imputed, lapwing_bbs$imputed)
t.test(index_redshank$imputed, redshank_bbs$imputed)
t.test(index_snipe$imputed, snipe_bbs$imputed)
t.test(index_yellow_wagtail$imputed, yellow_wagtail_bbs$imputed)
t.test(index_curlew$imputed, curlew_bbs$imputed)

## independent 2-group Mann-Whitney U Test
wilcox.test(index_lapwing$imputed, lapwing_bbs$imputed)
wilcox.test(index_redshank$imputed, redshank_bbs$imputed)
wilcox.test(index_snipe$imputed, snipe_bbs$imputed)
wilcox.test(index_yellow_wagtail$imputed, yellow_wagtail_bbs$imputed)
wilcox.test(index_curlew$imputed, curlew_bbs$imputed)

## Differences between trend values
difference_in_trends<-function(index_reserve, bird_bbs, name){
  index_reserve<-index_reserve$imputed-bird_bbs$imputed
  a<-c(1994:2018)
  years<-data.frame(year=a)
  index_reserve<-cbind(index_reserve, years)
  index_reserve<-index_reserve%>%
    mutate(species=name)%>%
    rename(difference=1)
}

lapwing_differences<-difference_in_trends(index_lapwing, lapwing_bbs, name = "Lapwing")
curlew_differences<-difference_in_trends(index_curlew, curlew_bbs, name = "Curlew")
redshank_differences<-difference_in_trends(index_redshank, redshank_bbs, name = "Redshank")
yellow_wagtail_differences<-difference_in_trends(index_yellow_wagtail, yellow_wagtail_bbs, name = "Yellow Wagtail")
snipe_differences<-difference_in_trends(index_snipe, snipe_bbs, name = "Snipe")
differences<-rbind(lapwing_differences, curlew_differences, redshank_differences, yellow_wagtail_differences, 
                   snipe_differences)

# boxplot the index values per species by reserve vs counterfactual 
ggplot(five_species_combined, aes( x = species, y = imputed, colour = trend))+
  geom_boxplot()+theme_bw()+labs(x = "",y = "Index")

## Kruskal-Wallis Test

kruskal<-function(bird_species){
  data<-five_species_combined%>%
    filter(species==bird_species)
  data$trend<-as.factor(data$trend)  
  
  kruskal.test(imputed ~ trend, data = data)
}
kruskal("Lapwing")
kruskal("Curlew")
kruskal("Yellow Wagtail")
kruskal("Snipe")
kruskal("Redshank")


# Liberal counterfactual ----

library(purrr)
library(readxl)
library(dplyr)
library(broom)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(tibble)
library(openxlsx)
library(rtrim)

# Read in BBS bird count raw data
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

# Assign bbs_habitat_all to bbs_grass all so that it uses observations from every habitat type 
bbs_grass_all<-bbs_habitat_all

# Add grid altitude data
bbs_mean_altitude_uk<-read.csv("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/BBS GB Mean.csv", header=TRUE)%>%
  rename(Gridref=1)
bbs_median_altitude_uk<-read.csv("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/BBS GB Median.csv", header=TRUE)%>%
  rename(Gridref=1)
bbs_mean_altitude_ni<-read.csv("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/BBS NI mean.csv", header=TRUE)%>%
  rename(Gridref=1)
bbs_median_altitude_ni<-read.csv("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/BBS NI median.csv", header=TRUE)%>%
  rename(Gridref=1)
altitude_data_uk<-left_join(bbs_median_altitude_uk, bbs_mean_altitude_uk, by="Gridref")%>%
  rename(mean_altitude=3)%>%
  rename(median_altitude=2)
altitude_data_ni<-left_join(bbs_median_altitude_ni, bbs_mean_altitude_ni, by="Gridref")%>%
  rename(mean_altitude=3)%>%
  rename(median_altitude=2)
altitude_data_ni$Gridref<-as.character(altitude_data_ni$Gridref)
for (i in 1:nrow(altitude_data_ni)){
  altitude_data_ni$Gridref[i]<-paste("I",altitude_data_ni$Gridref[i], sep="")
}
altitude_data<-rbind(altitude_data_uk, altitude_data_ni)
altitude_data_ni$Gridref<-as.factor(altitude_data_ni$Gridref)
bbs_grass_all<-left_join(bbs_grass_all, altitude_data, by="Gridref")




# Merge the grid data which is used to create null values by accounting for the grids which are observed, with the species data
# to create a full dataset containing all species values plus surveyed squares

bbs_grass_species<-left_join(bbs_grass_all, bbs_species, by = c("year","Gridref"))

#The null values created for surveyed grids are missing county codes so these are added again
# first by deleting the incomplete county and country and then attaching a complete version

bbs_grass_species<-bbs_grass_species%>%
  select(everything(),-c(County, Country))
county_country<-bbs_habitat_all%>%
  select(year, Gridref, County, Country)
county_country<-county_country<-unique(county_country)
bbs_grass_species<-left_join(bbs_grass_species, county_country, by=c("year", "Gridref"))
bbs_grass_species<-bbs_grass_species%>%
  select(year, Gridref, County, Country, everything())

# Exclude squares overlapping with reserves

overlapping_grids<-read.delim("C:/Users/seanj/OneDrive - University College London/GIS/lwg_bbs_overlap.txt",
                              header = TRUE, sep = ",")%>%
  select(Gridref)

bbs_grass_species<-anti_join(bbs_grass_species, overlapping_grids, by=("Gridref"))

# Subset any transect count above 10 to 0, as observations above 10 will not be breeding birds according to BBS waders protocol 

bbs_grass_species<-bbs_grass_species%>%mutate(S1=ifelse(S1>10,0,S1),
                                              S2=ifelse(S2>10,0,S2),
                                              S3=ifelse(S3>10,0,S3),
                                              S4=ifelse(S4>10,0,S4),
                                              S5=ifelse(S5>10,0,S5),
                                              S6=ifelse(S6>10,0,S6),
                                              S7=ifelse(S7>10,0,S7),
                                              S8=ifelse(S8>10,0,S8),
                                              S9=ifelse(S9>10,0,S9),
                                              S10=ifelse(S10>10,0,S10))

# Attach country data to each square
county_codes<-read_excel("C:/Users/seanj/OneDrive - University College London/RSPB/Data/county_codes_bbs.xlsx", sheet = "Sheet 1")
county_codes<-county_codes%>%
  rename(Country=COUNTY)%>%
  rename(County=County)
bbs_grass_species<-left_join(bbs_grass_species, county_codes, by=c("County", "Country"))

# Function that filters the BBS data for a species, attaches that to the grid data and, summarises transects and and adds null-values
# to zero count grids, then selects the maximum annual grid count between the late and the early survey and then calculates 
# a trend based on that data


bbs_trend_creator<-function(species_name, model_type=3, autocorrelation=TRUE, overdispersion=TRUE){
  
  checker4<-bbs_grass_species%>%
    filter(species==species_name | is.na(species))%>%
    mutate(count=rowSums(select(.,S1:S10), na.rm=TRUE))%>%
    group_by(Gridref, year, species)%>%
    summarise(max_count=max(count))
  
  a<-c(1994:2018)
  years<-data.frame(year=a)
  full_data_of_grassland<-left_join(bbs_grass_all, years , by="year")
  aaa<-expand(full_data_of_grassland, Gridref, year)
  aaaa<-left_join(aaa, checker4, by=c("year", "Gridref"))
  
  try_checker<-aaaa%>%
    group_by(Gridref)%>%
    filter(sum(is.na(max_count))<24)
  
  
  bbs_model <- trim(max_count ~ Gridref + year, data=try_checker, model=model_type, serialcor = autocorrelation, overdisp = overdispersion) 
}
# function that creates a wide dataframe for imputed and observed respectively

imputed<-function(results) {
  
  results%>%
    select(site, time, imputed)%>%
    pivot_wider(names_from = "time",
                values_from = "imputed")
}

observed<-function(results){
  results%>%
    select(site, time, observed)%>%
    pivot_wider(names_from = "time",
                values_from = "observed")
}
# create trim, index, plot, result, imputed and observed data frame

lapwing_trim<-bbs_trend_creator(species_name="L.", autocorrelation=FALSE, model_type=3)
lapwing_bbs<-index(lapwing_trim, "both")
plot(lapwing_bbs, main="Lapwing - Model 3")
lapwing_results<-results(lapwing_trim)
lapwing_imputed<-imputed(lapwing_results)
lapwing_observed<-observed(lapwing_results)


curlew_trim<-bbs_trend_creator(species_name="CU", autocorrelation=FALSE, overdispersion=FALSE, model_type=3)
curlew_bbs<-index(curlew_trim, "both")
plot(curlew_bbs, main="Curlew - Model 3")
curlew_results<-results(curlew_trim)
curlew_imputed<-imputed(curlew_results)
curlew_observed<-observed(curlew_results)


redshank_trim<-bbs_trend_creator(species_name="RK", autocorrelation=FALSE,overdispersion=FALSE, model_type=3)
redshank_bbs<-index(redshank_trim, "both")
plot(redshank_bbs, main="Redshank - Model 3")
redshank_results<-results(redshank_trim)
redshank_imputed<-imputed(redshank_results)
redshank_observed<-observed(redshank_results)


snipe_trim<-bbs_trend_creator(species_name="SN", autocorrelation=FALSE,overdispersion=FALSE, model_type=3)
snipe_bbs<-index(snipe_trim, "both")
plot(snipe_bbs, main="Snipe - Model 3")
snipe_results<-results(snipe_trim)
snipe_imputed<-imputed(snipe_results)
snipe_observed<-observed(snipe_results)


yellow_wagtail_trim<-bbs_trend_creator(species_name="YW", autocorrelation=FALSE,overdispersion=FALSE, model_type=3)
yellow_wagtail_bbs<-index(yellow_wagtail_trim, "both")
plot(yellow_wagtail_bbs, main="Yellow Wagtail - Model 3")
yellow_wagtail_results<-results(yellow_wagtail_trim)
yellow_wagtail_imputed<-imputed(yellow_wagtail_results)
yellow_wagtail_observed<-observed(yellow_wagtail_results)


# function that attaches a species name to each index series and calculates upper and lower limits 
# in order to plot them together in ggplot 

plot_prepare<-function(data, species_name, BBS_or_reserve="Liberal counterfactual"){
  data%>%
    mutate(se_positive=imputed+se_imp)%>%
    mutate(se_negative=imputed-se_imp)%>%
    mutate(species=species_name)%>%
    mutate(trend=BBS_or_reserve)
}

lapwing_bbs_ggplot_ready<-plot_prepare(lapwing_bbs, "Lapwing")
curlew_bbs_ggplot_ready<-plot_prepare(curlew_bbs, "Curlew")
redshank_bbs_ggplot_ready<-plot_prepare(redshank_bbs, "Redshank")
snipe_bbs_ggplot_ready<-plot_prepare(snipe_bbs, "Snipe")
yellow_wagtail_bbs_ggplot_ready<-plot_prepare(yellow_wagtail_bbs, "Yellow Wagtail")

# Bind the five species together and plot them with the SE as shaded outlines
five_bbs_species_liberal<-rbind(lapwing_bbs_ggplot_ready, curlew_bbs_ggplot_ready, redshank_bbs_ggplot_ready, 
                        snipe_bbs_ggplot_ready, yellow_wagtail_bbs_ggplot_ready)

# Script for creating reserve trends


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

lwg_species <- c("Shoveler", "Curlew", "Lapwing", "Snipe", "Black-t godwit (limosa)", "Garganey", "Yellow wagtail", "Redshank")

lwg_reserve_species<-reserve_species%>%
  filter(species %in% lwg_species)

# Homogenise main and sub site information along area and habitat before acquisition
reserve_information<-read_excel("C:/Users/seanj/OneDrive - University College London/RSPB/Data/RSPB work sheet.xlsx", sheet = "reserve_information")

lwg_reserve_species<-left_join(lwg_reserve_species, reserve_information, by = c("sub_site"))

# Drop observations without proper main and sub site - remember to look these through again with Malcolm for some of the main sites which lack categorisation 
lwg_reserve_species<-lwg_reserve_species[complete.cases(lwg_reserve_species[ , 5]),]

# rearrange columns and delete the old sub_site variable to keep only one cleaned  main site and sub site variable
lwg_reserve_species<-lwg_reserve_species%>%
  select(-c(sub_site))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, everything())

# set counts to integer format
lwg_reserve_species$count<-as.integer(lwg_reserve_species$count)
lwg_reserve_species$year<-as.integer(lwg_reserve_species$year)

# add 0.1 to all counts NOT DONE
##lwg_reserve_species<-lwg_reserve_species%>%
#  mutate(count=count+0.1)

# Create trends similar to those in the BBS trends using sub site as site variable

# reserve_trend_creator<-function(data, species_name, autocorrelation=TRUE, overdispersion=TRUE){}



# plotting reserve trends (I should create a function for this)
curlew_lwg<-lwg_reserve_species%>%
  filter(species=="Curlew")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annualcurlew<-trim(count~site+year, data=curlew_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_curlew<-index(annualcurlew, "both")
plot(index_curlew, main="Reserve Curlew - Model 3")
curlew_reserve_results<-results(annualcurlew)
curlew_reserve_imputed<-imputed(curlew_reserve_results)
curlew_reserve_observed<-observed(curlew_reserve_results)

lapwing_lwg<-lwg_reserve_species%>%
  filter(species=="Lapwing")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annuallapwing<-trim(count~site+year, data=lapwing_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_lapwing<-index(annuallapwing, "both")
plot(index_lapwing, main="Reserve Lapwing - Model 3")
lapwing_reserve_results<-results(annuallapwing)
lapwing_reserve_imputed<-imputed(lapwing_reserve_results)
lapwing_reserve_observed<-observed(lapwing_reserve_results)

redshank_lwg<-lwg_reserve_species%>%
  filter(species=="Redshank")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annualredshank<-trim(count~site+year, data=redshank_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_redshank<-index(annualredshank, "both")
plot(index_redshank, main="Reserve Redshank - Model 3")
redshank_reserve_results<-results(annualredshank)
redshank_reserve_imputed<-imputed(redshank_reserve_results)
redshank_reserve_observed<-observed(redshank_reserve_results)

snipe_lwg<-lwg_reserve_species%>%
  filter(species=="Snipe")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annualsnipe<-trim(count~site+year, data=snipe_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_snipe<-index(annualsnipe, "both")
plot(index_snipe, main="Reserve Snipe - Model 3")
snipe_reserve_results<-results(annualsnipe)
snipe_reserve_imputed<-imputed(snipe_reserve_results)
snipe_reserve_observed<-observed(snipe_reserve_results)

yellow_wagtail_lwg<-lwg_reserve_species%>%
  filter(species=="Yellow wagtail")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annualwagtail<-trim(count~site+year, data=yellow_wagtail_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_yellow_wagtail<-index(annualwagtail, "both")
plot(index_yellow_wagtail, main="Reserve Yellow Wagtail - Model 3")
yellow_wagtail_reserve_results<-results(annualwagtail)
yellow_wagtail_reserve_imputed<-imputed(yellow_wagtail_reserve_results)
yellow_wagtail_reserve_observed<-observed(yellow_wagtail_reserve_results)


# Prepare for ggplot
lapwing_reserve_ggplot_ready<-plot_prepare(index_lapwing, "Lapwing", BBS_or_reserve = "Reserve")
curlew_reserve_ggplot_ready<-plot_prepare(index_curlew, "Curlew", BBS_or_reserve = "Reserve")
redshank_reserve_ggplot_ready<-plot_prepare(index_redshank, "Redshank", BBS_or_reserve = "Reserve")
snipe_reserve_ggplot_ready<-plot_prepare(index_snipe, "Snipe", BBS_or_reserve = "Reserve")
yellow_wagtail_reserve_ggplot_ready<-plot_prepare(index_yellow_wagtail, "Yellow Wagtail", BBS_or_reserve = "Reserve")

five_reserve_species<-rbind(lapwing_reserve_ggplot_ready, curlew_reserve_ggplot_ready, redshank_reserve_ggplot_ready, 
                            snipe_reserve_ggplot_ready, yellow_wagtail_reserve_ggplot_ready)



# Combine and plot reserve and bbs trends
five_species_combined<-rbind(five_reserve_species, five_bbs_species_liberal)

plot_five_species_combined<-ggplot(data=five_species_combined, aes(x=time, y=imputed, colour=trend)) + 
  geom_ribbon(aes(ymin=five_species_combined$se_negative, 
                  ymax=five_species_combined$se_positive), 
              fill = "grey70", linetype=3, alpha=0.1)+ylab("Index - 1994 = 1")+xlab("Time")+
  geom_hline(yintercept = 1, linetype=2)+facet_wrap(~species, scales="free")+
  theme_classic()+scale_x_continuous(name = "Time", limits=c(1994,2018), breaks = seq(1994,2019, by = 6))+
  theme(strip.text = element_text(size=20), legend.title = element_blank())+geom_smooth(se = FALSE, size = 1)+
  theme(legend.position = c(0.8,0.35), legend.text = element_text(size = 25), legend.key.size = unit(2, "cm"),
        axis.title=element_text(size=20,face="bold"), axis.text=element_text(size=20))



plot_five_species_combined

# ggsave(filename = "C:/Users/seanj/OneDrive/Skrivebord/Plots and graphs/UK_trends_liberal.tiff", 
#       plot = plot_five_species_combined, width = 42, height = 20, dpi = 1000, units = "cm")

## Welch Two Sample t-test
t.test(index_lapwing$imputed, lapwing_bbs$imputed)
t.test(index_redshank$imputed, redshank_bbs$imputed)
t.test(index_snipe$imputed, snipe_bbs$imputed)
t.test(index_yellow_wagtail$imputed, yellow_wagtail_bbs$imputed)
t.test(index_curlew$imputed, curlew_bbs$imputed)

## independent 2-group Mann-Whitney U Test
wilcox.test(index_lapwing$imputed, lapwing_bbs$imputed)
wilcox.test(index_redshank$imputed, redshank_bbs$imputed)
wilcox.test(index_snipe$imputed, snipe_bbs$imputed)
wilcox.test(index_yellow_wagtail$imputed, yellow_wagtail_bbs$imputed)
wilcox.test(index_curlew$imputed, curlew_bbs$imputed)

## Differences between trend values
difference_in_trends<-function(index_reserve, bird_bbs, name){
  index_reserve<-index_reserve$imputed-bird_bbs$imputed
  a<-c(1994:2018)
  years<-data.frame(year=a)
  index_reserve<-cbind(index_reserve, years)
  index_reserve<-index_reserve%>%
    mutate(species=name)%>%
    rename(difference=1)
}

lapwing_differences<-difference_in_trends(index_lapwing, lapwing_bbs, name = "Lapwing")
curlew_differences<-difference_in_trends(index_curlew, curlew_bbs, name = "Curlew")
redshank_differences<-difference_in_trends(index_redshank, redshank_bbs, name = "Redshank")
yellow_wagtail_differences<-difference_in_trends(index_yellow_wagtail, yellow_wagtail_bbs, name = "Yellow Wagtail")
snipe_differences<-difference_in_trends(index_snipe, snipe_bbs, name = "Snipe")
differences<-rbind(lapwing_differences, curlew_differences, redshank_differences, yellow_wagtail_differences, snipe_differences)

# boxplot the index values per species by reserve vs counterfactual 
ggplot(five_species_combined, aes( x = species, y = imputed, colour = trend))+
  geom_boxplot()+theme_bw()+labs(x = "",y = "Index")

## Kruskal-Wallis Test

kruskal<-function(bird_species){
  data<-five_species_combined%>%
    filter(species==bird_species)
  data$trend<-as.factor(data$trend)  
  
  kruskal.test(imputed ~ trend, data = data)
}
kruskal("Lapwing")
kruskal("Curlew")
kruskal("Yellow Wagtail")
kruskal("Snipe")
kruskal("Redshank")

# Conservative counterfactual ----


library(purrr)
library(readxl)
library(dplyr)
library(broom)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(tibble)
library(openxlsx)
library(rtrim)

# Read in BBS bird count raw data
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

# Filter only for grids containing habitat code 5-8 grasslands 

bbs_grass_all<-bbs_habitat_all%>%
  filter(PLevel1=="C" & PLevel2 == 5 | SLevel1=="C" & SLevel2 == 5 | PLevel1=="C" & PLevel2 == 6 | SLevel1=="C" & SLevel2 == 6 | PLevel1=="C" & PLevel2 == 7 | SLevel1=="C" & SLevel2 == 7 | PLevel1=="C" & PLevel2 == 8 | SLevel1=="C" & SLevel2 == 8)%>%
  select(year, Gridref)
bbs_grass_all<-unique(bbs_grass_all)

# Add grid altitude data
bbs_mean_altitude_uk<-read.csv("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/BBS GB Mean.csv", header=TRUE)%>%
  rename(Gridref=1)
bbs_median_altitude_uk<-read.csv("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/BBS GB Median.csv", header=TRUE)%>%
  rename(Gridref=1)
bbs_mean_altitude_ni<-read.csv("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/BBS NI mean.csv", header=TRUE)%>%
  rename(Gridref=1)
bbs_median_altitude_ni<-read.csv("C:/Users/seanj/OneDrive/Skrivebord/R/RSPB/BBS NI median.csv", header=TRUE)%>%
  rename(Gridref=1)
altitude_data_uk<-left_join(bbs_median_altitude_uk, bbs_mean_altitude_uk, by="Gridref")%>%
  rename(mean_altitude=3)%>%
  rename(median_altitude=2)
altitude_data_ni<-left_join(bbs_median_altitude_ni, bbs_mean_altitude_ni, by="Gridref")%>%
  rename(mean_altitude=3)%>%
  rename(median_altitude=2)
altitude_data_ni$Gridref<-as.character(altitude_data_ni$Gridref)
for (i in 1:nrow(altitude_data_ni)){
  altitude_data_ni$Gridref[i]<-paste("I",altitude_data_ni$Gridref[i], sep="")
}
altitude_data<-rbind(altitude_data_uk, altitude_data_ni)
altitude_data_ni$Gridref<-as.factor(altitude_data_ni$Gridref)
bbs_grass_all<-left_join(bbs_grass_all, altitude_data, by="Gridref")


# Merge the grid data which is used to create null values by accounting for the grids which are observed, with the species data
# to create a full dataset containing all species values plus surveyed squares

bbs_grass_species<-left_join(bbs_grass_all, bbs_species, by = c("year","Gridref"))

#The null values created for surveyed grids are missing county codes so these are added again
# first by deleting the incomplete county and country and then attaching a complete version

bbs_grass_species<-bbs_grass_species%>%
  select(everything(),-c(County, Country))
county_country<-bbs_habitat_all%>%
  select(year, Gridref, County, Country)
county_country<-county_country<-unique(county_country)
bbs_grass_species<-left_join(bbs_grass_species, county_country, by=c("year", "Gridref"))
bbs_grass_species<-bbs_grass_species%>%
  select(year, Gridref, County, Country, everything())

# Exclude squares overlapping with reserves

overlapping_grids<-read.delim("C:/Users/seanj/OneDrive - University College London/GIS/lwg_bbs_overlap.txt",
                              header = TRUE, sep = ",")%>%
  select(Gridref)

bbs_grass_species<-anti_join(bbs_grass_species, overlapping_grids, by=("Gridref"))

# Exclude grids which are not Lowland (over 250m altitude)

bbs_grass_species<-bbs_grass_species%>%filter(mean_altitude<=250)

# Subset any transect count above 10 to 0, as observations above 10 will not be breeding birds according to BBS waders protocol 

bbs_grass_species<-bbs_grass_species%>%mutate(S1=ifelse(S1>10,0,S1),
                                              S2=ifelse(S2>10,0,S2),
                                              S3=ifelse(S3>10,0,S3),
                                              S4=ifelse(S4>10,0,S4),
                                              S5=ifelse(S5>10,0,S5),
                                              S6=ifelse(S6>10,0,S6),
                                              S7=ifelse(S7>10,0,S7),
                                              S8=ifelse(S8>10,0,S8),
                                              S9=ifelse(S9>10,0,S9),
                                              S10=ifelse(S10>10,0,S10))

# Attach country data to each square
county_codes<-read_excel("C:/Users/seanj/OneDrive - University College London/RSPB/Data/county_codes_bbs.xlsx", sheet = "Sheet 1")
county_codes<-county_codes%>%
  rename(Country=COUNTY)%>%
  rename(County=County)
bbs_grass_species<-left_join(bbs_grass_species, county_codes, by=c("County", "Country"))

# Function that filters the BBS data for a species, attaches that to the grid data and, summarises transects and and adds null-values
# to zero count grids, then selects the maximum annual grid count between the late and the early survey and then calculates 
# a trend based on that data


bbs_trend_creator<-function(species_name, model_type=3, autocorrelation=TRUE, overdispersion=TRUE){
  
  checker4<-bbs_grass_species%>%
    filter(species==species_name | is.na(species))%>%
    mutate(count=rowSums(select(.,S1:S10), na.rm=TRUE))%>%
    group_by(Gridref, year, species)%>%
    summarise(max_count=max(count))
  
  a<-c(1994:2018)
  years<-data.frame(year=a)
  full_data_of_grassland<-left_join(bbs_grass_all, years , by="year")
  aaa<-expand(full_data_of_grassland, Gridref, year)
  aaaa<-left_join(aaa, checker4, by=c("year", "Gridref"))
  
  try_checker<-aaaa%>%
    group_by(Gridref)%>%
    filter(sum(is.na(max_count))<24)
  
  
  bbs_model <- trim(max_count ~ Gridref + year, data=try_checker, model=model_type, serialcor = autocorrelation, overdisp = overdispersion) 
}
# function that creates a wide dataframe for imputed and observed obs respectively

imputed<-function(results) {
  
  results%>%
    select(site, time, imputed)%>%
    pivot_wider(names_from = "time",
                values_from = "imputed")
}

observed<-function(results){
  results%>%
    select(site, time, observed)%>%
    pivot_wider(names_from = "time",
                values_from = "observed")
}
# create trim, index, plot, result, imputed and observed data frame

lapwing_trim<-bbs_trend_creator(species_name="L.", autocorrelation=FALSE, model_type=3)
lapwing_bbs<-index(lapwing_trim, "both")
plot(lapwing_bbs, main="Lapwing - Model 3")
lapwing_results<-results(lapwing_trim)
lapwing_imputed<-imputed(lapwing_results)
lapwing_observed<-observed(lapwing_results)


curlew_trim<-bbs_trend_creator(species_name="CU", autocorrelation=FALSE, overdispersion=FALSE, model_type=3)
curlew_bbs<-index(curlew_trim, "both")
plot(curlew_bbs, main="Curlew - Model 3")
curlew_results<-results(curlew_trim)
curlew_imputed<-imputed(curlew_results)
curlew_observed<-observed(curlew_results)


redshank_trim<-bbs_trend_creator(species_name="RK", autocorrelation=FALSE,overdispersion=FALSE, model_type=3)
redshank_bbs<-index(redshank_trim, "both")
plot(redshank_bbs, main="Redshank - Model 3")
redshank_results<-results(redshank_trim)
redshank_imputed<-imputed(redshank_results)
redshank_observed<-observed(redshank_results)


snipe_trim<-bbs_trend_creator(species_name="SN", autocorrelation=FALSE,overdispersion=FALSE, model_type=3)
snipe_bbs<-index(snipe_trim, "both")
plot(snipe_bbs, main="Snipe - Model 3")
snipe_results<-results(snipe_trim)
snipe_imputed<-imputed(snipe_results)
snipe_observed<-observed(snipe_results)


yellow_wagtail_trim<-bbs_trend_creator(species_name="YW", autocorrelation=FALSE,overdispersion=FALSE, model_type=3)
yellow_wagtail_bbs<-index(yellow_wagtail_trim, "both")
plot(yellow_wagtail_bbs, main="Yellow Wagtail - Model 3")
yellow_wagtail_results<-results(yellow_wagtail_trim)
yellow_wagtail_imputed<-imputed(yellow_wagtail_results)
yellow_wagtail_observed<-observed(yellow_wagtail_results)

# function that attaches a species name to each index series and calculates upper and lower limits 
# in order to plot them together in ggplot 

plot_prepare<-function(data, species_name, BBS_or_reserve="Conservative counterfactual"){
  data%>%
    mutate(se_positive=imputed+se_imp)%>%
    mutate(se_negative=imputed-se_imp)%>%
    mutate(species=species_name)%>%
    mutate(trend=BBS_or_reserve)
}

lapwing_bbs_ggplot_ready<-plot_prepare(lapwing_bbs, "Lapwing")
curlew_bbs_ggplot_ready<-plot_prepare(curlew_bbs, "Curlew")
redshank_bbs_ggplot_ready<-plot_prepare(redshank_bbs, "Redshank")
snipe_bbs_ggplot_ready<-plot_prepare(snipe_bbs, "Snipe")
yellow_wagtail_bbs_ggplot_ready<-plot_prepare(yellow_wagtail_bbs, "Yellow Wagtail")

# Bind the three species together. Snipe and Yellow Wagtail are excluded as they don't have enough obs for modelling
five_bbs_species_conservative<-rbind(lapwing_bbs_ggplot_ready, curlew_bbs_ggplot_ready, redshank_bbs_ggplot_ready, snipe_bbs_ggplot_ready, yellow_wagtail_bbs_ggplot_ready)

# Script for creating reserve trends


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

lwg_species <- c("Shoveler", "Curlew", "Lapwing", "Snipe", "Black-t godwit (limosa)", "Garganey", "Yellow wagtail", "Redshank")

lwg_reserve_species<-reserve_species%>%
  filter(species %in% lwg_species)

# Homogenise main and sub site information along area and habitat before acquisition
reserve_information<-read_excel("C:/Users/seanj/OneDrive - University College London/RSPB/Data/RSPB work sheet.xlsx", sheet = "reserve_information")

lwg_reserve_species<-left_join(lwg_reserve_species, reserve_information, by = c("sub_site"))

# Drop observations without proper main and sub site - remember to look these through again with Malcolm for some of the main sites which lack categorisation 
lwg_reserve_species<-lwg_reserve_species[complete.cases(lwg_reserve_species[ , 5]),]

# rearrange columns and delete the old sub_site variable to keep only one cleaned  main site and sub site variable
lwg_reserve_species<-lwg_reserve_species%>%
  select(-c(sub_site))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, everything())

# set counts to integer format
lwg_reserve_species$count<-as.integer(lwg_reserve_species$count)
lwg_reserve_species$year<-as.integer(lwg_reserve_species$year)

# add 0.1 to all counts NOT DONE
##lwg_reserve_species<-lwg_reserve_species%>%
#  mutate(count=count+0.1)

# Create trends similar to those in the BBS trends using sub site as site variable

# reserve_trend_creator<-function(data, species_name, autocorrelation=TRUE, overdispersion=TRUE){}



# plotting reserve trends (I should create a function for this)
curlew_lwg<-lwg_reserve_species%>%
  filter(species=="Curlew")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annualcurlew<-trim(count~site+year, data=curlew_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_curlew<-index(annualcurlew, "both")
plot(index_curlew, main="Reserve Curlew - Model 3")


lapwing_lwg<-lwg_reserve_species%>%
  filter(species=="Lapwing")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annuallapwing<-trim(count~site+year, data=lapwing_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_lapwing<-index(annuallapwing, "both")
plot(index_lapwing, main="Reserve Lapwing - Model 3")

redshank_lwg<-lwg_reserve_species%>%
  filter(species=="Redshank")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annualredshank<-trim(count~site+year, data=redshank_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_redshank<-index(annualredshank, "both")
plot(index_redshank, main="Reserve Redshank - Model 3")

snipe_lwg<-lwg_reserve_species%>%
  filter(species=="Snipe")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annualsnipe<-trim(count~site+year, data=snipe_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_snipe<-index(annualsnipe, "both")
plot(index_snipe, main="Reserve Snipe - Model 3")

yellow_wagtail_lwg<-lwg_reserve_species%>%
  filter(species=="Yellow wagtail")%>%
  group_by(sub_site, year)%>%
  rename(site=sub_site)
annualwagtail<-trim(count~site+year, data=yellow_wagtail_lwg, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_yellow_wagtail<-index(annualwagtail, "both")
plot(index_yellow_wagtail, main="Reserve Yellow Wagtail - Model 3")

# Prepare for ggplot
lapwing_reserve_ggplot_ready<-plot_prepare(index_lapwing, "Lapwing", BBS_or_reserve = "Reserve")
curlew_reserve_ggplot_ready<-plot_prepare(index_curlew, "Curlew", BBS_or_reserve = "Reserve")
redshank_reserve_ggplot_ready<-plot_prepare(index_redshank, "Redshank", BBS_or_reserve = "Reserve")
snipe_reserve_ggplot_ready<-plot_prepare(index_snipe, "Snipe", BBS_or_reserve = "Reserve")
yellow_wagtail_reserve_ggplot_ready<-plot_prepare(index_yellow_wagtail, "Yellow Wagtail", BBS_or_reserve = "Reserve")

five_reserve_species<-rbind(lapwing_reserve_ggplot_ready, curlew_reserve_ggplot_ready, redshank_reserve_ggplot_ready, 
                            snipe_reserve_ggplot_ready, yellow_wagtail_reserve_ggplot_ready)



# Combine and plot reserve and bbs trends for all counterfactual combinations
five_species_combined<-rbind(five_reserve_species, five_bbs_species_conservative)

plot_five_species_combined<-ggplot(data=five_species_combined, aes(x=time, y=imputed, colour=trend)) +
  ylab("Index - 1994 = 1")+xlab("Time")+
  geom_line(linetype = 2)+
  geom_hline(yintercept = 1, linetype=2)+facet_wrap(~species, scales="free")+
  theme_classic()+scale_x_continuous(name = "Time", limits=c(1994,2018), breaks = seq(1994,2019, by = 6))+
  theme(strip.text = element_text(size=20), legend.title = element_blank())+geom_smooth(se = FALSE, size = 1)+
  theme(legend.position = c(0.85,0.3), legend.text = element_text(size = 30), legend.key.size = unit(2, "cm"),
        axis.title=element_text(size=20,face="bold"))


plot_five_species_combined

ggsave(filename = "C:/Users/seanj/OneDrive/Skrivebord/Plots and graphs/UK_trends_conservative.png", 
       plot = plot_five_species_combined, width = 42, height = 20, dpi = 1000, units = "cm")

## Welch Two Sample t-test
t.test(index_lapwing$imputed, lapwing_bbs$imputed)
t.test(index_redshank$imputed, redshank_bbs$imputed)
t.test(index_snipe$imputed, snipe_bbs$imputed)
t.test(index_yellow_wagtail$imputed, yellow_wagtail_bbs$imputed)
t.test(index_curlew$imputed, curlew_bbs$imputed)

## independent 2-group Mann-Whitney U Test
wilcox.test(index_lapwing$imputed, lapwing_bbs$imputed)
wilcox.test(index_redshank$imputed, redshank_bbs$imputed)
wilcox.test(index_snipe$imputed, snipe_bbs$imputed)
wilcox.test(index_yellow_wagtail$imputed, yellow_wagtail_bbs$imputed)
wilcox.test(index_curlew$imputed, curlew_bbs$imputed)

## Differences between trend values
difference_in_trends<-function(index_reserve, bird_bbs, name){
  index_reserve<-index_reserve$imputed-bird_bbs$imputed
  a<-c(1994:2018)
  years<-data.frame(year=a)
  index_reserve<-cbind(index_reserve, years)
  index_reserve<-index_reserve%>%
    mutate(species=name)%>%
    rename(difference=1)
}

lapwing_differences<-difference_in_trends(index_lapwing, lapwing_bbs, name = "Lapwing")
curlew_differences<-difference_in_trends(index_curlew, curlew_bbs, name = "Curlew")
redshank_differences<-difference_in_trends(index_redshank, redshank_bbs, name = "Redshank")
yellow_wagtail_differences<-difference_in_trends(index_yellow_wagtail, yellow_wagtail_bbs, name = "Yellow Wagtail")
snipe_differences<-difference_in_trends(index_snipe, snipe_bbs, name = "Snipe")
differences<-rbind(lapwing_differences, curlew_differences, redshank_differences)

# boxplot the index values per species by reserve vs counterfactual 
ggplot(five_species_combined, aes( x = species, y = imputed, colour = trend))+
  geom_boxplot()+theme_bw()+labs(x = "",y = "Index")

## Kruskal-Wallis Test

kruskal<-function(bird_species){
  data<-five_species_combined%>%
    filter(species==bird_species)
  data$trend<-as.factor(data$trend)  
  
  kruskal.test(imputed ~ trend, data = data)
}
kruskal("Lapwing")
kruskal("Curlew")
kruskal("Yellow Wagtail")
kruskal("Snipe")
kruskal("Redshank")

# Plotting the different counterfactuals vs reserve trends to illustrate the effect of covariates

comparison_of_counterfactuals<-bind_rows(five_bbs_species, five_bbs_species_conservative, five_bbs_species_liberal, five_reserve_species)

# Combine and plot reserve and bbs trends for all counterfactual combinations

plot_five_species_combined<-ggplot(data=comparison_of_counterfactuals, aes(x=time, y=imputed, colour=trend)) +
  ylab("Index - 1994 = 1")+xlab("Time")+
  geom_line(linetype = 2)+
  geom_hline(yintercept = 1, linetype=2)+facet_wrap(~species, scales="free")+
  theme_classic()+scale_x_continuous(name = "Time", limits=c(1994,2018), breaks = seq(1994,2019, by = 6))+
  theme(strip.text = element_text(size=20), legend.title = element_blank())+geom_smooth(se = FALSE, size = 1)+
  theme(legend.position = c(0.85,0.3), legend.text = element_text(size = 30), legend.key.size = unit(2, "cm"),
        axis.title=element_text(size=20,face="bold"), axis.text=element_text(size=20))



plot_five_species_combined

# First - liberal vs counterfactual
comparison_of_counterfactuals_liberal<-comparison_of_counterfactuals%>%
  filter(trend == "Liberal counterfactual" | trend == "Counterfactual")%>%
  droplevels()

  plot_comparison_of_counterfactuals_liberal<-comparison_of_counterfactuals_liberal%>%
    ggplot(., aes(x=time, y=imputed, colour=trend))+ 
  geom_ribbon(aes(ymin=comparison_of_counterfactuals_liberal$se_negative, 
                  ymax=comparison_of_counterfactuals_liberal$se_positive), 
              fill = "grey70", linetype=3, alpha=0.1)+ylab("Index - 1994 = 1")+xlab("Time")+
  geom_line(linetype = 2)+
  geom_hline(yintercept = 1, linetype=2)+facet_wrap(~species, scales="free")+
  theme_classic()+scale_x_continuous(name = "Time", limits=c(1994,2018), breaks = seq(1994,2019, by = 3))+
  theme(strip.text = element_text(size=20), legend.title = element_blank())+geom_smooth(se = FALSE, size = 1)+
  theme(legend.position = c(0.85,0.30), legend.text = element_text(size = 30), legend.key.size = unit(3, "cm"),
        axis.title.y=element_text(size=14,face="bold"))+facet_wrap(~species, scales = "free")

plot_comparison_of_counterfactuals_liberal

# Then, Conservative vs counterfactual
comparison_of_counterfactuals_conservative<-comparison_of_counterfactuals%>%
  filter(trend == "Conservative counterfactual" | trend == "Counterfactual")%>%
  droplevels()

plot_comparison_of_counterfactuals_conservative<-comparison_of_counterfactuals_conservative%>%
  ggplot(., aes(x=time, y=imputed, colour=trend))+ 
  geom_ribbon(aes(ymin=comparison_of_counterfactuals_conservative$se_negative, 
                  ymax=comparison_of_counterfactuals_conservative$se_positive), 
              fill = "grey70", linetype=3, alpha=0.1)+ylab("Index - 1994 = 1")+xlab("Time")+
  geom_line(linetype = 2)+
  geom_hline(yintercept = 1, linetype=2)+facet_wrap(~species, scales="free")+
  theme_classic()+scale_x_continuous(name = "Time", limits=c(1994,2018), breaks = seq(1994,2019, by = 3))+
  theme(strip.text = element_text(size=20), legend.title = element_blank())+geom_smooth(se = FALSE, size = 1)+
  theme(legend.position = c(0.85,0.30), legend.text = element_text(size = 30), legend.key.size = unit(3, "cm"),
        axis.title.y=element_text(size=14,face="bold"))+facet_wrap(~species, scales = "free")

plot_comparison_of_counterfactuals_conservative

# Third, plot the two approaches against each other
comparison_of_counterfactuals_con_vs_lib<-comparison_of_counterfactuals%>%
  filter(trend == "Conservative counterfactual" | trend == "Liberal counterfactual")%>%
  droplevels()

plot_comparison_of_counterfactuals_con_vs_lib<-comparison_of_counterfactuals_con_vs_lib%>%
  ggplot(., aes(x=time, y=imputed, colour=trend))+ 
  geom_ribbon(aes(ymin=comparison_of_counterfactuals_con_vs_lib$se_negative, 
                  ymax=comparison_of_counterfactuals_con_vs_lib$se_positive), 
              fill = "grey70", linetype=3, alpha=0.1)+ylab("Index - 1994 = 1")+xlab("Time")+
  geom_line(linetype = 2)+
  geom_hline(yintercept = 1, linetype=2)+facet_wrap(~species, scales="free")+
  theme_classic()+scale_x_continuous(name = "Time", limits=c(1994,2018), breaks = seq(1994,2019, by = 3))+
  theme(strip.text = element_text(size=20), legend.title = element_blank())+geom_smooth(se = FALSE, size = 1)+
  theme(legend.position = c(0.85,0.30), legend.text = element_text(size = 25), legend.key.size = unit(3, "cm"),
        axis.title.y=element_text(size=14,face="bold"))+facet_wrap(~species, scales = "free")

plot_comparison_of_counterfactuals_con_vs_lib
