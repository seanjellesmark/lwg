test<-bbs_species%>%pivot_longer (cols = S1:S10,
                                 names_to = "Transect",
                                 values_to = "Count")
test$Transect<-as.numeric(gsub("S", "", test$Transect))

bbs_wet_grasslands<-bbs_habitat_all%>%
  filter(PLevel1=="C" & PLevel2 == 5 | SLevel1=="C" & SLevel2 == 5 | PLevel1=="C" & PLevel2 == 6 | SLevel1=="C" & SLevel2 == 6 | PLevel1=="C" & PLevel2 == 7 | SLevel1=="C" & SLevel2 == 7 | PLevel1=="C" & PLevel2 == 8 | SLevel1=="C" & SLevel2 == 8)

test11<-left_join(bbs_wet_grasslands, test, by = c("year","Gridref","Transect"))                                 

test111<-test11%>%
  filter(PLevel1 == "C" | SLevel1 == "C")%>%
  filter(is.na(RSPBOverlap))

test1111<-test111%>%
  mutate(species=ifelse(is.na(species),"Monitored",species))

test11111<-test1111%>%
  group_by(Gridref, year, EarlyLate, species)%>%
  summarise(Grid_count = sum(Count))%>%
  group_by(Gridref, year, species)%>%
  summarise(Grid_count = max(Grid_count))

test111111<-test11111%>%
  mutate(Grid_count = ifelse(is.na(Grid_count),0,Grid_count))

test1111111<-test111111%>%
  filter(species == "L." | species == "Monitored")
test_cons_lapwing<-trim(Grid_count~Gridref+year, data=test1111111, model=3, serialcor=TRUE, overdisp=TRUE)                                                                       
index_cons_lapwing<-index(test_cons_lapwing, "both")




test2<-test1%>%
  filter(is.na(RSPBOverlap))
                                 
test3<-test2%>%
  group_by(Gridref, year, EarlyLate, species)%>%
  summarise(EandL_max_count = sum(Count, na.rm = TRUE))

test4<-test3%>%group_by(Gridref, year, species)%>%
  summarise(max_count = max(EandL_max_count))

test_bbs_lapwing<-test4%>%
  filter(species =="L." | is.na(species))

test_lapwing_trim<-trim(max_count~Gridref+year, data=test_bbs_lapwing, model=3, serialcor=TRUE, overdisp=TRUE)
test_lapwing_index<-index(test_lapwing_trim)
plot(test_lapwing_index)
test_results_lapwing<-results(test_lapwing_trim)
imputed_test_lapwing<-imputed(test_results_lapwing)
observed_test_lapwing<-observed(test_results_lapwing)
# for Redshank
test_bbs_redshank<-test5%>%
  filter(species =="RK" | is.na(species))

test_redshank_trim<-trim(max_count~Gridref+year, data=test_bbs_redshank, model=3, serialcor=TRUE, overdisp=TRUE)
test_redshank_index<-index(test_redshank_trim)
plot(test_redshank_index)
test_results_redshank<-results(test_redshank_trim)

# difference between the grids
setdiff(lapwing_trim[["site_id"]], test_lapwing_trim[["site_id"]])
setdiff(test_lapwing_trim[["site_id"]], lapwing_trim[["site_id"]])


## Producing the excel sheet with all the bbs grids each species is present in.
arc_gis<-bbs_grass_species%>%
  filter(!is.na(species))%>%
  distinct(Gridref)%>%
  mutate(presence = 1)
write.csv(arc_gis, "C:/Users/seanj/OneDrive - University College London/RSPB/Data/BBS_Grids_for_arcgis.csv")

## Producing the excel sheet with all the bbs grids snipe is present in.
arc_gis_snipe<-bbs_grass_species%>%
  filter(species == "SN")%>%
  distinct(Gridref)%>%
  mutate(snipe_presence = 1)
write.csv(arc_gis_snipe, "C:/Users/seanj/OneDrive - University College London/RSPB/Data/Snipe_Grids_for_arcgis.csv")


# read txt file with overlapping arcgis grids
overlapping_grids<-read.delim("C:/Users/seanj/OneDrive - University College London/GIS/lwg_bbs_overlap.txt",
                              header = TRUE, sep = ",")%>%
  select(Gridref)

# with 6 km buffer

overlapping_grids_6buffer<-read.delim("C:/Users/seanj/OneDrive - University College London/GIS/buffer_6km.txt",
header = TRUE, sep = ",")%>%
  select(Gridref)

# All reserves 
overlapping_grids_all_reserves<-read.delim("C:/Users/seanj/OneDrive - University College London/GIS/all_reserves_bbs_overlap.txt",
                                      header = TRUE, sep = ",")%>%
  select(Gridref)



## Testing whether keeping only the reserves which were acquired in 1994 makes a difference on the trend (needs the benchmark counterfactual to run) ----

# plotting reserve trends (I should create a function for this)
curlew_lwg<-lwg_reserve_species%>%
  filter(species=="Curlew")%>%
  filter(year_of_acquisition == "before 1994")%>%
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
  filter(year_of_acquisition == "before 1994")%>%
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
  filter(year_of_acquisition == "before 1994")%>%
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
  filter(year_of_acquisition == "before 1994")%>%
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
  filter(year_of_acquisition = 1994)%>%
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

plot_five_species_combined<-ggplot(data=five_species_combined, aes(x=time, y=imputed, colour=trend, fill = trend, linetype = trend)) + 
  geom_ribbon(aes(ymin=five_species_combined$se_negative, 
                  ymax=five_species_combined$se_positive), 
              linetype=3, alpha=0.3)+ylab("Index - 1994 = 1")+xlab("Time")+
  geom_line(size = 1.2)+
  geom_hline(yintercept = 1, linetype=2)+facet_wrap(~species, scales="free")+
  theme_classic()+scale_x_continuous(name = "Time", limits=c(1994,2018), breaks = seq(1994,2019, by = 6))+
  theme(strip.text = element_text(size=20), legend.title = element_blank())+
  theme(legend.position = c(0.8,0.35), legend.text = element_text(size = 25), legend.key.size = unit(2, "cm"),
        axis.title=element_text(size=20,face="bold"), axis.text=element_text(size=20))
plot_five_species_combined

## Testing whether excluding Ouse Washes makes a difference on the trend (needs the benchmark counterfactual to run) ----
# Note I have already created a reserve_vs_Ouse file for plotting which I wrote directly in the console and is now gone because I'm an idiot
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

# Reserve trend without Ouse Washes. Curlew not included because of no pairs
lapwing_lwg<-lwg_reserve_species%>%
  filter(species=="Lapwing")%>%
  filter(main_site != "Ouse Washes")%>%
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
  filter(main_site != "Ouse Washes")%>%
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
  filter(main_site != "Ouse Washes")%>%
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
  filter(main_site != "Ouse Washes")%>%
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
redshank_reserve_ggplot_ready<-plot_prepare(index_redshank, "Redshank", BBS_or_reserve = "Reserve")
snipe_reserve_ggplot_ready<-plot_prepare(index_snipe, "Snipe", BBS_or_reserve = "Reserve")
yellow_wagtail_reserve_ggplot_ready<-plot_prepare(index_yellow_wagtail, "Yellow wagtail", BBS_or_reserve = "Reserve")

# Name this one Ouse and change the name of the trend
five_reserve_species_ouse<-rbind(lapwing_reserve_ggplot_ready, redshank_reserve_ggplot_ready, 
                                 snipe_reserve_ggplot_ready, yellow_wagtail_reserve_ggplot_ready)
five_reserve_species_ouse<-five_reserve_species_ouse%>%
  mutate(trend = "Without Ouse Washes")

# Create the Benchmark trend for comparison trend

# plotting reserve trends (I should create a function for this)

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
redshank_reserve_ggplot_ready<-plot_prepare(index_redshank, "Redshank", BBS_or_reserve = "Reserve")
snipe_reserve_ggplot_ready<-plot_prepare(index_snipe, "Snipe", BBS_or_reserve = "Reserve")
yellow_wagtail_reserve_ggplot_ready<-plot_prepare(index_yellow_wagtail, "Yellow wagtail", BBS_or_reserve = "Reserve")

five_reserve_species<-rbind(lapwing_reserve_ggplot_ready, redshank_reserve_ggplot_ready, 
                            snipe_reserve_ggplot_ready, yellow_wagtail_reserve_ggplot_ready)
four_bbs_species<-five_bbs_species %>% 
  filter(species != "Curlew")
reserve_vs_Ouse<-rbind(five_reserve_species_ouse, five_reserve_species, four_bbs_species)
reserve_vs_Ouse<-reserve_vs_Ouse%>%
  rename(`Yellow wagtail`=`Yellow Wagtail`)

# adjust data to only include Yellow wagtail and snipe, as they are the only one's different from their normal reserve trends

plot_five_species_ouse<-ggplot(data=reserve_vs_Ouse, aes(x=time, y=imputed, colour=trend, fill = trend, linetype = trend)) + 
  geom_ribbon(aes(ymin=reserve_vs_Ouse$se_negative, 
                  ymax=reserve_vs_Ouse$se_positive), 
              linetype=3, alpha=0.3)+ylab("Index - 1994 = 1")+xlab("Time")+
  geom_line(size = 1.2)+
  geom_hline(yintercept = 1, linetype=2)+facet_wrap(~species, scales="free")+
  theme_classic()+scale_x_continuous(name = "Time", limits=c(1994,2018), breaks = seq(1994,2019, by = 6))+
  theme(strip.text = element_text(size=20), legend.title = element_blank())+
  theme(legend.position = "bottom", legend.text = element_text(size = 25), legend.key.size = unit(2, "cm"),
        axis.title=element_text(size=20,face="bold"), axis.text=element_text(size=20))+
  scale_color_viridis_d(option = "D", begin = 0, end = 0.6, aesthetics = c("colour","fill"))+
  scale_linetype_manual(values = c("Reserve" = "solid", "Without Ouse Washes" = "twodash", "Benchmark \ncounterfactual" = "dotted"))
plot_five_species_ouse

# ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Plots and graphs/without_ouse_washes.png", 
#      plot = plot_five_species_ouse, width = 40, height = 20, dpi = 600, units = "cm")

# t.test the trends with and without the Ouse Washes
t.test(index_curlew1$imputed, index_curlew$imputed)
t.test(index_snipe1$imputed, index_snipe$imputed) 
t.test(index_lapwing1$imputed, index_lapwing$imputed)
t.test(index_yellow_wagtail1$imputed, index_yellow_wagtail$imputed)
t.test(index_redshank1$imputed, index_redshank$imputed)

# Count the number of observation used annually -----------------
observed_sites<-function(results){
  results%>%
    select(site, time, observed)%>%
    filter(!is.na(observed)) %>% 
    group_by(time) %>% 
    summarise(n_sites = n())
}
n_yearly_obs<-observed_sites(lapwing_results)

## Check the effect of using every observation from reserves which at some point contained grassland. Here using Lapwing
tester_lapwing<-bbs_species %>% 
  filter(species == "L.")
tester_lapwing <- tester_lapwing %>% 
  mutate(count_sum = rowSums(select(., S1:S10), na.rm = TRUE))
tester_lapwing<-tester_lapwing %>%
  group_by(year, Gridref) %>% 
  summarise(max_count = max(count_sum))

tester_lapwing_wide<-tester_lapwing %>% 
  pivot_wider(values_from = "max_count",
              names_from = "year")
