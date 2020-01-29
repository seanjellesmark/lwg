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
