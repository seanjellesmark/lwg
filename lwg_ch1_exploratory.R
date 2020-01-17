test<-bbs_species%>%pivot_longer (cols = S1:S10,
                                 names_to = "Transect",
                                 values_to = "Count")
test$Transect<-as.numeric(gsub("S", "", test$Transect))

test1<-left_join(bbs_habitat_all, test, by = c("year","Gridref","Transect"))                                 
test2<-test2%>%
  filter(PLevel1=="C" & PLevel2 == 6 | SLevel1=="C" & SLevel2 == 6)

test3<-test3%>%
  filter(is.na(RSPBOverlap))
                                 
test4<-test3%>%
  group_by(Gridref, year, EarlyLate, species)%>%
  summarise(EandL_max_count = sum(Count, na.rm = TRUE))

test5<-test4%>%group_by(Gridref, year, species)%>%
  summarise(max_count = max(EandL_max_count))

test_bbs_lapwing<-test5%>%
  filter(species =="L." | is.na(species))

test_lapwing_trim<-trim(max_count~Gridref+year, data=test_bbs_lapwing, model=3, serialcor=TRUE, overdisp=TRUE)
test_lapwing_index<-index(test_lapwing_trim)
plot(test_lapwing_index)
test_results_lapwing<-results(test_lapwing_trim)

# for Redshank
test_bbs_redshank<-test5%>%
  filter(species =="RK" | is.na(species))

test_redshank_trim<-trim(max_count~Gridref+year, data=test_bbs_redshank, model=3, serialcor=TRUE, overdisp=TRUE)
test_redshank_index<-index(test_redshank_trim)
plot(test_redshank_index)
test_results_redshank<-results(test_redshank_trim)
