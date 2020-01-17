test<-bbs_grass_species%>%pivot_longer (cols = S1:S10,
                                 names_to = "Transect",
                                 values_to = "Count")
test$Transect<-as.numeric(gsub("S", "", test$Transect))
                                 
                                 
                                 

                                 
                                 
test1<-test%>%
  group_by(Gridref, year, EarlyLate, species)%>%
  summarise(max_count = sum(Count, na.rm = TRUE)
            
                                                                      