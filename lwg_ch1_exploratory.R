test<-bbs_grass_species%>%pivot_longer(cols = S1:S10,
                                 names_to = "Transect",
                                 values_to = "Count",
                                 names_prefix = gsub("[a-zA-Z ]", "")

                                 
                                 
test1<-test%>%
  group_by(Gridref, year, EarlyLate, species)%>%
  summarise(counts = sum(Count, na.rm = TRUE)
            
                                                                      