library(dplyr)
library(openxlsx)
# import worldpop 2025 population
# pop25<-read.xlsx("../Resources/OUTPUT/population_1_year_lga_nigeria_2025_TD.xlsx") %>%  
pop25<-read.xlsx("../Resources/OUTPUT/2018_2024_POPULATION_ESTIMATES/2024_V2_population_1_year_lga_nigeria_g3_clean.xlsx") %>%  
  select(-mtotal,-ftotal,-ttotal) %>% 
  mutate(mtot=rowSums(select(.,starts_with("m"))),
         ftot=rowSums(select(.,starts_with("f")))
         ) %>% 
  mutate(
    m0=m0,
    f0=f0
  ) %>% 
  select(statename,m0,f0,mtot,ftot) %>% group_by(statename) %>% 
  reframe(m0=sum(m0),f0=sum(f0),
          mtot=sum(mtot),ftot=sum(ftot)) %>% 
  mutate(t0=m0+f0,
         ttot=mtot+ftot)

tmp<-pop25[,c("m0","f0","t0","mtot","ftot","ttot")]%>%
  summarise_all(sum) %>% mutate(statename="National")

pop25<-pop25 %>% bind_rows(tmp) %>% 
  filter(statename %in% c("Kogi","Bayelsa","Kaduna","Lagos","National")) 
# write.xlsx(pop25,"wp2025_0yo.xlsx")

#####################################
# Calculate birth rates
cbr<-pop25 %>% select(statename,t0,ttot) %>% mutate(Births=t0/0.9581,
                                                    CBR=Births/ttot*1000
                                                    )

cbr %>% View()




  
