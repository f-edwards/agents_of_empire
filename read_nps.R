### read national prisoner stats - 2020 for agents of empire

library(tidyverse)

nps<-read_tsv("./data/ICPSR_38249/DS0001/38249-0001-Data.tsv")
### subset to total under jurisdiction and AIAN under jurisdiction
nps<-nps %>% 
  select(YEAR, STATE, 
         JURTOTM, JURTOTF,
         AIANM, AIANF) %>% 
  filter(YEAR>=2000) %>% 
  mutate(JURTOTM = ifelse(JURTOTM<0, NA, JURTOTM),
         JURTOTF = ifelse(JURTOTF<0, NA, JURTOTF),
         AIANM = ifelse(AIANM<0, NA, AIANM),
         AIANF = ifelse(AIANF<0, NA, AIANF))

### aggregate across sex
nps<-nps %>% 
  mutate(total_prison_pop = JURTOTM + JURTOTF,
         aian_prison_pop = AIANM + AIANF) %>% 
  rename_all(tolower) %>% 
  select(year, state, total_prison_pop, aian_prison_pop) %>% 
  mutate(aian_prison_pop_pct = aian_prison_pop / total_prison_pop * 100)
  

