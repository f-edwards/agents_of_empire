### read national prisoner stats - 2020 for agents of empire

library(tidyverse)
library(brms)

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
  select(year, state, aian_prison_pop) 

### NPS DATA QUALITY EDA
# nps %>% 
#   ggplot(aes(x = year, y = aian_prison_pop)) + 
#   geom_line() + 
#   geom_point() + 
#   facet_wrap(~state, scales = "free")

### suspect observations
### AK 2013; GA 2006; NM 2012; NV 2008, 2009;
### TX 2000 - 2005
### treat as missing and impute
nps_error<-data.frame(state = 
                        c("AK", "GA", "NM",
                          "NV", "NV", rep("TX", 6)),
                      year = c(2013, 2006, 2012, 2008, 2009, 
                               2000:2005),
                      error = T)

nps<-nps %>% 
  left_join(nps_error) %>% 
  mutate(aian_prison_pop = ifelse(!(is.na(error)),
                                  NA,
                                  aian_prison_pop))

### incar trends
vera<-read_csv("./data/incarceration_trends.csv")

# vera %>% 
#   filter(year>1999) %>% 
#   group_by(state, year) %>% 
#   summarize(jail_aian = sum(native_jail_pop, na.rm=T)) %>% 
#   ggplot(aes(x = year, y = jail_aian)) + 
#   geom_line() + 
#   geom_point() + 
#   facet_wrap(~state, scales = "free")

## actually looks good!
vera <- vera %>% 
  filter(year>1999) %>% 
  group_by(state, year) %>% 
  summarize(aian_jail_pop = sum(native_jail_pop, na.rm=T)) 

### vera has complete state/year from 00-18
incar<-vera %>% 
  left_join(nps) %>% 
  filter(state!="DC") %>% 
  select(-error)

write_csv(incar, "./data/state_aian_incar.csv")

