#### models for agents of empire
### created 1/27/22
### modified 2/11/22

### packages
library(tidyverse)
library(lme4)
library(brms)
library(modelr)
library(tidybayes)
library(ggdist)
library(ggridges)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

### data
### NATL PRISON STATS
nps<-read_csv("./data/state_aian_incar.csv")
### NHGIS CENSUS 2010
census<-read_csv("./data/aianh_pop_census2010.csv")
### fill in zeroes
fe<-read_csv("./data/fe_state_imputed_1_26_22.csv")  %>% 
  complete(.imp, state, year, race_ethn, 
           fill = list(fe_deaths = 0))
#### compute as period mean (00-19)
### avg deaths / year
fe<-fe %>% 
  group_by(.imp, state, race_ethn) %>% 
  summarize(fe_deaths = mean(fe_deaths)) %>% 
  ungroup() %>% 
  rename(fe_imp = .imp)

fc<-read_csv("./data/fc_st.csv") 

pop<-read_csv("./data/pop_st.csv") %>% 
  filter(year>1999) %>% 
  filter(state!="KR") %>% 
  pivot_wider(names_from = child,
              values_from = pop,
              names_prefix = "pop_")

# ### aianh infrastructure data
# inst <- readRDS("./data/state_dat.rds")
# inst<-inst %>% 
#   select(stusps, aianh_pct_tribal_courts, 
#          st_income_ineq) %>% 
#   rename(state = stusps) %>% 
#   distinct()

### join for all available years (onto pop)
### one imputation for now
dat<-fc %>% 
  left_join(fe) %>% 
  left_join(nps) %>% 
  left_join(pop) %>%  
  filter(race_ethn=="AIAN") %>% 
  left_join(census) %>% 
  filter(state!="DC") %>% 
  mutate(fe_deaths_rt_100k = fe_deaths / pop_adult * 1e5,
         aian_prison_rt_1k = aian_prison_pop / pop_adult * 1e3) %>% 
  mutate(imp = paste(.imp, fe_imp, sep = "_")) %>% 
  select(-.imp, -fe_imp) %>% 
  select(imp, everything())

### convert to list for brm_multiple
dat_l<-list()
imps<-unique(dat$imp)
for(i in 1:length(imps)){
  dat_l[[i]]<-dat %>% 
    filter(imp == imps[i])
}

#### QUESTION 0: CORRELATION OF VIOLENCE ACROSS DOMAINS
q0_m1<-brm_multiple(fc_total_contact ~ 
                      scale(aian_prison_rt_1k) + 
                      scale(fe_deaths_rt_100k) + 
                      offset(log(pop_child)) + 
                      (1|state),
                    data = dat_l,
                    iter = 1e4,
                    family = negbinomial())

saveRDS(q0_m1, file = "./models/q0_m1.rds")


#### THIS WORKS - CONVERT TO Z SCORE FOR JOINT PLOT

# q0_m1f<-glmer.nb(fc_total_contact ~ 
#                       scale(I(aian_prison_pop / pop_adult)) + 
#                       offset(log(pop_child)) + 
#                       (1|state),
#                     data = dat_l[[1]])
### first 2 models give same results brms or lme4

### yep, clear correlation

#### QUESTION 1: LAND CONTESTATION

library(MASS)
select<-dplyr::select

#### THINK ABT CORRELATION AIANH WITH AIAN pop PCT
### MAYBE BE SPECIFIC TO URBAN / NOT URBAN? Relocation is the big issue here
# qplot(pct_AIAN_2010, pct_AIANH_2010, data = dat)
# dat %>% 
#   filter(year==2019) %>% 
#   summarize(corPcts = cor(pct_AIAN_2010, pct_AIANH_2010))
### 0.8 correlation, can't include both...

m0_land_AIANH<-glm.nb(fc_total_contact ~ 
                    scale(pct_AIANH_2010) + 
                  offset(log(pop_child)),
                  data = dat %>% 
                    filter(year == 2019))

m0_pct_AIAN<-glm.nb(fc_total_contact ~ 
                        scale(pct_AIAN_2010) + 
                        offset(log(pop_child)),
                      data = dat %>% 
                        filter(year == 2019))

### AIC IS WITHIN 1 POINT

#### QUESTION 2: TIMING OF STATE ADMISSION AND POLICY ERA
### state admission 
### model with FC to start

m0_admit<-glmer.nb(fc_total_contact ~ 
                scale(admission_year) + 
                  (1|state),
                data = dat %>% 
                  filter(race_ethn=="AIAN"),
                offset = log(pop_child))

#### baseline is there for linear time
### now add temporal period following T's timeline
### T's periodization
## <1820: 1. Treaties, trade and intercourse period
## 1820-1880: 2. Removal and reservation period
## 1880-1920: 3. Allotment and Assimilation
## 1920-1940: 4. Indian New Deal 
## 1940-1960: 5. Termination period
## 1960-Now: 6. Self determination

dat<-dat %>% 
  mutate(indian_policy_era = 
           case_when(
             admission_year<1820 ~ "1. Treaties, trade and intercourse period",
             admission_year<1881 ~ "2. Removal and reservation period",
             admission_year<1921 ~ "3. Allotment and Assimilation",
             admission_year<1941 ~ "4. Indian New Deal",
             admission_year<1961 ~ "5. Termination period",
             admission_year>1960 ~ "6. Self determnation period"
           )) 

m1_admit<-glmer.nb(fc_total_contact ~ 
                     #scale(admission_year) + 
                     indian_policy_era + 
                     (1|state),
                   data = dat %>% 
                     filter(race_ethn=="AIAN"),
                   offset = log(pop_child))

### clear pattern here, rates increase over time, highest in termination era admits
### but that's only AK and HI

### QUESTION 3: RESTRICTIONS ON SOVEREIGN POWER VIA LAW
m0_pl280<-glmer.nb(fc_total_contact ~ 
                     pl280 + 
                     (1|state),
                   data = dat %>% 
                     filter(race_ethn=="AIAN"),
                   offset = log(pop_child))

### clear pl280 relationship, condition on timing of entry?
m1_pl280<-glmer.nb(fc_total_contact ~ 
                     pl280 + 
                     indian_policy_era + 
                     (1|state),
                   data = dat %>% 
                     filter(race_ethn=="AIAN"),
                   offset = log(pop_child))
### yep, joint relationship is there

### QUESTION 5: JOINT MODELS?
### anticipate 'control' critique for pop distribution
### fold it into narrative of settler colonialism as structure

