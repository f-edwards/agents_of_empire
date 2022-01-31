#### models for agents of empire
### created 1/27/22
### modified 1/31/22

### packages
library(tidyverse)
library(lme4)
library(brms)

### data
source("read_nps.R")
### fill in zeroes
fe<-read_csv("./data/fe_state_imputed_1_26_22.csv")  %>% 
  complete(.imp, state, year, race_ethn, 
           fill = list(fe_deaths = 0))

fc<-read_csv("./data/fc_st.csv") 

pop<-read_csv("./data/pop_st.csv") %>% 
  filter(year>1999) %>% 
  filter(state!="KR") %>% 
  pivot_wider(names_from = child,
              values_from = pop,
              names_prefix = "pop_")

### join for all available years (onto pop)
### one imputation for now
dat<-pop %>% 
  left_join(fc %>% 
              filter(.imp==1) %>% 
              select(-.imp)) %>% 
  left_join(fe %>% 
              filter(.imp==1) %>% 
              select(-.imp)) %>% 
  left_join(nps) %>% 
  filter(year>=2005) %>% 
  filter(race_ethn=="AIAN")

#### QUESTION 0: CORRELATION OF VIOLENCE ACROSS DOMAINS
m0_entangled<-glmer.nb(fc_total_contact ~ 
                         scale(I(aian_prison_pop / pop_adult)) + 
                         (1|state),
                       data = dat,
                       offset = log(pop_child))

#### QUESTION 1: LAND CONTESTATION




#### QUESTION 2: TIMING OF STATE ADMISSION AND POLICY ERA
### state admission 
### model with FC to start

m0_admit<-glmer.nb(floor(fc_entries) ~ 
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
             admission_year>1960 ~ "6. Selft determnation period"
           )) 

m1_admit<-glmer.nb(floor(fc_entries_mn) ~ 
                     #scale(admission_year) + 
                     indian_policy_era + 
                     (1|state),
                   data = dat %>% 
                     filter(race_ethn=="AIAN"),
                   offset = log(pop_child))

### clear pattern here, rates increase over time, highest in termination era admits
### but that's only AK and HI

### QUESTION 3: RESTRICTIONS ON SOVEREIGN POWER VIA LAW
m0_pl280<-glmer.nb(floor(fc_entries_mn) ~ 
                     pl280 + 
                     (1|state),
                   data = dat %>% 
                     filter(race_ethn=="AIAN"),
                   offset = log(pop_child))

### clear pl280 relationship, condition on timing of entry?
m1_pl280<-glmer.nb(floor(fc_entries_mn) ~ 
                     pl280 + 
                     indian_policy_era + 
                     (1|state),
                   data = dat %>% 
                     filter(race_ethn=="AIAN"),
                   offset = log(pop_child))
### yep, joint relationship is there

### QUESTION 4: HIGH INSTITUTIONAL CAPACITY, SOVEREIGN SHIELDS

### QUESTION 6: JOINT MODELS?
### anticipate 'control' critique for pop distribution
### fold it into narrative of settler colonialism as structure

