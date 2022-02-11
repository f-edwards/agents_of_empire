### read in nhgis pop data for agents of empire
###
library(tidyverse)

state<-read_csv("./data/nhgis0057_csv/nhgis0057_ds172_2010_state.csv")

### variable definitions from codebook
state_all<-state %>% 
  select(YEAR,
         STATE, 
         H72001,
         H72005, H72012, H72016,
         H72020, H72021, H72022,
         H72027, H72031, H72032,
         H72033, H72037, H72038,
         H72039, H72043, H72044,
         H72045,
         H72048:H72050,
         H72054:H72056,
         H72058:H72060,
         H72062, H72064:H72066,
         H72068, H72069, H72071) %>%
  mutate(pop_aian = rowSums(across(H72005:H72071))) %>%
  rename(pop_total = H72001) %>%
  select(YEAR, STATE,
         pop_total, pop_aian) %>% 
  mutate(type = "total")

state_reservation_trust_federal<-state %>% 
  select(YEAR,
         STATE, 
         H72AU001,
         H72AU005, H72AU012, H72AU016,
         H72AU020, H72AU021, H72AU022,
         H72AU027, H72AU031, H72AU032,
         H72AU033, H72AU037, H72AU038,
         H72AU039, H72AU043, H72AU044,
         H72AU045,
         H72AU048:H72AU050,
         H72AU054:H72AU056,
         H72AU058:H72AU060,
         H72AU062, H72AU064:H72AU066,
         H72AU068, H72AU069, H72AU071) %>%
  mutate(pop_aian = rowSums(across(H72AU005:H72AU071))) %>%
  rename(pop_total = H72AU001) %>%
  select(YEAR, STATE,
         pop_total, pop_aian) %>% 
  mutate(type = "reservation_trust_federal")

state_reservation_trust_state<-state %>% 
  select(YEAR,
         STATE, 
         H72AV001,
         H72AV005, H72AV012, H72AV016,
         H72AV020, H72AV021, H72AV022,
         H72AV027, H72AV031, H72AV032,
         H72AV033, H72AV037, H72AV038,
         H72AV039, H72AV043, H72AV044,
         H72AV045,
         H72AV048:H72AV050,
         H72AV054:H72AV056,
         H72AV058:H72AV060,
         H72AV062, H72AV064:H72AV066,
         H72AV068, H72AV069, H72AV071) %>%
  mutate(pop_aian = rowSums(across(H72AV005:H72AV071))) %>%
  rename(pop_total = H72AV001) %>%
  select(YEAR, STATE,
         pop_total, pop_aian) %>% 
  mutate(type = "reservation_trust_state")

TDSA<-state %>% 
  select(YEAR,
         STATE, 
         H72AW001,
         H72AW005, H72AW012, H72AW016,
         H72AW020, H72AW021, H72AW022,
         H72AW027, H72AW031, H72AW032,
         H72AW033, H72AW037, H72AW038,
         H72AW039, H72AW043, H72AW044,
         H72AW045,
         H72AW048:H72AW050,
         H72AW054:H72AW056,
         H72AW058:H72AW060,
         H72AW062, H72AW064:H72AW066,
         H72AW068, H72AW069, H72AW071) %>%
  mutate(pop_aian = rowSums(across(H72AW005:H72AW071))) %>%
  rename(pop_total = H72AW001) %>%
  select(YEAR, STATE,
         pop_total, pop_aian) %>% 
  mutate(type = "TDSA")

TDSA<-state %>% 
  select(YEAR,
         STATE, 
         H72AW001,
         H72AW005, H72AW012, H72AW016,
         H72AW020, H72AW021, H72AW022,
         H72AW027, H72AW031, H72AW032,
         H72AW033, H72AW037, H72AW038,
         H72AW039, H72AW043, H72AW044,
         H72AW045,
         H72AW048:H72AW050,
         H72AW054:H72AW056,
         H72AW058:H72AW060,
         H72AW062, H72AW064:H72AW066,
         H72AW068, H72AW069, H72AW071) %>%
  mutate(pop_aian = rowSums(across(H72AW005:H72AW071))) %>%
  rename(pop_total = H72AW001) %>%
  select(YEAR, STATE,
         pop_total, pop_aian) %>% 
  mutate(type = "TDSA")

AK<-state %>% 
  select(YEAR,
         STATE, 
         H72AY001,
         H72AY005, H72AY012, H72AY016,
         H72AY020, H72AY021, H72AY022,
         H72AY027, H72AY031, H72AY032,
         H72AY033, H72AY037, H72AY038,
         H72AY039, H72AY043, H72AY044,
         H72AY045,
         H72AY048:H72AY050,
         H72AY054:H72AY056,
         H72AY058:H72AY060,
         H72AY062, H72AY064:H72AY066,
         H72AY068, H72AY069, H72AY071) %>% 
  mutate(pop_aian = rowSums(across(H72AY005:H72AY071))) %>%
  rename(pop_total = H72AY001) %>%
  select(YEAR, STATE,
         pop_total, pop_aian) %>% 
  mutate(type = "AK village")

stateTDSA<-state %>% 
  select(YEAR,
         STATE, 
         H72AZ001,
         H72AZ005, H72AZ012, H72AZ016,
         H72AZ020, H72AZ021, H72AZ022,
         H72AZ027, H72AZ031, H72AZ032,
         H72AZ033, H72AZ037, H72AZ038,
         H72AZ039, H72AZ043, H72AZ044,
         H72AZ045,
         H72AZ048:H72AZ050,
         H72AZ054:H72AZ056,
         H72AZ058:H72AZ060,
         H72AZ062, H72AZ064:H72AZ066,
         H72AZ068, H72AZ069, H72AZ071) %>%
  mutate(pop_aian = rowSums(across(H72AZ005:H72AZ071))) %>%
  rename(pop_total = H72AZ001) %>%
  select(YEAR, STATE,
         pop_total, pop_aian) %>% 
  mutate(type = "state TDSA")

state_aian<-state_all %>% 
  bind_rows(state_reservation_trust_federal,
            state_reservation_trust_state,
            stateTDSA,
            TDSA,
            AK) 

### will use reservation (federal, state) TDSA (incl OK), state TDSA, AK Village
state_aianh<-state_aian %>% 
  filter(type!="total") %>% 
  group_by(STATE) %>% 
  summarize(pop_total_aianh = sum(pop_total),
            pop_aian_aianh = sum(pop_aian))

state_aian<-state_aian %>% 
  filter(type=="total") %>% 
  left_join(state_aianh)

### xwalk state abbreviations
xwalk<-data.frame(STATE = state.name, state = state.abb) %>% 
  bind_rows(data.frame(STATE = "District Of Columbia", state = "DC"))

state_aian<-state_aian %>% 
  left_join(xwalk) %>% 
  select(-STATE, -YEAR)

### as proportions

state_aian<-state_aian %>% 
  mutate(pct_AIAN_2010 = pop_aian/pop_total * 100,
         pct_AIANH_2010 = pop_aian_aianh / pop_aian * 100) %>% 
  select(state, pct_AIAN_2010, pct_AIANH_2010)

write_csv(state_aian, "./data/aianh_pop_census2010.csv")

