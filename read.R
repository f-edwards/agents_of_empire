#########
# read data for agents_of_empire

### notes
## revision 1/31, replace quantile imputation summary with raw
## plan to fit model on imp 1 for test, fit full bayesian on all imputed for 
## final

##### libraries
library(tidyverse)
library(haven)
library(lubridate)
### read pop data 
pop_aian<-read_csv("data/aianh_pop_census2010.csv")
### read fatal encounters
fe <- read_csv("./data/fe_1_26_22.csv", 
               guess_max = 1e5) %>% 
  filter(!(is.na(`Unique ID`))) 

fe<-fe %>% 
  rename(id = `Unique ID`,
         name = "Name",
         age = "Age",
         gender = "Gender",
         race = "Race",
         state = State,
         county = `Location of death (county)`,
         lat = Latitude,
         lon = Longitude,
         cause_of_death = `Highest level of force`,
         official_disposition = `Dispositions/Exclusions INTERNAL USE, NOT FOR ANALYSIS`,
         year = `Date of injury resulting in death (month/day/year)`) %>% 
  select(id, name, age, gender, race, state,
         county, lat, lon, cause_of_death, official_disposition,
         year) %>% 
  mutate(
    fe_cause_of_death = 
      case_when(grepl("suicide", tolower(official_disposition)) ~ "suicide",
                T ~ cause_of_death),
    year = year(mdy(year))) %>% 
  filter(year<=2019) # to match pop and AFCARS
### will want to impute missings, use pop data

### read pop data
pop<-read_fwf("./data/us.1990_2019.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop"))) 

### subset to 2016-2019 following period in AZ data
pop<-pop%>%
  mutate(year = as.integer(year),
         age = as.integer(age)) %>% 
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AIAN",
             race==4 ~ "API",
             hisp==1 ~ "Latinx")) 

### compute child and adult pop for each group
pop_st<-pop %>% 
  mutate(child = ifelse(age<18,
                        "child",
                        "adult")) %>% 
  group_by(year, state, child, race_ethn) %>% 
  summarize(pop = sum(pop)) %>% 
  ungroup()

## make pl 280 variable
pop_st$pl280<-case_when(
  pop_st$state %in% c("CA", "MN", "NE", "OR", 
                  "WI", "AK") ~ "Mandatory",
  pop_st$state %in% c("NV", "FL", "ID", "IA", 
                  "WA", "SD", "MT", "ND", 
                  "AZ", "UT") ~ "Optional",
  T ~ "Non-PL280")

### get year of admission data
state_data<-read_csv("https://raw.githubusercontent.com/CivilServiceUSA/us-states/master/data/states.csv")
  
pop_st<-pop_st %>% 
  left_join(state_data %>% 
              select(-state) %>% 
              rename(state = code) %>% 
              mutate(admission_year = year(admission_date)) %>% 
              select(state, admission_year))

### set up FE imputation
fe<-fe %>% 
  mutate(race_ethn = case_when(
    race == "Native American/Alaskan" ~ "AIAN",
    race == "African-American/Black" | race == "African-American/Black African-American/Black Not imputed" ~ "Black",
    race == "European-American/White" | race == "european-American/White" | race == "European-American/European-American/White" |
      race == "Middle Eastern" ~ "White",
    race == "Asian/Pacific Islander" ~ "API",
    race == "Hispanic/Latino" ~ "Latinx",
    T ~ NA_character_
  )) 

### pop comp for imputation predictor
pop_comp<-pop_st %>% 
  group_by(year, state, race_ethn) %>% 
  summarize(pop = sum(pop)) %>% 
  ungroup() %>% 
  left_join(pop_st %>% 
              group_by(year, state) %>% 
              summarize(pop_tot = sum(pop)) %>% 
              ungroup()) %>% 
  mutate(pop_pct = pop / pop_tot * 100) %>% 
  select(-pop, -pop_tot) %>% 
  pivot_wider(names_from = race_ethn,
              values_from = pop_pct,
              names_prefix = "pct_") %>% 
  select(-pct_White) 
  

### join, format for imp
fe_imp_dat<-fe %>% 
  select(id, year, state, race_ethn, age) %>% 
  mutate(age = as.numeric(age)) %>% 
  left_join(pop_comp) %>% 
  mutate(id = as.character(id),
         state = as.character(state),
         race_ethn = as.factor(race_ethn))

# ## quick check of regression, pct_black predicts Black race_ethn, ok
# m0<-glm(race_ethn=="Black" ~ 
#           year + age + pct_Black,
#         data = fe_imp,
#         family = "binomial")

library(mice)
rm(pop)
gc()
fe_imp<-mice(fe_imp_dat,
             m = 100)

fe_imp_out<-complete(fe_imp, action = "long")

# ### check counts across imps
# fe_imp_vis<-fe_imp_out %>% 
#   group_by(.imp, race_ethn) %>% 
#   summarize(n = n())
# ### densities of imputed race counts
# ggplot(fe_imp_vis,
#        aes(x = n)) + 
#   geom_density() + 
#   facet_wrap(~race_ethn, scales = "free")

### cut FE at 80th percentiles
fe_imp_out<-fe_imp_out %>%
  group_by(.imp, state, year, race_ethn) %>%
  summarize(fe_deaths = n())


### read AFCARS
fc_out<-read_csv("./data/afcars_all_events_state.csv")
### aggregate ages, make 80th pct cut
# fc_out<-fc %>% 
#   group_by(.imp, state, year, race_ethn) %>% 
#   summarize(across(fc_entries:fc_tpr, sum)) %>% 
#   group_by(state, year, race_ethn) %>% 
#   summarize(fc_entries_mn = mean(fc_entries),
#             fc_entries_lwr_80 = quantile(fc_entries, 0.1),
#             fc_entries_upr_80 = quantile(fc_entries, 0.9),
#             fc_total_contact_mn = mean(fc_total_contact),
#             fc_total_contact_lwr_80 = quantile(fc_total_contact, 0.1),
#             fc_total_contact_upr_80 = quantile(fc_total_contact, 0.9),
#             fc_tpr_mn = mean(fc_tpr),
#             fc_tpr_lwr_80 = quantile(fc_tpr, 0.1),
#             fc_tpr_upr_80 = quantile(fc_tpr, 0.9))
# # crosswalk to state abb            
xwalk<-read_csv("https://gist.githubusercontent.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53/raw/11d15233327c8080c9646c7e1f23052659db251d/us-state-ansi-fips.csv")
xwalk<-xwalk %>% 
  mutate(st = as.numeric(st)) %>% 
  rename(state = st, state_abb = stusps)
  
fc_out<-fc_out %>% 
  left_join(xwalk) %>% 
  rename(st_fips = state,
         state = state_abb) %>% 
  mutate(race_ethn = case_when(
    race_ethn == "AI/AN" ~ "AIAN",
    race_ethn == "Asian/PI" ~ "API",
    race_ethn == "Hispanic" ~ "Latinx",
    T ~ race_ethn
  ))
### AK 2004 is empty
fc_out<-fc_out %>% 
  group_by(.imp, state, year, race_ethn) %>% 
  summarize(across(fc_entries:fc_tpr, sum)) %>% 
  filter(!(state=="AK" & year==2004))
#### read in NPS data



write_csv(fe_imp_out, "./data/fe_state_imputed_1_26_22.csv")
write_csv(pop_st, "./data/pop_st.csv")
write_csv(fc_out, "./data/fc_st.csv")


