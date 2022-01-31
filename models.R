#### models for agents of empire
### created 1/27/22
### modified 1/27/22

### packages
library(tidyverse)
library(lme4)
library(brms)

### data
### fill in zeroes
fe<-read_csv("./data/fe_state_imputed_1_26_22.csv")  %>% 
  complete(state, year, race_ethn, 
           fill = list(fe_deaths_mn = 0,
                       fe_deaths_lwr_80 = 0,
                       fe_deaths_upr_80 = 0))

fc<-read_csv("./data/fc_st.csv") %>% 
  select(-st_fips, -stname)

pop<-read_csv("./data/pop_st.csv") %>% 
  filter(year>1999) %>% 
  filter(state!="KR") %>% 
  pivot_wider(names_from = child,
              values_from = pop,
              names_prefix = "pop_")

### join for all available years (onto pop)

dat<-pop %>% 
  left_join(fc) %>% 
  left_join(fe)

#### QUESTION 0: CORRELATION OF VIOLENCE ACROSS DOMAINS
# 
# ### compute unconditional correlations (or regressions) of FE and FC
# ### start at state-year, if power is low, move to state-period
# 
# ### first, look at scatterplot
# ggplot(dat %>% 
#          filter(race_ethn == "AIAN"), 
#        aes(x = fc_total_contact_mn / pop_child * 1e3,
#            y = fe_deaths_mn / pop_adult * 1e5)) + 
#   geom_point() + 
#   geom_smooth(method = "lm")
# 
# ### high police killing outliers are probably low pop one death years
# ### look at places with mort rate >20 per 100k
# temp<-dat %>% 
#   filter(race_ethn == "AIAN") %>% 
#   mutate(fe_rt = fe_deaths_mn / pop_adult*1e5) %>% 
#   filter(fe_rt>20) %>% 
#   select(year, state, pop_adult, fe_deaths_mn)
# ### 35 obs, maximum death count is 1.18
# ### should smooth this with cross period or moving average
# 
# ### compute 5 year moving average for FE deaths
# dat_out<-list()
# for(i in 2004:2019){
#   temp<-dat %>% 
#     filter(year <= i & year >= i - 4) %>% 
#     select(year, state, race_ethn, fe_deaths_mn,
#            fe_deaths_lwr_80, fe_deaths_upr_80) %>% 
#     group_by(state, race_ethn) %>% 
#     summarize(fe_deaths_mn_5yr = mean(fe_deaths_mn),
#               fe_deaths_lwr_80_5yr = mean(fe_deaths_lwr_80),
#               fe_deaths_upr_80_5yr = mean(fe_deaths_upr_80)) %>% 
#     ungroup() %>% 
#     mutate(year = i)  
#     
#   
#   dat_out[[i]]<-temp
# }
# 
# dat_out<-bind_rows(dat_out)
# dat<-dat %>% 
#   left_join(dat_out)
# 
# ### visualize with moving average for FE
# ggplot(dat %>% 
#          filter(race_ethn == "AIAN"), 
#        aes(x = fc_total_contact_mn / pop_child * 1e3,
#            y = fe_deaths_mn_5yr / pop_adult * 1e5)) + 
#   geom_point() + 
#   geom_smooth(method = "lm")
# 
# ### CHECK ON OUTLIERS AGAIN, but this looks better
# 
# ### try x period estimate
# cross_period<-dat %>% 
#   select(state, race_ethn, fe_deaths_mn,
#          fe_deaths_lwr_80, fe_deaths_upr_80) %>% 
#   group_by(state, race_ethn) %>% 
#   summarize(fe_deaths_mn_cross = mean(fe_deaths_mn),
#             fe_deaths_lwr_80_cross = mean(fe_deaths_lwr_80),
#             fe_deaths_upr_80_cross = mean(fe_deaths_upr_80)) %>% 
#   ungroup() 
# 
# dat<-dat %>% 
#   left_join(cross_period)
# ### visualize with moving average for FE
# ggplot(dat %>% 
#          filter(race_ethn == "AIAN"), 
#        aes(x = fc_total_contact_mn / pop_child * 1e3,
#            y = fe_deaths_mn_cross / pop_adult * 1e5)) + 
#   geom_point() + 
#   geom_smooth(method = "lm")
# 
# ### still some outliers, probably 1 or 2 states with low pops
# ### an RE model can handle it
# 
# ### test model - 
# m0<-lm(I(fe_deaths_mn / pop_adult * 1e5) ~ 
#          I(fc_total_contact_mn / pop_child * 1e3),
#        data = dat %>% 
#          filter(race_ethn == "AIAN"))
# ### clear positive relationship
# m0_re<-lmer(I(fe_deaths_mn / pop_adult * 1e5) ~ 
#          I(fc_total_contact_mn / pop_child * 1e3) + 
#            (1|state),
#        data = dat %>% 
#          filter(race_ethn == "AIAN"))
### washes out with RE, think on that

### FE IS PRETTY SPARSE. LETS USE NPS FOR THIS

### state admission 

