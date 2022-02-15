library(MASS)
library(tidyverse)
library(ggdist)
library(tidybayes)
library(usmap)
library(viridisLite)
library(gridExtra)

select<-dplyr::select

####################################
############ MAPS
####################################

########## OUTCOME MAPS
p1<-plot_usmap(data = dat %>% 
             mutate(fc_rt = 
                      fc_total_contact / pop_child * 1e2) %>% 
             filter(year == 2019),
           values = "fc_rt") + 
  scale_fill_viridis() + 
  labs(title = "Foster care, 2019\n(percent of children)") + 
  theme(legend.title = element_blank(), legend.position = "bottom")

p2<-plot_usmap(data = dat %>% 
             mutate(incar_rt = 
                      aian_prison_pop / pop_adult * 1e3) %>% 
             filter(year == 2019),
           values = "incar_rt") + 
  scale_fill_viridis() + 
  labs(title = "Incarceration, 2019\n(adults per 1,000)") + 
  theme(legend.title = element_blank(), legend.position = "bottom")

p3<-plot_usmap(data = dat  %>% 
                 filter(year == 2019),
               values = "fe_deaths_rate") + 
  scale_fill_viridis() + 
  labs(title = "Killed by police, 2000-2019\n(per 100,000 annually)") + 
  theme(legend.title = element_blank(), legend.position = "bottom")
  
p_out<-grid.arrange(p1, p2, p3, nrow = 1)

ggsave(plot = p_out, "./vis/outcome_map.png", width = 7)

######################## PREDICTOR MAPS

p2<-plot_usmap(data = dat,
               values = "pct_AIAN_2010") + 
  scale_fill_viridis() + 
  labs(title = "Percent of population\nthat is AIAN (2010)") + 
  theme(legend.title = element_blank(), legend.position = "bottom")

p3<-plot_usmap(data = dat,
               values = "pct_AIANH_2010") + 
  scale_fill_viridis() + 
  labs(title = "Percent of AIAN population\nliving on tribal lands") + 
  theme(legend.title = element_blank(), legend.position = "bottom")

p1<-plot_usmap(data = dat,
               values = "admission_year")+ 
  scale_fill_viridis() + 
  labs(title = "Year of statehood") + 
  theme(legend.title = element_blank(), legend.position = "bottom")


p_out<-grid.arrange(p1, p2, p3, nrow = 1)

ggsave(plot = p_out, "./vis/predictor_map.png", width = 7)

### PREDICTION DATA 
pred_dat_imp1<-dat %>% filter(imp=="1_1")

#### COUNTERFACTUALS
### WHAT ARE GOOD COUNTERFACTUALS: GO FROM 5th->95th? or observed state ranges?
### incar
# qplot(pred_dat_imp1$aian_prison_pop_rt_1k)
### skewed distribution, SD is repeat outlier
incar_range<-pred_dat_imp1 %>% 
  summarize(lwr = quantile(aian_prison_pop_rt_1k, 0.05, na.rm=T),
            upr = quantile(aian_prison_pop_rt_1k, 0.95, na.rm=T))





### 90 percent range excludes SD, but includes high value other states, seems reasonable
### FE
# qplot(pred_dat_imp1$fe_deaths_rt_100k)
fe_range<-pred_dat_imp1 %>% 
  summarize(lwr = quantile(fe_deaths_rt_100k, 0.05, na.rm=T),
            upr = quantile(fe_deaths_rt_100k, 0.95, na.rm=T))
### looks fine. 
pred_dat_q0<-data.frame(
  aian_prison_rt_1k= seq(from = incar_range[[1]],
                        to = incar_range[[2]], 
                        length.out = 10),
  fe_deaths_rt_100k = seq(from = fe_range[[1]],
                          to = fe_range[[2]],
                          length.out = 10),
  state = "WI",
  pop_child = 1e5
)

t<-posterior_predict(q0_m1,
                     newdata = pred_dat_q0)
### model visuals: Bayesian
### USE TIDYBAYES AND GGDIST

### make plot data
prison_cuts<-quantile(dat$aian_prison_rt_1k, c(0.05, 0.95), na.rm=T)
fe_cuts<-quantile(dat$fe_deaths_rt_100k, c(0.05, 0.95), na.rm=T)

q0_m1_post<-data.frame(
  pop_child = 1e5,
  aian_prison_rt_1k = seq(prison_cuts[[1]], prison_cuts[[2]], length.out = 5),
  fe_deaths_rt_100k = seq(fe_cuts[[1]], fe_cuts[[2]], length.out = 5),
  quantile = seq(0.05, 0.95, length.out = 5)) %>%
  add_epred_draws(q0_m1, 
                  re_formula = NA, 
                  allow_new_levels = T) 


ggplot(q0_m1_post, 
       aes(x = .epred/1000,
           y = quantile,
           group = fe_deaths_rt_100k)) + 
  geom_density_ridges(scale = 2) +
  labs(x = "Expected percent of AIAN children in foster care, posterior mean",
       y = "Quantile of police violence and incarceration of AIAN adults")









# Model visuals: frequentist
# make_vis<-function(x){
#   b<-summary(x)$coef[, 1, drop = FALSE]
#   se<-summary(x)$coef[, 2, drop = FALSE]
#   predicted <- data.frame(scenario = c("1) Mean - 1 SD", "2) Mean", "3) Mean + 1 SD"),
#                           yhat = c(exp(b[1] - b[2]),
#                                    exp(b[1]),
#                                    exp(b[1] + b[2])))
#   return(predicted)
# }
# 
# #### q1: entangled
# 
# plot_1_dat<-make_vis(m_q0_fc) %>% 
#   mutate(model = "Incarceration") %>% 
#   bind_rows(
#     make_vis(m_q0_police) %>% 
#       mutate(model = "Police violence"))
# 
# ggplot(plot_1_dat,
#        aes(y = scenario,
#            x = yhat * 1e2,
#            color = model)) + 
#          geom_point() + 
#   labs(y = "Predictor value",
#        x = "Expected percent of AIAN children in foster care",
#        color = "") 
# 
# ggsave("./vis/model_entangled.png", width = 7)
#   
# ### q2: land contestation
# # m0_land_AIANH
# # m0_pct_AIAN
# 
# plot_1_dat<-make_vis(m0_land_AIANH) %>% 
#   mutate(model = "Percent AIAN population on tribal lands") %>% 
#   bind_rows(
#     make_vis(m0_pct_AIAN) %>% 
#       mutate(model = "Percent AIAN population"))
# 
# ggplot(plot_1_dat,
#        aes(y = scenario,
#            x = yhat * 1e2,
#            color = model)) + 
#   geom_point() + 
#   labs(y = "Predictor value",
#        x = "Expected percent of AIAN children in foster care",
#        color = "") 
# 
# ggsave("./vis/model_land.png", width = 7)
# 
# ### q3: land contestation
# # m0_admit
# # m1_admit
# 
# ### admission year
# pred_dat<-data.frame(
#   admission_year = min(dat$admission_year):max(dat$admission_year))
# 
# pred_dat<-pred_dat %>% 
#   mutate(yhat = 
#            predict(m0_admit, newdata = pred_dat,
#                    re.form = NA,
#                    type = "response"))
# 
# ggplot(pred_dat,
#        aes(x = admission_year,
#            y = yhat * 100)) + 
#   geom_line() + 
#   geom_text(data = dat %>% 
#               filter(year==2019),
#             aes(x = admission_year,
#                 y = fc_total_contact / pop_child * 100,
#                 label = state)) + 
#   labs(y = "Percent of AIAN children in foster care",
#        x = "Year of state admission to US",
#        subtitle = "Text labels indiate 2019 foster care rates, line indicates regression prediction")
# ggsave("./vis/model_adm_year.png", width = 7)
# 
# ### era of admission
# 
# pred_dat<-data.frame(indian_policy_era = dat$indian_policy_era) %>% 
#   distinct()
# 
# pred_dat<-pred_dat %>% 
#   mutate(yhat = 
#            predict(m1_admit, newdata = pred_dat,
#                    re.form = NA,
#                    type = "response"))
# 
# ggplot(pred_dat,
#        aes(x = yhat * 100,
#            y = indian_policy_era)) + 
#   geom_point() + 
#   labs(y = "Policy era when state was admitted",
#        x = "Expected percent of AIAN children in foster care")
# 
# ggsave("./vis/model_policy_era.png", width = 7)
# 
# ### restrictions on sovereign power
# 
# #### pl280 map
# plot_usmap(data = dat,
#                values = "pl280")+ 
#   scale_fill_viridis_d() + 
#   labs(title = "Public Law 280") + 
#   theme(legend.title = element_blank(), legend.position = "bottom")
# 
# ggsave("./vis/pl280map.png", width = 7)
# 
# pred_dat<-data.frame(indian_policy_era = dat$indian_policy_era,
#                      pl280 = dat$pl280) %>% 
#   distinct()
# 
# pred_dat<-pred_dat %>% 
#   mutate(yhat = 
#            predict(m1_pl280, newdata = pred_dat,
#                    re.form = NA,
#                    type = "response"))
# 
# ggplot(pred_dat,
#        aes(x = yhat * 100,
#            y = indian_policy_era,
#            color = pl280)) + 
#   geom_point() + 
#   labs(y = "Policy era when state was admitted",
#        x = "Expected percent of AIAN children in foster care",
#        color = "Public Law 280")
# 
# ggsave("./vis/model_pl280.png", width = 7)
