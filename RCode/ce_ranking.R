load('CE_project.RData')
## Mixed effects
library(dplyr)

# Set up data -------------------------------------------------------------
# library(lme4)
library(hglm)

library(readxl)  # install.packages("readxl") or install.packages("tidyverse")
library(plyr)
library(tibble)
library(data.table)
library(dplyr)
state_name_abbr = read.table(file='~/Documents/CE/klepikhina-masters-ce/data/state_to_abbr.csv',header = TRUE, sep=',')
cols_to_be_rectified <- names(state_name_abbr)[vapply(state_name_abbr, is.character, logical(1))]
state_name_abbr[,cols_to_be_rectified] <- lapply(state_name_abbr[,cols_to_be_rectified], trimws)
urb = read.table(file='~/Documents/CE/klepikhina-masters-ce/data/urbanization_classification.csv',header = TRUE, sep=',')

urb = left_join(urb, state_name_abbr, by = "State.Abr.")
drops = c("State.Abr.", "CBSA.title", "CBSA.2012.pop", "County.2012.pop", "X1990.based.code", "X")
urb = urb[ , !(names(urb) %in% drops)]
colnames(urb) <- c("FIPS", "County", "urb_code_2013", "urb_code_2006", "State")
urb$County = gsub("(.*?)\\sCounty$", "\\1", urb$County)
urb[,3] <- sapply(urb[,3],as.factor)
urb[,4] <- sapply(urb[,4],as.factor)

h_ranks = as.data.table(read_excel(path = "~/Documents/CE/klepikhina-masters-ce/data/county_health_rankings_2013.xls", sheet=3))
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
h_ranks = header.true(h_ranks)
h_ranks[, 1] <- sapply(h_ranks[, 1], as.integer)
colnames(h_ranks) <- c("FIPS", "State", "County", "Mortality_Z_Score", "Mortality_Rank", "Morbidity_Z_Score", "Morbidity_Rank", "Health_Behaviors_Z_Score", "Health_Behaviors_Rank", "Clinical_Care_Z_Score", "Clinical_Care_Rank", "Soc_Econ_Factors_Z_Score", "Soc_Econ_Factors_Rank", "Physical_Env_Z_Score", "Physical_Env_Rank")
h_ranks=h_ranks[!is.na(h_ranks$County),]
h_ranks[,4:15] <- lapply(h_ranks[,4:15],as.numeric)

h_factors = as.data.table(read_excel(path = "~/Documents/CE/klepikhina-masters-ce/data/county_health_rankings_2013.xls", sheet=4))
h_factors = header.true(h_factors)
h_factors[, 1] <- sapply(h_factors[, 1], as.integer)
h_factors=h_factors[!is.na(h_factors$County),]
h_factors = h_factors[,!c(4:30)]
h_factors = h_factors[,!c(6:8,10:12,14:16,19:21,24:26,29,33:35,38:40,44,51,54:56,59:61,64:66,68,72:74,78,81:83,86:88,91:94,97,99,102,105,108,111)]
h_factors = h_factors[,!c(20,21,23:27)]
h_factors = h_factors[,!c(24,29,46)]
# h_factors = h_factors[-ix, ]#subset(h_factors, select=-c("PCP Rate","PCP Ratio"))
colnames(h_factors) <- c("FIPS", "State", "County",
                         "Smoker_Sample_Size", "Perc_Smoker", "Perc_Obese",
                         "Perc_Phys_Inactive", "Excessive_Drinking_Sample_Size", "Perc_Excessive_Drinking",
                         "MV_Deaths", "MV_Mortality_Rate", "Chlamydia_Cases",
                         "Chlamydia_Rate", "Teen_Births", "Teen_Pop",
                         "Teen_Birth_Rate", "Uninsured", "Perc_Uninsured",
                         "Num_Physicians", "Num_Dentists", "Num_Medicare_Enrolled_Amb_Care",
                         "Amb_Care_Rate", "Num_Diabetics", "Num_Medicare_Enrolled_Mammography",
                         "Perc_Mammography", "Perc_HS_Grad", "Num_Some_College",
                         "Perc_Some_College", "Num_Unemployed", "Labor_Force",
                         "Perc_Unemployed", "Num_Children_Poverty", "Perc_Children_Poverty",
                         "Inadeq_Social_Support_Sample_Size", "Perc_No_Social_Support",
                         "Num_Single_Parent_House",
                         "Num_Households", "Annual_Violent_Crimes", "Violent_Crime_Rate",
                         "Avg_Daily_Particulate_Matter", "Perc_Pop_In_Violation_Drinking_Water_Safety",
                         "Num_Pop_In_Violation_Drinking_Water_Safety",
                         "Num_Rec_Fac", "Num_Limited_Access_To_Healthy_Food",
                         "Perc_Limited_Access_To_Healthy_Food", "Num_Fast_Food", "Perc_Fast_Food")
h_factors[,4:47] <- lapply(h_factors[,4:47],as.numeric)

demographics = as.data.table(read_excel(path = "~/Documents/CE/klepikhina-masters-ce/data/county_health_rankings_2013.xls", sheet=5))[, (16:61) := NULL]
demographics = header.true(demographics)
colnames(demographics) <- c("FIPS", "State", "County", "Population", "perc_under_18", "perc_over_65", "perc_AfAm", "perc_AmIn_AlNa", "perc_As", "perc_NaHI_PaIs", "perc_Hisp", "perc_NonHispWh", "non_profi_en", "perc_non_profi_en", "perc_female")
demographics=demographics[!is.na(demographics$County),]
demographics[, 1] <- sapply(demographics[, 1], as.integer)
demographics[,4:15] <- lapply(demographics[,4:15],as.numeric)

h_outcomes = as.data.table(read_excel(path = "~/Documents/CE/klepikhina-masters-ce/data/county_health_rankings_2013.xls", sheet=4))[, (31:138) := NULL]
h_outcomes = header.true(h_outcomes)
h_outcomes = header.true(h_outcomes)
h_outcomes[, 1] <- sapply(h_outcomes[, 1], as.integer)
colnames(h_outcomes) <- c("FIPS", "State", "County", "premature_deaths", "premature_death_YPLL_rate", "premature_death_YPLL_rate_CI_low", "premature_death_YPLL_rate_CI_high", "premature_death_YPLL_rate_Z_score", "poor_health_sample_size", "poor_health_perc", "poor_health_CI_low", "poor_health_CI_high", "poor_health_Z_score", "poor_phys_health_sample_size", "poor_phys_health_avg_over_30_days", "poor_phys_health_avg_over_30_days_CI_low", "poor_phys_health_avg_over_30_days_CI_high", "poor_phys_health_avg_over_30_days_Z_score", "poor_ment_health_sample_size", "poor_ment_health_avg_over_30_days", "poor_ment_health_avg_over_30_days_CI_low", "poor_ment_health_avg_over_30_days_CI_high", "poor_ment_health_avg_over_30_days_Z_score", "unreliable_data", "low_birthweight_births", "live_births", "low_birthweight_perc", "low_birthweight_perc_CI_low", "low_birthweight_perc_CI_high", "low_birthweight_perc_Z_score")
h_outcomes=h_outcomes[!is.na(h_outcomes$County),]
h_outcomes[,4:23] <- lapply(h_outcomes[,4:23],as.numeric)
h_outcomes$unreliable_data <- ifelse(grepl("x", h_outcomes$unreliable_data), 1, 0)
h_outcomes$unreliable_data <- sapply(h_outcomes$unreliable_data,as.factor)
h_outcomes[,25:30] <- lapply(h_outcomes[,25:30],as.numeric)

merge_cols <- c("FIPS", "County", "State")

df  <- merge(h_ranks, h_outcomes, by = merge_cols, all.x = TRUE)
df  <- merge(df, demographics, by = merge_cols, all.x = TRUE)
df  <- merge(df, urb, by = merge_cols, all.x = TRUE)
df  <- merge(df, h_factors, by = merge_cols, all.x = TRUE)
df[,1]  <- sapply(demographics[,1],as.factor)
df$urb_code_2013 <- factor(df$urb_code_2013)

df$poor_health_estimate = round(df$poor_health_perc*(df$poor_health_sample_size*0.01),0)

tmp = df[complete.cases(df), ] # complete dataset -- no NAs






# Imports For Bayes -------------------------------------------------------
library(rstanarm)
library(mice)
# md.pattern(df)
library(VIM)
library(broom.mixed)
library(shinystan)
library(brms)
library(dplyr)

library(rstan)
library(stringr)

library(BayesianFROC)
library(rstan)




# Impute Data -------------------------------------------------------------
df = df[, !(names(df) %in% c("poor_health_perc", "poor_health_sample_size"))]
imputed_Data <- mice(df, m=3, maxit = 1, method = 'cart', seed = 500)




# Create Summary Table 1 ----------------------------------------------------
get_df <- function(fit_summary) {
  post_mean_counties <- fit_summary[,c("mean")][55:3195]
  post_mean_state <- fit_summary[,c("mean")][4:54]
  intercept<- fit_summary[,c("mean")][1]
  
  print(dim(fit_summary))
  
  row_name_counties = names(post_mean_counties)
  row_values_counties = unname(post_mean_counties)
  state = str_extract(row_name_counties, '(?<=\\[)(.*?)(?=\\_)') #"(?<=\\[)([^\\[]*)(?=_)")
  intercept_col = rep(intercept, length(row_name_counties))
  class.df.counties<- data.frame(state, row_name_counties, row_values_counties, intercept_col)
  
  print(tail(unique(row_name_counties)))
  print(unique(state))
  print(dim(class.df.counties))

  row_name_state = names(post_mean_state)
  row_values_state = unname(post_mean_state)
  state = str_extract(row_name_state, "(?<=\\[)(.*?)(?=\\,)")
  intercept_col = rep(intercept, length(row_name_state))
  class.df.states<- data.frame(state, row_name_state, row_values_state, intercept_col)
  
  print(unique(row_name_state))
  print(unique(state))
  print(dim(class.df.states))
  
  class.df = merge(class.df.counties, class.df.states,c("state","intercept_col")) #by="state")
  
  print(dim(class.df))
  
  return(class.df)
  
}


# Get Summary Table 2 -----------------------------------------------------
get_df2 <- function(fit_summary) {
  
  print(dim(fit_summary))
  
  intercept<- fit_summary[,c("mean")][1]
  b_perc_AfAm<- fit_summary[,c("mean")][2]
  b_perc_As<- fit_summary[,c("mean")][3]
  b_perc_AmIn_AlNa<- fit_summary[,c("mean")][4]
  b_perc_Hisp<- fit_summary[,c("mean")][5]
  b_urb_code_20134<- fit_summary[,c("mean")][6]
  b_urb_code_20136<- fit_summary[,c("mean")][7]
  b_urb_code_20132<- fit_summary[,c("mean")][8]
  b_urb_code_20135<- fit_summary[,c("mean")][9]
  b_urb_code_20131<- fit_summary[,c("mean")][10]
  b_perc_female<- fit_summary[,c("mean")][11]
  b_perc_under_18<- fit_summary[,c("mean")][12]
  b_perc_over_65<- fit_summary[,c("mean")][13]
  # sd_state <- fit_summary[,c("mean")][14]
  # sd_counties <- fit_summary[,c("mean")][15]
  post_mean_state <- fit_summary[,c("mean")][16:66]
  post_mean_counties <- fit_summary[,c("mean")][67:3207]
  
  row_name_counties = names(post_mean_counties)
  row_values_counties = unname(post_mean_counties)
  counties = str_extract(row_name_counties, "(?<=_)([^_]+)(?=,)")
  counties = gsub('\\.', ' ', counties)
  state = str_extract(row_name_counties, '(?<=\\[)(.*?)(?=\\_)') #"(?<=\\[)([^\\[]*)(?=_)")
  intercept_col = rep(intercept, length(row_name_counties))
  class.df.counties<- data.frame(state, counties, row_name_counties, row_values_counties, intercept_col,
                                 b_perc_AfAm, b_perc_As, b_perc_AmIn_AlNa, b_perc_Hisp, b_perc_female,
                                 b_perc_under_18, b_perc_over_65)
  class.df.counties$b_urb[class.df.counties$counties == df$County & df$urb_code_2013 == "1"] <- b_urb_code_20131
  class.df.counties$b_urb[class.df.counties$counties == df$County & df$urb_code_2013 == "2"] <- b_urb_code_20132
  class.df.counties$b_urb[class.df.counties$counties == df$County & df$urb_code_2013 == "4"] <- b_urb_code_20134
  class.df.counties$b_urb[class.df.counties$counties == df$County & df$urb_code_2013 == "5"] <- b_urb_code_20135
  class.df.counties$b_urb[class.df.counties$counties == df$County & df$urb_code_2013 == "6"] <- b_urb_code_20136
  class.df.counties$b_urb[is.na(class.df.counties$b_urb)] <- 0
  
  print(dim(class.df.counties))
  
  row_name_state = names(post_mean_state)
  row_values_state = unname(post_mean_state)
  state = str_extract(row_name_state, "(?<=\\[)(.*?)(?=\\,)")
  intercept_col = rep(intercept, length(row_name_state))
  class.df.states<- data.frame(state, row_name_state, row_values_state, intercept_col)
  
  print(dim(class.df.states))
  
  class.df = merge(class.df.counties, class.df.states,c("state","intercept_col")) #by="state")
  
  print(dim(class.df))
  
  return(class.df)
  
}






# SEL Ranking -------------------------------------------------------------
sel <- function(data) {
  data1 <- na.omit(data) 
  k = length(data1$true_ranks)
  return ((1/k)*sum((data1$my_ranks-data1$true_ranks)^2))
}

save.image('CE_project.RData')
############################################################################################################################
#################################################### premature deaths 1 ####################################################
premature_deaths.1.prior <- c(
  prior(gamma(7.5, 1), class = Intercept)
)
premature_deaths.bayes.1 = brm_multiple(premature_deaths ~ (1|State/County), data=imputed_Data,
                                  family = poisson(link = "log"), prior=premature_deaths.1.prior,
                                  backend = "rstan", silent = 0, iter=4000)
save.image('CE_project.RData')
load('CE_project.RData')
launch_shinystan(premature_deaths.bayes.1)

fit_summary.pd1 <- summary(premature_deaths.bayes.1$fit)
premature_deaths.1.df = get_df(fit_summary.pd1$summary)
premature_deaths.1.df.summed = premature_deaths.1.df[,c("state", "row_name_counties")]
premature_deaths.1.df.summed$summed = exp(
                               premature_deaths.1.df$intercept_col +
                               premature_deaths.1.df$row_values_state +
                               premature_deaths.1.df$row_values_counties)

rank.premature_deaths.1 = premature_deaths.1.df.summed %>%
  group_by(state) %>%
  mutate(my_ranks = order(order(summed, row_name_counties, decreasing=TRUE)))

rank.premature_deaths.1 = rank.premature_deaths.1[with(rank.premature_deaths.1, order(row_name_counties)), ]
rank.premature_deaths.1$true_ranks = df$Mortality_Rank

sel.prem_death.1 <-rank.premature_deaths.1 %>% 
  group_by(state) %>%
  do(data.frame(standard.error.loss.mortality.1=sel(.))) 
sel.prem_death.1 = sel.prem_death.1[with(sel.prem_death.1, order(standard.error.loss.mortality.1)), ]
sel.prem_death.1

g1 <- ggplot(data = sel.prem_death.1, mapping = aes(x = as.factor(state), y = standard.error.loss.mortality.1)) +
  geom_bar(stat = "identity") +
  labs(x = "state") +
  ggtitle("Mortality Rank Mean Squared Error Loss Model 1") +
  xlab("") +
  ylab("Mean Squared Error Loss") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title=element_text(size=20))

print(g1)

#################################################### premature deaths 2 ####################################################
premature_deaths.2.prior <- c(
  prior(gamma(7.5, 1), class = Intercept),
  prior(normal(0, 10), class = b)
)
premature_deaths.bayes.2 = brm_multiple(premature_deaths ~ perc_AfAm + perc_As + perc_AmIn_AlNa +
                                          perc_Hisp + urb_code_2013 + perc_female + perc_under_18 +
                                          perc_over_65 + (1|State/County), data=imputed_Data,
                                        family = poisson(link = "log"), prior=premature_deaths.2.prior,
                                        backend = "rstan", silent = 0, iter=4000)
save.image('CE_project.RData')
load('CE_project.RData')
launch_shinystan(premature_deaths.bayes.2)

fit_summary.pd2 = summary(premature_deaths.bayes.2$fit)
premature_deaths.2.df = get_df2(fit_summary.pd2$summary)
premature_deaths.2.df.summed = premature_deaths.2.df[,c("state", "row_name_counties")]
premature_deaths.2.df.summed$summed = exp(premature_deaths.2.df$intercept_col +
                                            premature_deaths.2.df$b_perc_AfAm +
                                            premature_deaths.2.df$b_perc_As +
                                            premature_deaths.2.df$b_perc_AmIn_AlNa +
                                            premature_deaths.2.df$b_perc_Hisp+
                                            premature_deaths.2.df$b_urb+
                                            premature_deaths.2.df$b_perc_female+
                                            premature_deaths.2.df$b_perc_under_18+
                                            premature_deaths.2.df$b_perc_over_65+
                                            premature_deaths.2.df$row_values_state+
                                            premature_deaths.2.df$row_values_counties)

rank.premature_deaths.2 = premature_deaths.2.df.summed %>%
  group_by(state) %>%
  mutate(my_ranks = order(order(summed, row_name_counties, decreasing=TRUE)))

rank.premature_deaths.2 = rank.premature_deaths.2[with(rank.premature_deaths.2, order(row_name_counties)), ]
rank.premature_deaths.2$true_ranks = df$Mortality_Rank

sel.prem_death.2 <-rank.premature_deaths.2 %>% 
  group_by(state) %>%
  do(data.frame(standard.error.loss.mortality.2=sel(.))) 
sel.prem_death.2 = sel.prem_death.2[with(sel.prem_death.2, order(standard.error.loss.mortality.2)), ]
sel.prem_death.2

g2 <- ggplot(data = sel.prem_death.2, mapping = aes(x = as.factor(state), y = standard.error.loss.mortality.2)) +
  geom_bar(stat = "identity") +
  labs(x = "state") +
  ggtitle("Mortality Rank Mean Squared Error Loss Model 2") +
  xlab("") +
  ylab("Mean Squared Error Loss") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title=element_text(size=20))

print(g2)

############################################################################################################################
#################################################### poor_phys_health_avg_over_30_days 1 ###################################
poor_phys_avg.1.prior <- c(
  prior(normal(3, 10), class = Intercept),
  prior(normal(3, 10), class = sigma)
)
poor_phys_avg.bayes.1 = brm_multiple(poor_phys_health_avg_over_30_days ~ (1|State/County), data=imputed_Data,
                                        family = gaussian(link = "identity"), prior=poor_phys_avg.1.prior,
                                        backend = "rstan", silent = 0, iter=4000, control=list(adapt_delta=0.8))
save.image('CE_project.RData')
load('CE_project.RData')
launch_shinystan(poor_phys_avg.bayes.1)
# list_of_draws <- extract(testing$fit)

fit_summary.ppa1 = summary(poor_phys_avg.bayes.1$fit)
print(dim(fit_summary.ppa1$summary))

post_mean_counties <- fit_summary.ppa1$summary[,c("mean")][56:3196]
post_mean_state <- fit_summary.ppa1$summary[,c("mean")][5:55]
intercept<- fit_summary.ppa1$summary[,c("mean")][1]

row_name_counties = names(post_mean_counties)
row_values_counties = unname(post_mean_counties)
state = str_extract(row_name_counties, '(?<=\\[)(.*?)(?=\\_)') #"(?<=\\[)([^\\[]*)(?=_)")
intercept_col = rep(intercept, length(row_name_counties))
poor_phys_avg.1.df.counties<- data.frame(state, row_name_counties, row_values_counties, intercept_col)

print(unique(state))
print(dim(poor_phys_avg.1.df.counties))

row_name_state = names(post_mean_state)
row_values_state = unname(post_mean_state)
state = str_extract(row_name_state, "(?<=\\[)(.*?)(?=\\,)")
intercept_col = rep(intercept, length(row_name_state))
poor_phys_avg.1.df.states<- data.frame(state, row_name_state, row_values_state, intercept_col)

print(unique(row_name_state))
print(unique(state))
print(dim(poor_phys_avg.1.df.states))

poor_phys_avg.1.df = merge(poor_phys_avg.1.df.counties, poor_phys_avg.1.df.states,c("state","intercept_col"))
print(dim(poor_phys_avg.1.df))

rm(post_mean_counties, row_values_counties, state, intercept_col, poor_phys_avg.1.df.counties,
   row_name_state, row_values_state, poor_phys_avg.1.df.states)

# poor_phys_avg.1.df = get_df(fit_summary.ppa1$summary)
poor_phys_avg.1.df.summed = poor_phys_avg.1.df[,c("state", "row_name_counties")]
poor_phys_avg.1.df.summed$summed = poor_phys_avg.1.df$intercept_col +
                                            poor_phys_avg.1.df$row_values_state +
                                            poor_phys_avg.1.df$row_values_counties

# rank.poor_phys_avg.1 = poor_phys_avg.1.df.summed %>%
#   group_by(state) %>%
#   mutate(my_ranks = order(order(summed, row_name_counties, decreasing=TRUE)))



#################################################### poor_phys_health_avg_over_30_days 2 ####################################
poor_phys_avg.2.prior <- c(
  prior(normal(0, 10), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(normal(0, 10), class = sigma)
) ## HAS SUPER BAD COUNTY INTERCEPT
poor_phys_avg.bayes.2 = brm_multiple(poor_phys_health_avg_over_30_days ~ perc_AfAm + perc_As + perc_AmIn_AlNa +
                                       perc_Hisp + factor(urb_code_2013) + perc_female + perc_under_18 +
                                       perc_over_65 + (1|State/County), data=imputed_Data,
                                     family = gaussian(link = "identity"), prior=poor_phys_avg.2.prior,
                                     backend = "rstan", silent = 0, iter=4000)
save.image('CE_project.RData')
load('CE_project.RData')
launch_shinystan(poor_phys_avg.bayes.2)

fit_summary.ppa2 = summary(poor_phys_avg.bayes.2$fit)
print(dim(fit_summary.ppa2$summary))

intercept<- fit_summary.ppa2$summary[,c("mean")][1]
b_perc_AfAm<- fit_summary.ppa2$summary[,c("mean")][2]
b_perc_As<- fit_summary.ppa2$summary[,c("mean")][3]
b_perc_AmIn_AlNa<- fit_summary.ppa2$summary[,c("mean")][4]
b_perc_Hisp<- fit_summary.ppa2$summary[,c("mean")][5]
b_urb_code_20134<- fit_summary.ppa2$summary[,c("mean")][6]
b_urb_code_20136<- fit_summary.ppa2$summary[,c("mean")][7]
b_urb_code_20132<- fit_summary.ppa2$summary[,c("mean")][8]
b_urb_code_20135<- fit_summary.ppa2$summary[,c("mean")][9]
b_urb_code_20131<- fit_summary.ppa2$summary[,c("mean")][10]
b_perc_female<- fit_summary.ppa2$summary[,c("mean")][11]
b_perc_under_18<- fit_summary.ppa2$summary[,c("mean")][12]
b_perc_over_65<- fit_summary.ppa2$summary[,c("mean")][13]
post_mean_state <- fit_summary.ppa2$summary[,c("mean")][17:67]
post_mean_counties <- fit_summary.ppa2$summary[,c("mean")][68:3208]

row_name_counties = names(post_mean_counties)
row_values_counties = unname(post_mean_counties)
counties = str_extract(row_name_counties, "(?<=_)([^_]+)(?=,)")
counties = gsub('\\.', ' ', counties)
state = str_extract(row_name_counties, '(?<=\\[)(.*?)(?=\\_)') #"(?<=\\[)([^\\[]*)(?=_)")
intercept_col = rep(intercept, length(row_name_counties))
poor_phys_avg.2.df.counties<- data.frame(state, counties, row_name_counties, row_values_counties, intercept_col,
                               b_perc_AfAm, b_perc_As, b_perc_AmIn_AlNa, b_perc_Hisp, b_perc_female,
                               b_perc_under_18, b_perc_over_65)
poor_phys_avg.2.df.counties$b_urb[poor_phys_avg.2.df.counties$counties == df$County & df$urb_code_2013 == "1"] <- b_urb_code_20131
poor_phys_avg.2.df.counties$b_urb[poor_phys_avg.2.df.counties$counties == df$County & df$urb_code_2013 == "2"] <- b_urb_code_20132
poor_phys_avg.2.df.counties$b_urb[poor_phys_avg.2.df.counties$counties == df$County & df$urb_code_2013 == "4"] <- b_urb_code_20134
poor_phys_avg.2.df.counties$b_urb[poor_phys_avg.2.df.counties$counties == df$County & df$urb_code_2013 == "5"] <- b_urb_code_20135
poor_phys_avg.2.df.counties$b_urb[poor_phys_avg.2.df.counties$counties == df$County & df$urb_code_2013 == "6"] <- b_urb_code_20136
poor_phys_avg.2.df.counties$b_urb[is.na(poor_phys_avg.2.df.counties$b_urb)] <- 0

print(dim(poor_phys_avg.2.df.counties))

row_name_state = names(post_mean_state)
row_values_state = unname(post_mean_state)
state = str_extract(row_name_state, "(?<=\\[)(.*?)(?=\\,)")
intercept_col = rep(intercept, length(row_name_state))
poor_phys_avg.2.df.states<- data.frame(state, row_name_state, row_values_state, intercept_col)

print(dim(poor_phys_avg.2.df.states))

poor_phys_avg.2.df = merge(poor_phys_avg.2.df.counties, poor_phys_avg.2.df.states,c("state","intercept_col")) #by="state")

print(dim(poor_phys_avg.2.df))

rm(post_mean_counties, row_values_counties, state, intercept_col, poor_phys_avg.2.df.counties,
   row_name_state, row_values_state, poor_phys_avg.2.df.states, b_perc_AfAm, b_perc_As,
   b_perc_AmIn_AlNa, b_perc_Hisp, b_perc_female, b_perc_under_18, b_perc_over_65)

# poor_phys_avg.2.df = get_df2(fit_summary.ppa2$summary)
poor_phys_avg.2.df.summed = poor_phys_avg.2.df[,c("state", "row_name_counties")]
poor_phys_avg.2.df.summed$summed = poor_phys_avg.2.df$intercept_col +
  poor_phys_avg.2.df$b_perc_AfAm +
  poor_phys_avg.2.df$b_perc_As +
  poor_phys_avg.2.df$b_perc_AmIn_AlNa +
  poor_phys_avg.2.df$b_perc_Hisp+
  poor_phys_avg.2.df$b_urb+
  poor_phys_avg.2.df$b_perc_female+
  poor_phys_avg.2.df$b_perc_under_18+
  poor_phys_avg.2.df$b_perc_over_65+
  poor_phys_avg.2.df$row_values_state+
  poor_phys_avg.2.df$row_values_counties

# rank.poor_phys_avg.2 = poor_phys_avg.2.df.summed %>%
#   group_by(state) %>%
#   mutate(my_ranks = order(order(summed, row_name_counties, decreasing=TRUE)))

# rank.poor_phys_avg.2 = rank.poor_phys_avg.2[with(rank.poor_phys_avg.2, order(row_name_counties)), ]


############################################################################################################################
#################################################### poor_ment_health_avg_over_30_days 1 ###################################
poor_ment_avg.1.prior <- c(
  prior(normal(0, 10), class = Intercept),
  prior(normal(0, 10), class = sigma)
) ## HAS SUPER BAD COUNTY INTERCEPT
poor_ment_avg.bayes.1 = brm_multiple(poor_ment_health_avg_over_30_days ~ (1|State/County), data=imputed_Data,
                                     family = gaussian(link = "identity"), prior=poor_ment_avg.1.prior,
                                     backend = "rstan", silent = 0, iter=4000)
save.image('CE_project.RData')
load('CE_project.RData')
launch_shinystan(poor_ment_avg.bayes.1)

fit_summary.pma1 = summary(poor_ment_avg.bayes.1$fit)
print(dim(fit_summary.pma1$summary))

post_mean_counties <- fit_summary.pma1$summary[,c("mean")][56:3196]
post_mean_state <- fit_summary.pma1$summary[,c("mean")][5:55]
intercept<- fit_summary.pma1$summary[,c("mean")][1]

row_name_counties = names(post_mean_counties)
row_values_counties = unname(post_mean_counties)
state = str_extract(row_name_counties, '(?<=\\[)(.*?)(?=\\_)') #"(?<=\\[)([^\\[]*)(?=_)")
intercept_col = rep(intercept, length(row_name_counties))
poor_ment_avg.1.df.counties<- data.frame(state, row_name_counties, row_values_counties, intercept_col)

print(unique(state))
print(dim(poor_ment_avg.1.df.counties))

row_name_state = names(post_mean_state)
row_values_state = unname(post_mean_state)
state = str_extract(row_name_state, "(?<=\\[)(.*?)(?=\\,)")
intercept_col = rep(intercept, length(row_name_state))
poor_ment_avg.1.df.states<- data.frame(state, row_name_state, row_values_state, intercept_col)

print(unique(row_name_state))
print(unique(state))
print(dim(poor_ment_avg.1.df.states))

poor_ment_avg.1.df = merge(poor_ment_avg.1.df.counties, poor_ment_avg.1.df.states,c("state","intercept_col"))
print(dim(poor_ment_avg.1.df))

rm(post_mean_counties, row_values_counties, state, intercept_col, poor_ment_avg.1.df.counties,
   row_name_state, row_values_state, poor_ment_avg.1.df.states)

poor_ment_avg.1.df.summed = poor_ment_avg.1.df[,c("state", "row_name_counties")]
poor_ment_avg.1.df.summed$summed = poor_ment_avg.1.df$intercept_col +
                                         poor_ment_avg.1.df$row_values_state +
                                         poor_ment_avg.1.df$row_values_counties

# rank.poor_ment_avg.1 = poor_ment_avg.1.df.summed %>%
#   group_by(state) %>%
#   mutate(my_ranks = order(order(summed, row_name_counties, decreasing=TRUE)))
# 
# rank.poor_ment_avg.1 = rank.poor_ment_avg.1[with(rank.poor_ment_avg.1, order(row_name_counties)), ]

#################################################### poor_ment_health_avg_over_30_days 2 ####################################
poor_ment_avg.2.prior <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, 1), class = b),
  prior(normal(0, 1), class = sigma)
) ## HAS SUPER BAD COUNTY INTERCEPT
poor_ment_avg.bayes.2 = brm_multiple(poor_ment_health_avg_over_30_days ~ perc_AfAm + perc_As + perc_AmIn_AlNa +
                                       perc_Hisp + factor(urb_code_2013) + perc_female + perc_under_18 +
                                       perc_over_65 + (1|State/County), data=imputed_Data,
                                     family = gaussian(link = "identity"), prior=poor_ment_avg.2.prior,
                                     backend = "rstan", silent = 0, iter=4000)
save.image('CE_project.RData')
load('CE_project.RData')
launch_shinystan(poor_ment_avg.bayes.2)

fit_summary.pma2 = summary(poor_ment_avg.bayes.2$fit)
print(dim(fit_summary.pma2$summary))

intercept<- fit_summary.pma2$summary[,c("mean")][1]
b_perc_AfAm<- fit_summary.pma2$summary[,c("mean")][2]
b_perc_As<- fit_summary.pma2$summary[,c("mean")][3]
b_perc_AmIn_AlNa<- fit_summary.pma2$summary[,c("mean")][4]
b_perc_Hisp<- fit_summary.pma2$summary[,c("mean")][5]
b_urb_code_20134<- fit_summary.pma2$summary[,c("mean")][6]
b_urb_code_20136<- fit_summary.pma2$summary[,c("mean")][7]
b_urb_code_20132<- fit_summary.pma2$summary[,c("mean")][8]
b_urb_code_20135<- fit_summary.pma2$summary[,c("mean")][9]
b_urb_code_20131<- fit_summary.pma2$summary[,c("mean")][10]
b_perc_female<- fit_summary.pma2$summary[,c("mean")][11]
b_perc_under_18<- fit_summary.pma2$summary[,c("mean")][12]
b_perc_over_65<- fit_summary.pma2$summary[,c("mean")][13]
post_mean_state <- fit_summary.pma2$summary[,c("mean")][17:67]
post_mean_counties <- fit_summary.pma2$summary[,c("mean")][68:3208]

row_name_counties = names(post_mean_counties)
row_values_counties = unname(post_mean_counties)
counties = str_extract(row_name_counties, "(?<=_)([^_]+)(?=,)")
counties = gsub('\\.', ' ', counties)
state = str_extract(row_name_counties, '(?<=\\[)(.*?)(?=\\_)') #"(?<=\\[)([^\\[]*)(?=_)")
intercept_col = rep(intercept, length(row_name_counties))
poor_ment_avg.2.df.counties<- data.frame(state, counties, row_name_counties, row_values_counties, intercept_col,
                                         b_perc_AfAm, b_perc_As, b_perc_AmIn_AlNa, b_perc_Hisp, b_perc_female,
                                         b_perc_under_18, b_perc_over_65)
poor_ment_avg.2.df.counties$b_urb[poor_ment_avg.2.df.counties$counties == df$County & df$urb_code_2013 == "1"] <- b_urb_code_20131
poor_ment_avg.2.df.counties$b_urb[poor_ment_avg.2.df.counties$counties == df$County & df$urb_code_2013 == "2"] <- b_urb_code_20132
poor_ment_avg.2.df.counties$b_urb[poor_ment_avg.2.df.counties$counties == df$County & df$urb_code_2013 == "4"] <- b_urb_code_20134
poor_ment_avg.2.df.counties$b_urb[poor_ment_avg.2.df.counties$counties == df$County & df$urb_code_2013 == "5"] <- b_urb_code_20135
poor_ment_avg.2.df.counties$b_urb[poor_ment_avg.2.df.counties$counties == df$County & df$urb_code_2013 == "6"] <- b_urb_code_20136
poor_ment_avg.2.df.counties$b_urb[is.na(poor_ment_avg.2.df.counties$b_urb)] <- 0

print(dim(poor_ment_avg.2.df.counties))

row_name_state = names(post_mean_state)
row_values_state = unname(post_mean_state)
state = str_extract(row_name_state, "(?<=\\[)(.*?)(?=\\,)")
intercept_col = rep(intercept, length(row_name_state))
poor_ment_avg.2.df.states<- data.frame(state, row_name_state, row_values_state, intercept_col)

print(dim(poor_ment_avg.2.df.states))

poor_ment_avg.2.df = merge(poor_ment_avg.2.df.counties, poor_ment_avg.2.df.states,c("state","intercept_col")) #by="state")

print(dim(poor_ment_avg.2.df))

rm(post_mean_counties, row_values_counties, state, intercept_col, poor_ment_avg.2.df.counties,
   row_name_state, row_values_state, poor_ment_avg.2.df.states, b_perc_AfAm, b_perc_As,
   b_perc_AmIn_AlNa, b_perc_Hisp, b_perc_female, b_perc_under_18, b_perc_over_65)

poor_ment_avg.2.df.summed = poor_ment_avg.2.df[,c("state", "row_name_counties")]
poor_ment_avg.2.df.summed$summed = poor_ment_avg.2.df$intercept_col +
  poor_ment_avg.2.df$b_perc_AfAm +
  poor_ment_avg.2.df$b_perc_As +
  poor_ment_avg.2.df$b_perc_AmIn_AlNa +
  poor_ment_avg.2.df$b_perc_Hisp+
  poor_ment_avg.2.df$b_urb+
  poor_ment_avg.2.df$b_perc_female+
  poor_ment_avg.2.df$b_perc_under_18+
  poor_ment_avg.2.df$b_perc_over_65+
  poor_ment_avg.2.df$row_values_state+
  poor_ment_avg.2.df$row_values_counties

# rank.poor_ment_avg.2 = poor_ment_avg.2.df.summed %>%
#   group_by(state) %>%
#   mutate(my_ranks = order(order(summed, row_name_counties, decreasing=TRUE)))
# 
# rank.poor_ment_avg.2 = rank.poor_ment_avg.2[with(rank.poor_ment_avg.2, order(row_name_counties)), ]

############################################################################################################################
#################################################### low_birthweight_births 1 ###################################
low_bwb.1.prior <- c(
  prior(normal(1, 1), class = Intercept)
) ## HAS KINDA BAD COUNTY INTERCEPT
low_bwb.bayes.1 = brm_multiple(low_birthweight_births ~ (1|State/County), data=imputed_Data,
                                     family = binomial(link = "logit"), prior=low_bwb.1.prior,
                                     backend = "rstan", silent = 0, iter=4000)
save.image('CE_project.RData')
load('CE_project.RData')
launch_shinystan(low_bwb.bayes.1)

fit_summary.lbwb1 = summary(low_bwb.bayes.1$fit)
print(dim(fit_summary.lbwb1$summary))

post_mean_counties <- fit_summary.lbwb1$summary[,c("mean")][55:3195]
post_mean_state <- fit_summary.lbwb1$summary[,c("mean")][4:54]
intercept<- fit_summary.lbwb1$summary[,c("mean")][1]

row_name_counties = names(post_mean_counties)
row_values_counties = unname(post_mean_counties)
state = str_extract(row_name_counties, '(?<=\\[)(.*?)(?=\\_)') #"(?<=\\[)([^\\[]*)(?=_)")
intercept_col = rep(intercept, length(row_name_counties))
low_bwb.1.df.counties<- data.frame(state, row_name_counties, row_values_counties, intercept_col)

print(unique(state))
print(dim(low_bwb.1.df.counties))

row_name_state = names(post_mean_state)
row_values_state = unname(post_mean_state)
state = str_extract(row_name_state, "(?<=\\[)(.*?)(?=\\,)")
intercept_col = rep(intercept, length(row_name_state))
low_bwb.1.df.states<- data.frame(state, row_name_state, row_values_state, intercept_col)

print(unique(row_name_state))
print(unique(state))
print(dim(low_bwb.1.df.states))

low_bwb.1.df = merge(low_bwb.1.df.counties, low_bwb.1.df.states,c("state","intercept_col"))
print(dim(low_bwb.1.df))

rm(post_mean_counties, row_values_counties, state, intercept_col, low_bwb.1.df.counties,
   row_name_state, row_values_state, low_bwb.1.df.states)

low_bwb.1.df.summed = low_bwb.1.df[,c("state", "row_name_counties")]
low_bwb.1.df.summed$summed = exp(low_bwb.1.df$intercept_col +low_bwb.1.df$row_values_state + low_bwb.1.df$row_values_counties) /
  (1+exp(low_bwb.1.df$intercept_col +low_bwb.1.df$row_values_state + low_bwb.1.df$row_values_counties))

# rank.low_bwb.1 = low_bwb.1.df.summed %>%
#   group_by(state) %>%
#   mutate(my_ranks = order(order(summed, row_name_counties, decreasing=TRUE)))
# 
# rank.low_bwb.1 = rank.low_bwb.1[with(rank.low_bwb.1, order(row_name_counties)), ]

#################################################### low_birthweight_births 2 ####################################
low_bwb.2.prior <- c(
  prior(normal(1, 1), class = Intercept),
  prior(normal(1, 1), class = b)
)
low_bwb.bayes.2 = brm_multiple(low_birthweight_births ~ perc_AfAm + perc_As + perc_AmIn_AlNa +
                                       perc_Hisp + factor(urb_code_2013) + perc_female + perc_under_18 +
                                       perc_over_65 + (1|State/County), data=imputed_Data,
                                     family = binomial(link = "logit"), prior=low_bwb.2.prior,
                                     backend = "rstan", silent = 0, inits=c(15, 5), iter=4000)
save.image('CE_project.RData')
load('CE_project.RData')
launch_shinystan(low_bwb.bayes.2)

fit_summary.lbwb2 = summary(low_bwb.bayes.2$fit)

print(dim(fit_summary.lbwb2$summary))

intercept<- fit_summary.lbwb2$summary[,c("mean")][1]
b_perc_AfAm<- fit_summary.lbwb2$summary[,c("mean")][2]
b_perc_As<- fit_summary.lbwb2$summary[,c("mean")][3]
b_perc_AmIn_AlNa<- fit_summary.lbwb2$summary[,c("mean")][4]
b_perc_Hisp<- fit_summary.lbwb2$summary[,c("mean")][5]
b_urb_code_20134<- fit_summary.lbwb2$summary[,c("mean")][6]
b_urb_code_20136<- fit_summary.lbwb2$summary[,c("mean")][7]
b_urb_code_20132<- fit_summary.lbwb2$summary[,c("mean")][8]
b_urb_code_20135<- fit_summary.lbwb2$summary[,c("mean")][9]
b_urb_code_20131<- fit_summary.lbwb2$summary[,c("mean")][10]
b_perc_female<- fit_summary.lbwb2$summary[,c("mean")][11]
b_perc_under_18<- fit_summary.lbwb2$summary[,c("mean")][12]
b_perc_over_65<- fit_summary.lbwb2$summary[,c("mean")][13]
post_mean_state <- fit_summary.lbwb2$summary[,c("mean")][16:66]
post_mean_counties <- fit_summary.lbwb2$summary[,c("mean")][67:3207]

row_name_counties = names(post_mean_counties)
row_values_counties = unname(post_mean_counties)
counties = str_extract(row_name_counties, "(?<=_)([^_]+)(?=,)")
counties = gsub('\\.', ' ', counties)
state = str_extract(row_name_counties, '(?<=\\[)(.*?)(?=\\_)') #"(?<=\\[)([^\\[]*)(?=_)")
intercept_col = rep(intercept, length(row_name_counties))
low_bwb.2.df.counties<- data.frame(state, counties, row_name_counties, row_values_counties, intercept_col,
                                         b_perc_AfAm, b_perc_As, b_perc_AmIn_AlNa, b_perc_Hisp, b_perc_female,
                                         b_perc_under_18, b_perc_over_65)
low_bwb.2.df.counties$b_urb[low_bwb.2.df.counties$counties == df$County & df$urb_code_2013 == "1"] <- b_urb_code_20131
low_bwb.2.df.counties$b_urb[low_bwb.2.df.counties$counties == df$County & df$urb_code_2013 == "2"] <- b_urb_code_20132
low_bwb.2.df.counties$b_urb[low_bwb.2.df.counties$counties == df$County & df$urb_code_2013 == "4"] <- b_urb_code_20134
low_bwb.2.df.counties$b_urb[low_bwb.2.df.counties$counties == df$County & df$urb_code_2013 == "5"] <- b_urb_code_20135
low_bwb.2.df.counties$b_urb[low_bwb.2.df.counties$counties == df$County & df$urb_code_2013 == "6"] <- b_urb_code_20136
low_bwb.2.df.counties$b_urb[is.na(low_bwb.2.df.counties$b_urb)] <- 0

print(dim(low_bwb.2.df.counties))

row_name_state = names(post_mean_state)
row_values_state = unname(post_mean_state)
state = str_extract(row_name_state, "(?<=\\[)(.*?)(?=\\,)")
intercept_col = rep(intercept, length(row_name_state))
low_bwb.2.df.states<- data.frame(state, row_name_state, row_values_state, intercept_col)

print(dim(low_bwb.2.df.states))

low_bwb.2.df = merge(low_bwb.2.df.counties, low_bwb.2.df.states,c("state","intercept_col")) #by="state")

print(dim(low_bwb.2.df))

rm(post_mean_counties, row_values_counties, state, intercept_col, low_bwb.2.df.counties,
   row_name_state, row_values_state, low_bwb.2.df.states, b_perc_AfAm, b_perc_As,
   b_perc_AmIn_AlNa, b_perc_Hisp, b_perc_female, b_perc_under_18, b_perc_over_65)

low_bwb.2.df.summed = low_bwb.2.df[,c("state", "row_name_counties")]
low_bwb.2.df.summed$summed = exp(low_bwb.2.df$intercept_col +
                                   low_bwb.2.df$b_perc_AfAm +
                                   low_bwb.2.df$b_perc_As +
                                   low_bwb.2.df$b_perc_AmIn_AlNa +
                                   low_bwb.2.df$b_perc_Hisp+
                                   low_bwb.2.df$b_urb+
                                   low_bwb.2.df$b_perc_female+
                                   low_bwb.2.df$b_perc_under_18+
                                   low_bwb.2.df$b_perc_over_65+
                                   low_bwb.2.df$row_values_state+
                                   low_bwb.2.df$row_values_counties)/
  (1+exp(low_bwb.2.df$intercept_col +
           low_bwb.2.df$b_perc_AfAm +
           low_bwb.2.df$b_perc_As +
           low_bwb.2.df$b_perc_AmIn_AlNa +
           low_bwb.2.df$b_perc_Hisp+
           low_bwb.2.df$b_urb+
           low_bwb.2.df$b_perc_female+
           low_bwb.2.df$b_perc_under_18+
           low_bwb.2.df$b_perc_over_65+
           low_bwb.2.df$row_values_state+
           low_bwb.2.df$row_values_counties))

# rank.low_bwb.2 = low_bwb.2.df.summed %>%
#   group_by(state) %>%
#   mutate(my_ranks = order(order(summed, row_name_counties, decreasing=TRUE)))
# 
# rank.low_bwb.2 = rank.low_bwb.2[with(rank.low_bwb.2, order(row_name_counties)), ]



# ############################################################################################################################
# #################################################### poor_health_perc 1 ###################################
poor_health_perc.1.prior <- c(
  prior(normal(1,1), class = Intercept)
)
poor_health_perc.bayes.1 = brm_multiple(poor_health_estimate ~ (1|State/County), data=imputed_Data,
                               family = binomial(link = "logit"), prior=poor_health_perc.1.prior,
                               backend = "rstan", silent = 0, iter=4000)

# poor_health_perc.1.prior <- c(
#   prior(beta(2, 2), class = Intercept)
# )
# poor_health_num = round(poor_health_sample_size * poor_health_percent)
# poor_health_perc.bayes.1 = brm_multiple(poor_health_num ~ (1|State/County), data=imputed_Data,
#                                         family = binomial(link = "logit"), prior=poor_health_perc.1.prior,
#                                         backend = "rstan", silent = 0, iter=4000)
save.image('CE_project.RData')
load('CE_project.RData')
launch_shinystan(poor_health_perc.bayes.1)
# list_of_draws <- extract(testing$fit)

fit_summary.php1 = summary(poor_health_perc.bayes.1$fit)
print(dim(fit_summary.php1$summary))

post_mean_counties <- fit_summary.php1$summary[,c("mean")][55:3195]
post_mean_state <- fit_summary.php1$summary[,c("mean")][4:54]
intercept<- fit_summary.php1$summary[,c("mean")][1]

row_name_counties = names(post_mean_counties)
row_values_counties = unname(post_mean_counties)
state = str_extract(row_name_counties, '(?<=\\[)(.*?)(?=\\_)') #"(?<=\\[)([^\\[]*)(?=_)")
intercept_col = rep(intercept, length(row_name_counties))
poor_health_perc.1.df.counties<- data.frame(state, row_name_counties, row_values_counties, intercept_col)

print(unique(state))
print(dim(poor_health_perc.1.df.counties))

row_name_state = names(post_mean_state)
row_values_state = unname(post_mean_state)
state = str_extract(row_name_state, "(?<=\\[)(.*?)(?=\\,)")
intercept_col = rep(intercept, length(row_name_state))
poor_health_perc.1.df.states<- data.frame(state, row_name_state, row_values_state, intercept_col)

print(unique(row_name_state))
print(unique(state))
print(dim(poor_health_perc.1.df.states))

poor_health_perc.1.df = merge(poor_health_perc.1.df.counties, poor_health_perc.1.df.states,c("state","intercept_col"))
print(dim(poor_health_perc.1.df))

rm(post_mean_counties, row_values_counties, state, intercept_col, poor_health_perc.1.df.counties,
   row_name_state, row_values_state, poor_health_perc.1.df.states)

poor_health_perc.1.df.summed = poor_health_perc.1.df[,c("state", "row_name_counties")]
poor_health_perc.1.df.summed$summed = exp(poor_health_perc.1.df$intercept_col +poor_health_perc.1.df$row_values_state +poor_health_perc.1.df$row_values_counties)/
  (1+exp(poor_health_perc.1.df$intercept_col +poor_health_perc.1.df$row_values_state +poor_health_perc.1.df$row_values_counties))

#################################################### poor_health_perc 2 ####################################
poor_health_perc.2.prior <- c(
  prior(beta(2, 2), class = Intercept),
  prior(normal(0, 1), class = b)
)
poor_health_perc.bayes.2 = brm_multiple(poor_health_estimate ~ perc_AfAm + perc_As + perc_AmIn_AlNa +
                                 perc_Hisp + factor(urb_code_2013) + perc_female + perc_under_18 +
                                 perc_over_65 + (1|State/County), data=imputed_Data,
                               family = binomial(link = "logit"), prior=poor_health_perc.2.prior,
                               backend = "rstan", silent = 0, iter=4000)
save.image('CE_project.RData')
load('CE_project.RData')
launch_shinystan(poor_health_perc.bayes.2)

fit_summary.php2 = summary(poor_health_perc.bayes.2$fit)

print(dim(fit_summary.php2$summary))

intercept<- fit_summary.php2$summary[,c("mean")][1]
b_perc_AfAm<- fit_summary.php2$summary[,c("mean")][2]
b_perc_As<- fit_summary.php2$summary[,c("mean")][3]
b_perc_AmIn_AlNa<- fit_summary.php2$summary[,c("mean")][4]
b_perc_Hisp<- fit_summary.php2$summary[,c("mean")][5]
b_urb_code_20134<- fit_summary.php2$summary[,c("mean")][6]
b_urb_code_20136<- fit_summary.php2$summary[,c("mean")][7]
b_urb_code_20132<- fit_summary.php2$summary[,c("mean")][8]
b_urb_code_20135<- fit_summary.php2$summary[,c("mean")][9]
b_urb_code_20131<- fit_summary.php2$summary[,c("mean")][10]
b_perc_female<- fit_summary.php2$summary[,c("mean")][11]
b_perc_under_18<- fit_summary.php2$summary[,c("mean")][12]
b_perc_over_65<- fit_summary.php2$summary[,c("mean")][13]
post_mean_state <- fit_summary.php2$summary[,c("mean")][16:66]
post_mean_counties <- fit_summary.php2$summary[,c("mean")][67:3207]

row_name_counties = names(post_mean_counties)
row_values_counties = unname(post_mean_counties)
counties = str_extract(row_name_counties, "(?<=_)([^_]+)(?=,)")
counties = gsub('\\.', ' ', counties)
state = str_extract(row_name_counties, '(?<=\\[)(.*?)(?=\\_)') #"(?<=\\[)([^\\[]*)(?=_)")
intercept_col = rep(intercept, length(row_name_counties))
poor_health_perc.2.df.counties<- data.frame(state, counties, row_name_counties, row_values_counties, intercept_col,
                                   b_perc_AfAm, b_perc_As, b_perc_AmIn_AlNa, b_perc_Hisp, b_perc_female,
                                   b_perc_under_18, b_perc_over_65)
poor_health_perc.2.df.counties$b_urb[poor_health_perc.2.df.counties$counties == df$County & df$urb_code_2013 == "1"] <- b_urb_code_20131
poor_health_perc.2.df.counties$b_urb[poor_health_perc.2.df.counties$counties == df$County & df$urb_code_2013 == "2"] <- b_urb_code_20132
poor_health_perc.2.df.counties$b_urb[poor_health_perc.2.df.counties$counties == df$County & df$urb_code_2013 == "4"] <- b_urb_code_20134
poor_health_perc.2.df.counties$b_urb[poor_health_perc.2.df.counties$counties == df$County & df$urb_code_2013 == "5"] <- b_urb_code_20135
poor_health_perc.2.df.counties$b_urb[poor_health_perc.2.df.counties$counties == df$County & df$urb_code_2013 == "6"] <- b_urb_code_20136
poor_health_perc.2.df.counties$b_urb[is.na(poor_health_perc.2.df.counties$b_urb)] <- 0

print(dim(poor_health_perc.2.df.counties))

row_name_state = names(post_mean_state)
row_values_state = unname(post_mean_state)
state = str_extract(row_name_state, "(?<=\\[)(.*?)(?=\\,)")
intercept_col = rep(intercept, length(row_name_state))
poor_health_perc.2.df.states<- data.frame(state, row_name_state, row_values_state, intercept_col)

print(dim(poor_health_perc.2.df.states))

poor_health_perc.2.df = merge(poor_health_perc.2.df.counties, poor_health_perc.2.df.states,c("state","intercept_col")) #by="state")

print(dim(poor_health_perc.2.df))

rm(post_mean_counties, row_values_counties, state, intercept_col, poor_health_perc.2.df.counties,
   row_name_state, row_values_state, poor_health_perc.2.df.states, b_perc_AfAm, b_perc_As,
   b_perc_AmIn_AlNa, b_perc_Hisp, b_perc_female, b_perc_under_18, b_perc_over_65)

poor_health_perc.2.df.summed = poor_health_perc.2.df[,c("state", "row_name_counties")]
poor_health_perc.2.df.summed$summed = exp(poor_health_perc.2.df$intercept_col +
                                   poor_health_perc.2.df$b_perc_AfAm +
                                   poor_health_perc.2.df$b_perc_As +
                                   poor_health_perc.2.df$b_perc_AmIn_AlNa +
                                   poor_health_perc.2.df$b_perc_Hisp+
                                   poor_health_perc.2.df$b_urb+
                                   poor_health_perc.2.df$b_perc_female+
                                   poor_health_perc.2.df$b_perc_under_18+
                                   poor_health_perc.2.df$b_perc_over_65+
                                   poor_health_perc.2.df$row_values_state+
                                   poor_health_perc.2.df$row_values_counties)/
  (1+exp(poor_health_perc.2.df$intercept_col +
           poor_health_perc.2.df$b_perc_AfAm +
           poor_health_perc.2.df$b_perc_As +
           poor_health_perc.2.df$b_perc_AmIn_AlNa +
           poor_health_perc.2.df$b_perc_Hisp+
           poor_health_perc.2.df$b_urb+
           poor_health_perc.2.df$b_perc_female+
           poor_health_perc.2.df$b_perc_under_18+
           poor_health_perc.2.df$b_perc_over_65+
           poor_health_perc.2.df$row_values_state+
           poor_health_perc.2.df$row_values_counties))
# poor_health_perc.2.df.summed = poor_health_perc.2.df[,c("state", "row_name_counties")]
# poor_health_perc.2.df.summed$summed = exp(poor_health_perc.2.df$intercept_col + poor_health_perc.2.df$row_values_state + poor_health_perc.2.df$row_values_counties)/
#   (1+exp(poor_health_perc.2.df$intercept_col + poor_health_perc.2.df$row_values_state + poor_health_perc.2.df$row_values_counties))
# 
# rank.poor_health_perc.2 = poor_health_perc.2.df.summed %>%
#   group_by(state) %>%
#   mutate(my_ranks = order(order(summed, row_name_counties, decreasing=TRUE)))
# 
# rank.poor_health_perc.2[with(rank.poor_health_perc.2, order(row_name_counties)), ]





# Morbidity Rank 1 ----------------------------------------------------------

all.df.summed.1 <- poor_phys_avg.1.df.summed[, c("state", "row_name_counties")]
all.df.summed.1$summed <- mean(poor_phys_avg.1.df.summed$summed+
  poor_ment_avg.1.df.summed$summed +
  low_bwb.1.df.summed$summed +
  poor_health_perc.1.df.summed$summed)

rank.morbidity.1 = all.df.summed.1 %>%
  group_by(state) %>%
  mutate(my_ranks = order(order(summed, row_name_counties, decreasing=TRUE)))

rank.morbidity.1 = rank.morbidity.1[with(rank.morbidity.1, order(row_name_counties)), ]
rank.morbidity.1$true_ranks = df$Morbidity_Rank

sel.morbidity.1 <-rank.morbidity.1 %>% 
  group_by(state) %>%
  do(data.frame(standard.error.loss.morbidity.1=sel(.))) 
sel.morbidity.1 = sel.morbidity.1[with(sel.morbidity.1, order(standard.error.loss.morbidity.1)), ]
sel.morbidity.1

g3 <- ggplot(data = sel.morbidity.1, mapping = aes(x = as.factor(state), y = standard.error.loss.morbidity.1)) +
  geom_bar(stat = "identity") +
  labs(x = "state") +
  ggtitle("Morbidity Rank Mean Squared Error Loss Model 1") +
  xlab("") +
  ylab("Mean Squared Error Loss") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title=element_text(size=20))

print(g3)



# Morbidity Rank 2 --------------------------------------------------------
all.df.summed.2 <- poor_phys_avg.2.df.summed[, c("state", "row_name_counties")]
all.df.summed.2$summed <- mean(poor_phys_avg.2.df.summed$summed+
                                 poor_ment_avg.2.df.summed$summed +
                                 low_bwb.2.df.summed$summed +
                                 poor_health_perc.2.df.summed$summed)

rank.morbidity.2 = all.df.summed.2 %>%
  group_by(state) %>%
  mutate(my_ranks = order(order(summed, row_name_counties, decreasing=TRUE)))

rank.morbidity.2 = rank.morbidity.2[with(rank.morbidity.2, order(row_name_counties)), ]
rank.morbidity.2$true_ranks = df$Morbidity_Rank

sel.morbidity.2 <-rank.morbidity.2 %>% 
  group_by(state) %>%
  do(data.frame(standard.error.loss.morbidity.2=sel(.))) 
sel.morbidity.2 = sel.morbidity.2[with(sel.morbidity.2, order(standard.error.loss.morbidity.2)), ]
sel.morbidity.2

g4 <- ggplot(data = sel.morbidity.2, mapping = aes(x = as.factor(state), y = standard.error.loss.morbidity.2)) +
  geom_bar(stat = "identity") +
  labs(x = "state") +
  ggtitle("Morbidity Rank Mean Squared Error Loss Model 2") +
  xlab("") +
  ylab("Mean Squared Error Loss") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title=element_text(size=20))

print(g4)





# Plotting  model 1 vs 2----------------------------------------------------------------
plot(sel.prem_death.2$standard.error.loss.mortality.2 ~sel.prem_death.1$standard.error.loss.mortality.1,
    main ="Relationship Between Model 1 and Model 2 Mortality Mean Squared Error Loss",
    xlab = "Mean Squared Error Loss Model 1",
    ylab = "Mean Squared Error Loss Model 2", cex=1.5)
abline(lm(sel.prem_death.2$standard.error.loss.mortality.2 ~ sel.prem_death.1$standard.error.loss.mortality.1))

plot(sel.morbidity.2$standard.error.loss.morbidity.2 ~ sel.morbidity.1$standard.error.loss.morbidity.1,
     main ="Relationship Between Model 1 and Model 2 Morbidity Mean Squared Error Loss",
     xlab = "Mean Squared Error Loss Model 1",
     ylab = "Mean Squared Error Loss Model 2", cex=1.5)
abline(lm(sel.morbidity.2$standard.error.loss.morbidity.2 ~ sel.morbidity.1$standard.error.loss.morbidity.1))


# Plotting model vs true --------------------------------------------------
par(mfrow=c(2,2))
plot(rank.premature_deaths.1$my_ranks, rank.premature_deaths.1$true_ranks,
     main ="Mortality Model 1 True Rank vs Estimated Rank",
     xlab = "True Rank",
     ylab = "Estimated Rank")
abline(lm(rank.premature_deaths.1$true_ranks ~ rank.premature_deaths.1$my_ranks))

plot(rank.premature_deaths.2$my_ranks, rank.premature_deaths.2$true_ranks,
     main ="Mortality Model 2 True Rank vs Estimated Rank",
     xlab = "True Rank",
     ylab = "Estimated Rank")
abline(lm(rank.premature_deaths.2$true_ranks ~ rank.premature_deaths.2$my_ranks))

plot(rank.morbidity.1$my_ranks, rank.morbidity.1$true_ranks,
     main ="Morbidity Model 1 True Rank vs Estimated Rank",
     xlab = "True Rank",
     ylab = "Estimated Rank")
abline(lm(rank.morbidity.1$true_ranks ~ rank.morbidity.1$my_ranks))

plot(rank.morbidity.2$my_ranks, rank.morbidity.2$true_ranks,
     main ="Morbidity Model 2 True Rank vs Estimated Rank",
     xlab = "True Rank",
     ylab = "Estimated Rank")
abline(lm(rank.morbidity.2$true_ranks ~ rank.morbidity.2$my_ranks))



# Plotting sel and pop ----------------------------------------------------
sel.prem_death.1 = sel.prem_death.1[with(sel.prem_death.1, order(state)), ]
sel.prem_death.1$population = aggregate(df$Population, by=list(State=df$State), FUN=sum)$x
plot(sel.prem_death.1$population, sel.prem_death.1$standard.error.loss.mortality.1,
     main ="Mortality Model 1 Mean Squared Error Loss vs State Population",
     xlab = "State Population",
     ylab = "Mortality Model 1 Mean Squared Error Loss")
abline(lm(sel.prem_death.1$standard.error.loss.mortality.1 ~ sel.prem_death.1$population))

sel.prem_death.2 = sel.prem_death.2[with(sel.prem_death.2, order(state)), ]
sel.prem_death.2$population = aggregate(df$Population, by=list(State=df$State), FUN=sum)$x
plot(sel.prem_death.2$population, sel.prem_death.2$standard.error.loss.mortality.2,
     main ="Mortality Model 2 Mean Squared Error Loss vs State Population",
     xlab = "State Population",
     ylab = "Mortality Model 2 Mean Squared Error Loss")
abline(lm(sel.prem_death.2$standard.error.loss.mortality.2 ~ sel.prem_death.2$population))

sel.morbidity.1 = sel.morbidity.1[with(sel.morbidity.1, order(state)), ]
sel.morbidity.1$population = aggregate(df$Population, by=list(State=df$State), FUN=sum)$x
plot(sel.morbidity.1$population, sel.morbidity.1$standard.error.loss.morbidity.1,
     main ="Morbidity Model 1 Mean Squared Error Loss vs State Population",
     xlab = "State Population",
     ylab = "Morbidity Model 1 Mean Squared Error Loss")
abline(lm(sel.morbidity.1$standard.error.loss.morbidity.1 ~ sel.morbidity.1$population))

sel.morbidity.2 = sel.morbidity.2[with(sel.morbidity.2, order(state)), ]
sel.morbidity.2$population = aggregate(df$Population, by=list(State=df$State), FUN=sum)$x
plot(sel.morbidity.2$population, sel.morbidity.2$standard.error.loss.morbidity.2,
     main ="Morbidity Model 2 Mean Squared Error Loss vs State Population",
     xlab = "State Population",
     ylab = "Morbidity Model 2 Mean Squared Error Loss")
abline(lm(sel.morbidity.2$standard.error.loss.morbidity.2 ~ sel.morbidity.2$population))


# Plotting sel vs number of counties --------------------------------------
par(mfrow=c(1,1))
sel.prem_death.1 = sel.prem_death.1[with(sel.prem_death.1, order(state)), ]
sel.prem_death.1$countycount = aggregate(df$County, by=list(State=df$State), FUN=length)$x
plot(sel.prem_death.1$countycount, sel.prem_death.1$standard.error.loss.mortality.1,
     main ="Mortality Model 1 Mean Squared Error Loss vs Number of Counties",
     xlab = "Number of Counties",
     ylab = "Mortality Model 1 Mean Squared Error Loss")
abline(lm(sel.prem_death.1$standard.error.loss.mortality.1 ~ sel.prem_death.1$countycount))

sel.prem_death.2 = sel.prem_death.2[with(sel.prem_death.2, order(state)), ]
sel.prem_death.2$countycount = aggregate(df$County, by=list(State=df$State), FUN=length)$x
plot(sel.prem_death.2$countycount, sel.prem_death.2$standard.error.loss.mortality.2,
     main ="Mortality Model 2 Mean Squared Error Loss vs Number of Counties",
     xlab = "Number of Counties",
     ylab = "Mortality Model 2 Mean Squared Error Loss")
abline(lm(sel.prem_death.2$standard.error.loss.mortality.2 ~ sel.prem_death.2$countycount))

sel.morbidity.1 = sel.morbidity.1[with(sel.morbidity.1, order(state)), ]
sel.morbidity.1$countycount = aggregate(df$County, by=list(State=df$State), FUN=length)$x
plot(sel.morbidity.1$countycount, sel.morbidity.1$standard.error.loss.morbidity.1,
     main ="Morbidity Model 1 Mean Squared Error Loss vs Number of Counties",
     xlab = "Number of Counties",
     ylab = "Morbidity Model 1 Mean Squared Error Loss")
abline(lm(sel.morbidity.1$standard.error.loss.morbidity.1 ~ sel.morbidity.1$countycount))

sel.morbidity.2 = sel.morbidity.2[with(sel.morbidity.2, order(state)), ]
sel.morbidity.2$countycount = aggregate(df$County, by=list(State=df$State), FUN=length)$x
plot(sel.morbidity.2$countycount, sel.morbidity.2$standard.error.loss.morbidity.2,
     main ="Morbidity Model 2 Mean Squared Error Loss vs Number of Counties",
     xlab = "Number of Counties",
     ylab = "Morbidity Model 2 Mean Squared Error Loss")
abline(lm(sel.morbidity.2$standard.error.loss.morbidity.2 ~ sel.morbidity.2$countycount))




