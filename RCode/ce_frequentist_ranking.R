## Mixed effects

library(readxl)  # install.packages("readxl") or install.packages("tidyverse")
library(plyr)
library(tibble)
library(data.table)
library(dplyr)
state_name_abbr = read.table(file='~/Documents/CE/state_to_abbr.csv',header = TRUE, sep=',')
cols_to_be_rectified <- names(state_name_abbr)[vapply(state_name_abbr, is.character, logical(1))]
state_name_abbr[,cols_to_be_rectified] <- lapply(state_name_abbr[,cols_to_be_rectified], trimws)
urb = read.table(file='~/Documents/CE/urbanization_classification.csv',header = TRUE, sep=',')

urb = left_join(urb, state_name_abbr, by = "State.Abr.")
drops = c("State.Abr.", "CBSA.title", "CBSA.2012.pop", "County.2012.pop", "X1990.based.code", "X")
urb = urb[ , !(names(urb) %in% drops)]
colnames(urb) <- c("FIPS", "County", "urb_code_2013", "urb_code_2006", "State")
urb$County = gsub("(.*?)\\sCounty$", "\\1", urb$County)
urb[,3] <- sapply(urb[,3],as.factor)
urb[,4] <- sapply(urb[,4],as.factor)

h_ranks = as.data.table(read_excel(path = "~/Documents/CE/county_health_rankings_2013.xls", sheet=3))
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
h_ranks = header.true(h_ranks)
h_ranks[, 1] <- sapply(h_ranks[, 1], as.integer)
colnames(h_ranks) <- c("FIPS", "State", "County", "Mortality_Z_Score", "Mortality_Rank", "Morbidity_Z_Score", "Morbidity_Rank", "Health_Behaviors_Z_Score", "Health_Behaviors_Rank", "Clinical_Care_Z_Score", "Clinical_Care_Rank", "Soc_Econ_Factors_Z_Score", "Soc_Econ_Factors_Rank", "Physical_Env_Z_Score", "Physical_Env_Rank")
h_ranks=h_ranks[!is.na(h_ranks$County),]
h_ranks[,4:15] <- lapply(h_ranks[,4:15],as.numeric)

h_factors = as.data.table(read_excel(path = "~/Documents/CE/county_health_rankings_2013.xls", sheet=4))
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

demographics = as.data.table(read_excel(path = "~/Documents/CE/county_health_rankings_2013.xls", sheet=5))[, (16:61) := NULL]
demographics = header.true(demographics)
colnames(demographics) <- c("FIPS", "State", "County", "Population", "perc_under_18", "perc_over_65", "perc_AfAm", "perc_AmIn_AlNa", "perc_As", "perc_NaHI_PaIs", "perc_Hisp", "perc_NonHispWh", "non_profi_en", "perc_non_profi_en", "perc_female")
demographics=demographics[!is.na(demographics$County),]
demographics[, 1] <- sapply(demographics[, 1], as.integer)
demographics[,4:15] <- lapply(demographics[,4:15],as.numeric)

h_outcomes = as.data.table(read_excel(path = "~/Documents/CE/county_health_rankings_2013.xls", sheet=4))[, (31:138) := NULL]
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

tmp = df[complete.cases(df), ] # complete dataset -- no NAs

### ASK:
# is it fine to only use the complete datasets? this removes abut 2/3 of the data
# it seems strange to me that low birthweight births is modeled as a binomial?
# how to turn a normal into a log normal :|
# the paper turned age, gender, race/ethnicity into categorical variables based on the percentages.  why?


## Mortality
library(lme4)
# imputed_Data <- mice(df, m=3, maxit = 1, method = 'cart', seed = 500)
# hglm2(log(premature_deaths) ~ (1|factor(State)) + (1|factor(County)),
#       offset = log(Population),
#       family = poisson(link = "log"), rand.family = gaussian(link = identity) ,
#       data = tmp)
tmp$State = factor(tmp$State)
tmp$County = factor(tmp$County)
premature_deaths_Rank.lmer1 <- glmer(premature_deaths ~ (1|State/County),
                                     offset = log(Population),
                                     family = poisson(link = "log") ,
                                     data = tmp)
premature_deaths_Rank.lmer2 <- glmer(premature_deaths ~ perc_AfAm + perc_As + perc_AmIn_AlNa +
                                       perc_Hisp + urb_code_2013 + perc_female + perc_under_18 +
                                       perc_over_65 + (1|State/County),
                                     offset = log(Population),
                                     family = poisson(link = "log") ,
                                     data = tmp)
premature_deaths_Rank.lmer1.fe = fixef(premature_deaths_Rank.lmer1)
premature_deaths_Rank.lmer1.re = ranef(premature_deaths_Rank.lmer1)
summary(premature_deaths_Rank.lmer2)

fit_summary.pd1 <- summary(premature_deaths_Rank.lmer1$fit)
premature_deaths.1.df = get_df(fit_summary.pd1)
premature_deaths.1.df.summed = premature_deaths.1.df[,c("state", "row_name_counties")]
premature_deaths.1.df.summed$summed = exp(premature_deaths.1.df$intercept_col +
                                            premature_deaths.1.df$row_values_state +
                                            premature_deaths.1.df$row_values_counties)

premature_deaths.1.rank = premature_deaths.1.df.summed %>%
  group_by(state) %>%
  mutate(my_ranks = order(order(summed, row_name_counties, decreasing=TRUE)))



tmp$low_birthweight_prob = tmp$low_birthweight_perc/100
low_birthweight_births.lmer1 <- hglm2(low_birthweight_prob~ (1|factor(State)) + (1|factor(County)),
                                      offset = log(Population),
                                      family = binomial(link = "logit"), rand.family = gaussian(link = identity) ,
                                      data = tmp)
low_birthweight_births.lmer2 <- hglm2(low_birthweight_prob ~ perc_AfAm + perc_As + perc_AmIn_AlNa +
                                        perc_Hisp + factor(urb_code_2013) + perc_female + perc_under_18 +
                                        perc_over_65 + (1|factor(State)) + (1|factor(County)),
                                      offset = log(Population),
                                      family = binomial(link = "logit"), #rand.family = gaussian(link = identity) ,
                                      data = tmp)
summary(low_birthweight_births.lmer1)
summary(low_birthweight_births.lmer2)

tmp$poor_health_prob = tmp$poor_health_perc/100
poor_health_prob.lmer1 <- hglm2(poor_health_prob ~ (1|factor(State)) + (1|factor(County)),
                                offset = log(Population),
                                family = binomial(link = "logit"), rand.family = gaussian(link = identity) ,
                                data = tmp)
poor_health_prob.lmer2 <- hglm2(poor_health_prob ~ perc_AfAm + perc_As + perc_AmIn_AlNa +
                                  perc_Hisp + factor(urb_code_2013) + perc_female + perc_under_18 +
                                  perc_over_65 + (1|factor(State)) + (1|factor(County)),
                                offset = log(Population),
                                family = binomial(link = "logit"), rand.family = gaussian(link = identity) ,
                                data = tmp)
summary(poor_health_prob.lmer1)
summary(poor_health_prob.lmer2)

poor_phys_health_perc.lmer1 <- hglm2(log(poor_phys_health_avg_over_30_days) ~ (1|factor(State)) + (1|factor(County)),
                                     family = gaussian(link = "identity"), rand.family = gaussian(link = identity) ,
                                     data = tmp)
poor_phys_health_perc.lmer2 <- hglm2(log(poor_phys_health_avg_over_30_days) ~ perc_AfAm + perc_As + perc_AmIn_AlNa +
                                       perc_Hisp + factor(urb_code_2013) + perc_female + perc_under_18 +
                                       perc_over_65 + (1|factor(State)) + (1|factor(County)),
                                     family = gaussian(link = "identity"), rand.family = gaussian(link = identity) ,
                                     data = tmp)
summary(poor_phys_health_perc.lmer1)
summary(poor_phys_health_perc.lmer2)

poor_ment_health_avg_over_30_days.lmer1 <- hglm2(log(poor_ment_health_avg_over_30_days) ~ (1|factor(State)) + (1|factor(County)),
                                                 family = gaussian(link = "identity"), rand.family = gaussian(link = identity) ,
                                                 data = tmp)
poor_ment_health_avg_over_30_days.lmer2 <- hglm2(log(poor_ment_health_avg_over_30_days) ~ perc_AfAm + perc_As + perc_AmIn_AlNa +
                                                   perc_Hisp + factor(urb_code_2013) + perc_female + perc_under_18 +
                                                   perc_over_65 + (1|factor(State)) + (1|factor(County)),
                                                 family = gaussian(link = "identity"), rand.family = gaussian(link = identity) ,
                                                 data = tmp)
summary(poor_ment_health_avg_over_30_days.lmer1)
summary(poor_ment_health_avg_over_30_days.lmer2)


