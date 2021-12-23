install.packages('tidyverse')
install.packages('skimr')
install.packages('janitor')

library(tidyverse)
library(skimr)
library(janitor)


#COVID19 Vaccine Statewise 

state_vaccine_df = read_csv('covid_vaccine_statewise.csv')
View(state_vaccine_df)


#formatting column names
colnames(state_vaccine_df) <- make_clean_names(colnames(state_vaccine_df))
state_vaccine_df[is.na(state_vaccine_df)] = 0

#selecting relevent columns from dataframe
state_wise_vaccination_df <- state_vaccine_df %>% select(updated_on, state, total_doses_administered, sites, first_dose_administered, 
                                                         second_dose_administered, male_doses_administered, female_doses_administered, 
                                                         transgender_doses_administered, covaxin_doses_administered, covi_shield_doses_administered, 
                                                         sputnik_v_doses_administered, aefi)


#aggregate values across states.
state_wise_df <- state_vaccine_df %>% filter(!is.na(total_doses_administered)) %>% 
  group_by(state) %>% summarise(
    total_doses = max(total_doses_administered, na.rm = TRUE),
    total_first_doses = max(first_dose_administered, na.rm = TRUE),
    total_second_doses = max(second_dose_administered, na.rm = TRUE),
    vaccinated_male = max(male_doses_administered, na.rm = TRUE),
    vaccinated_female = max(female_doses_administered, na.rm = TRUE),
    vaccinated_transgender = max(transgender_doses_administered, na.rm=TRUE),
    covishield_vaccinated = max(covi_shield_doses_administered, na.rm = TRUE),
    covaxin_vaccinated = max(covaxin_doses_administered, na.rm=TRUE),
    sputnik_vaccinated = max(sputnik_v_doses_administered, na.rm = TRUE)
    )


state_wise_df<- state_wise_df[-c(14),]


#saving dataframe to csv file.
write_csv(state_wise_df, "state_vaccination.csv")



#reading covid infection data
covid_df <- read_csv('covid_19_india.csv')


#select relevent columns from dataframe.
covid_data_df <- covid_df %>% select(Date, `State/UnionTerritory`, Cured, Deaths, Confirmed)
View(covid_data_df)

#formatting column names.
colnames(covid_data_df) <- make_clean_names(colnames(covid_data_df))
View(covid_data_df)

#renaming column names.
covid_data_df <- covid_data_df %>% rename(state = state_union_territory)
View(covid_data_df)


#sorting state alphabetically.
covid_data_df <- covid_data_df %>% arrange(state)
covid_data_df[2523:2524,2] <- 'Bihar'
covid_data_df[10023,2] <- 'Madhya Pradesh'
covid_data_df[10544,2] <- 'Maharashtra'

#filter out unassigned cases from dataframe.
covid_data_df <- covid_data_df %>% filter(state != 'Unassigned')


#aggregate values across states.
aggr_covid_data <- covid_data_df %>% group_by(state) %>% summarise(total_cured=max(cured),
                                                                   total_deaths=max(deaths),
                                                                   total_confirmed=max(confirmed))


View(aggr_covid_data)

state <- state_wise_df[1]
state <- state[-c(14),]
population_count <- c(380581,49577103,1383727,31205576,
                      104099452,1055450,25545198,585764,
                      16787941,1458545,60439692,25351462,
                      6864602,12267032,32988134,61095297,
                      33406061,274000,64473,72626809,
                      112374333,2570390,2966889,1097206,
                      1978502,41974219,1247953,27743338,
                      68548437,610577,72147030,35003674,
                      3673917,199812341,10086292,91276115) 
population <- data.frame(state, population_count)

state_wise_df <- cbind(state_wise_df, population)


#write dataframe to csv file.
write_csv(aggr_covid_data, 'covid_cases_summary.csv')




