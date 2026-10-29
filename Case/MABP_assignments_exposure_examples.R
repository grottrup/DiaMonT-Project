



library(lubridate)
library(dplyr)
library(ggplot2)
library(medisr)
library(VGAM)


data <- medisr::mabp
data <- tibble::tibble(data)

?medisr::mabp
#dataclean
# sex, diabetes, hypertension og eventuelt NIHSS
# 1) Mange af variablene i datasættet er skrevet som numeriske værdier e.g. sex=0/1. Lav nogle nye character variable, e.g. sex_chr=Male/Female

data <- data %>% 
  mutate(
    Sex_=ifelse(sex==1,yes="Female",no="Male"),
    Diabetes_=ifelse(Diabetes==1,yes="Diabetes",no="No diabetes"),
    Hypertension_=ifelse(Hyperton==1, yes="Hypertension", no="No hypertension")
  )



data <- data %>% 
  mutate(
    below70mmHg_cum = rowSums(70 > select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE),
    below70mmHg = 2 < below70mmHg_cum,
    primary_outcome_good_functional_outcome = mrs_3m < 3
  )

data

### laura

###############
### Library ###
###############

library(lubridate)
library(dplyr)
library(ggplot2)
library(medisr)
library(VGAM)

#################
### Load data ###
#################
medisr::medisr_init_project()
data <- medisr::mabp_data
data <- tibble::tibble(data)

######################
###Data Discription###
######################
?medisr::mabp

#####################
### Data Cleaning ###
#####################
#Sex (1=Female, 0=Male)
#Diabetes (1=yes,0=no)
#Hypertension (1=yes, 0=no)

data <- data |>
  mutate(
    Sex_=ifelse(sex==1,yes="Female",no="Male"),
    Diabetes_=ifelse(Diabetes==1,yes="Diabetes",no="No diabetes"),
    Hypertension_=ifelse(Hyperton==yes="Hypertension",no="No hypertension" )
  )

############
###Primær###
############
mabp:data <- data |>
  mutate(
    below70mmHg_cum = rowSums(70 > select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE),
    below70mmHg = 2 < below70mmHg_cum,
    good_functional_outcome = mrs_3m < 3
  )

#RR (Relativ risiko)
mean(data$good_functional_outcome[data$below70mmHg == TRUE])/mean(data$good_functional_outcome[data$below70mmHg == FALSE])

#RD (risk difference)
mean(data$good_functional_outcome[data$below70mmHg == TRUE])-mean(data$good_functional_outcome[data$below70mmHg == FALSE])

#OD (Odds Ratio)
(mean(data$good_functional_outcome[data$below70mmHg == TRUE])/(1-mean(data$good_functional_outcome[data$below70mmHg == TRUE])))/(mean(data$good_functional_outcome[data$below70mmHg == FALSE])/(1-mean(data$good_functional_outcome[data$below70mmHg == FALSE])))

#Binær tabel
table(data$below70mmHg,data$good_functional_outcome)



###############
###Sekundær####
###############













