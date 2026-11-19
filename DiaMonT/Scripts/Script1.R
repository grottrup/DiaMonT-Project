#############
##Load Data##
#############

#Langtidsblodsukker 
library(haven)
adapt_ano_cgm <- read_sas("RawDataSecret/adapt_ano_cgm.sas7bdat",NULL)
View(adapt_ano_cgm)

#Blodprøver for Aalborg
library(haven)
adapt_ano_labka_a <- read_sas("RawDataSecret/adapt_ano_labka_a.sas7bdat", 
                              NULL)
View(adapt_ano_labka_a)

#Inderholder også start HbA1c
library(haven)
adapt_ano_redcap_spg_afs_f <- read_sas("RawDataSecret/adapt_ano_redcap_spg_afs_f.sas7bdat", 
                                       NULL)
View(adapt_ano_redcap_spg_afs_f)

#Samme for falster, men indeholder også start HbA1c
library(haven)
adapt_ano_redcap_spg_ink_f <- read_sas("RawDataSecret/adapt_ano_redcap_spg_ink_f.sas7bdat", 
                                       NULL)
View(adapt_ano_redcap_spg_ink_f)

#Alder, gruppe
library(haven)
adapt_ano_redcap_stamdata <- read_sas("RawDataSecret/adapt_ano_redcap_stamdata.sas7bdat", 
                                      NULL)
View(adapt_ano_redcap_stamdata)


#####################
### Data Cleaning ###
#####################
#Omregening af MaalingTidFraInkl fra tid efter midnat i sekunder til tid på dagen (kl 01.00 = 6*6 (3600))






#####################
### Visualizing   ###
#####################


#Saving a plot
