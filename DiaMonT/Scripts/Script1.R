#############
##Load Data##
#############

#Langtidsblodsukker 
library(haven)
adapt_ano_cgm <- read_sas("RawDataSecret/adapt_ano_cgm.sas7bdat",NULL)
View(adapt_ano_cgm)
library(haven)
adapt_ano_cgm_f <- read_sas("RawDataSecret/adapt_ano_cgm_f.sas7bdat", 
                            NULL)
View(adapt_ano_cgm_f)

#Blodprøver for Aalborg
library(haven)
adapt_ano_labka_a <- read_sas("RawDataSecret/adapt_ano_labka_a.sas7bdat", 
                              NULL)
View(adapt_ano_labka_a)

#Spørgeskema fra falster 
library(haven)
adapt_ano_redcap_spg_afs_f <- read_sas("RawDataSecret/adapt_ano_redcap_spg_afs_f.sas7bdat", 
                                       NULL)
View(adapt_ano_redcap_spg_afs_f)
library(haven)
adapt_ano_redcap_spg_ink_f <- read_sas("RawDataSecret/adapt_ano_redcap_spg_ink_f.sas7bdat", 
                                       NULL)
View(adapt_ano_redcap_spg_ink_f)


#Sprøgeskema fra Aalborg
library(haven)
adapt_ano_redcap_spg_afs <- read_sas("RawDataSecret/adapt_ano_redcap_spg_afs.sas7bdat", 
                                     NULL)
View(adapt_ano_redcap_spg_afs)
library(haven)
adapt_ano_redcap_spg_ink <- read_sas("RawDataSecret/adapt_ano_redcap_spg_ink.sas7bdat", 
                                     NULL)
View(adapt_ano_redcap_spg_ink)

#Stamdata Aalborg: Alder, gruppe, køn mm
library(haven)
adapt_ano_redcap_stamdata <- read_sas("RawDataSecret/adapt_ano_redcap_stamdata.sas7bdat", 
                                      NULL)
View(adapt_ano_redcap_stamdata)
library(haven)
adapt_ano_redcap_stamdata2 <- read_sas("RawDataSecret/adapt_ano_redcap_stamdata2.sas7bdat", 
                                       NULL)
View(adapt_ano_redcap_stamdata2)


#Stamdata Falster: Alder, gruppe, køn mm
library(haven)
adapt_ano_redcap_stamdata_f <- read_sas("RawDataSecret/adapt_ano_redcap_stamdata_f.sas7bdat", 
                                        NULL)
View(adapt_ano_redcap_stamdata_f)
library(haven)
adapt_ano_redcap_stamdata_f2 <- read_sas("RawDataSecret/adapt_ano_redcap_stamdata_f2.sas7bdat", 
                                         NULL)
View(adapt_ano_redcap_stamdata_f2)




#####################
### Data Cleaning ###
#####################
#Saml alle fra Falsters hæmoglobin værdier (adapt_ano_redcap_spg_asf_f)
library(dplyr)

#Lav en kulonne fra med "Aalborg" i "adapt_Ano_labka_a" og en kulonne med "Falser" i "adapt_ano_redcap_spg_asf_f"
adapt_ano_labka_a <- adapt_ano_labka_a %>%
  mutate(By = "Aalborg")
View(adapt_ano_labka_a)

adapt_ano_redcap_spg_afs_f <- adapt_ano_redcap_spg_afs_f %>%
  mutate(By = "Falster")
View(adapt_ano_redcap_spg_afs_f)

#Nyt datasæt med falsters hæmoglobinværdier:
Haemoglobin_Vaedier_f_a <- adapt_ano_redcap_spg_afs_f %>%
  filter(Spoergsmaal == "Hæmoglobin A1c gruppe; Hb(B) (mmol/L)")
View(Haemoglobin_Vaedier_f_a)

#Overfører Aalborgs hæmoglobinværdier til det nye datasæt
Haemoglobin_Vaedier_a <- adapt_ano_labka_a %>%
  filter(ResultatTekst=resutattekst)

Haemoglobin_Vaedier_f_a <- bind_rows(
  Haemoglobin_Vaedier_f_a, adapt_ano_labka_a
)
View(Haemoglobin_Vaedier_f_a)


#Saml data i én kulonne 
Haemoglobin_Vaedier_f_a <- Haemoglobin_Vaedier_f_a %>%
  mutate(haemoglobinvaerdier = coalesce(
         as.character(SvarKode),
         as.character(ResultatTekst),
         ))
View(Haemoglobin_Vaedier_f_a)

#sletter unødvedige kulonner 
Haemoglobin_Vaedier_f_a <- Haemoglobin_Vaedier_f_a %>%
  select(-TidFraInkl, -Spoergeskema, -Spoergsmaal, - SvarTekst, -MaalingTidFraInkl, -NPUKode, -Analysenavn, -Analysekode, -ResultatForholdstegn, -ResultatEnhed, -InterntResultatNumerisk, -InterntResultatTekst)
View(Haemoglobin_Vaedier_f_a)

summery(Haemoglobin_Vaedier_f_a$haemoglobinvaerdier)



#Omregening af MaalingTidFraInkl fra tid efter midnat i sekunder til tid på dagen (kl 01.00 = 6*6 (3600))





####################
###Data beregning###
####################

#Primær 
#Opdel intervention VS kontrol
##Udelukker dem som ikke fra både intervention og kontrol som ikke har fulført forsøget (kode 2)
#Langtidsblodsukker



#Sekundær 
#TIR (Sammenligning af kontrol og intervantionsgruppe)







#####################
### Visualizing   ###
#####################


#Saving a plot
