library(haven)
library(dplyr)

#############
##Load Data##
#############
#Langtidsblodsukker 

adapt_ano_cgm <- read_sas("RawDataSecret/adapt_ano_cgm.sas7bdat",NULL)
View(adapt_ano_cgm)
adapt_ano_cgm_f <- read_sas("RawDataSecret/adapt_ano_cgm_f.sas7bdat", 
                            NULL)
View(adapt_ano_cgm_f)
#Blodprøver for Aalborg
adapt_ano_labka_a <- read_sas("RawDataSecret/adapt_ano_labka_a.sas7bdat", 
                              NULL)
View(adapt_ano_labka_a)
#Spørgeskema fra falster 
adapt_ano_redcap_spg_afs_f <- read_sas("RawDataSecret/adapt_ano_redcap_spg_afs_f.sas7bdat", 
                                       NULL)
View(adapt_ano_redcap_spg_afs_f)
adapt_ano_redcap_spg_ink_f <- read_sas("RawDataSecret/adapt_ano_redcap_spg_ink_f.sas7bdat", 
                                       NULL)
View(adapt_ano_redcap_spg_ink_f)
#Sprøgeskema fra Aalborg
adapt_ano_redcap_spg_afs <- read_sas("RawDataSecret/adapt_ano_redcap_spg_afs.sas7bdat", 
                                     NULL)
View(adapt_ano_redcap_spg_afs)
adapt_ano_redcap_spg_ink <- read_sas("RawDataSecret/adapt_ano_redcap_spg_ink.sas7bdat", 
                                     NULL)
View(adapt_ano_redcap_spg_ink)
#Stamdata Aalborg: Alder, gruppe, køn mm
adapt_ano_redcap_stamdata <- read_sas("RawDataSecret/adapt_ano_redcap_stamdata.sas7bdat", 
                                      NULL)
View(adapt_ano_redcap_stamdata)
adapt_ano_redcap_stamdata2 <- read_sas("RawDataSecret/adapt_ano_redcap_stamdata2.sas7bdat", 
                                       NULL)
View(adapt_ano_redcap_stamdata2)
#Stamdata Falster: Alder, gruppe, køn mm
adapt_ano_redcap_stamdata_f <- read_sas("RawDataSecret/adapt_ano_redcap_stamdata_f.sas7bdat", 
                                        NULL)
View(adapt_ano_redcap_stamdata_f)
adapt_ano_redcap_stamdata_f2 <- read_sas("RawDataSecret/adapt_ano_redcap_stamdata_f2.sas7bdat", 
                                         NULL)
View(adapt_ano_redcap_stamdata_f2)



#####################
### Data Cleaning ###
#####################
#Saml alle fra Falsters hæmoglobin værdier (adapt_ano_redcap_spg_asf_f)

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
  filter(ResultatTekst == ResultatTekst)

Haemoglobin_Vaedier_f_a <- bind_rows(
  Haemoglobin_Vaedier_f_a, adapt_ano_labka_a
)
View(Haemoglobin_Vaedier_f_a)


#Saml målt hæmoglobin data i én kulonne 
Haemoglobin_Vaedier_f_a <- Haemoglobin_Vaedier_f_a %>%
  mutate(Haemoglobinvaerdier = coalesce(
         as.character(SvarKode),
         as.character(ResultatTekst),
         ))
View(Haemoglobin_Vaedier_f_a)

#Saml MaalingTidFraInkl og TidFraInkl i én kulonne
Haemoglobin_Vaedier_f_a <- Haemoglobin_Vaedier_f_a %>%
  mutate(MaalingTidFraInkl_samlet = coalesce(
    as.character(MaalingTidFraInkl),
    as.character(TidFraInkl),
  ))
View(Haemoglobin_Vaedier_f_a)



#sletter unødvedige kulonner 
Haemoglobin_Vaedier_f_a <- Haemoglobin_Vaedier_f_a %>%
  select(-Spoergeskema, - SvarTekst, -NPUKode, -Analysenavn, -Analysekode, -ResultatForholdstegn, -InterntResultatNumerisk, -InterntResultatTekst)
View(Haemoglobin_Vaedier_f_a)



#VIL GERNE INDSÆTTE FÆLLES ENHED - MANGLER SVAR IFT ENHED!!!!!!!!!!! Virker til at aalborg har enhed mmol/mol og falster har mmol/L



summery(Haemoglobin_Vaedier_f_a$haemoglobinvaerdier)


#Indsætter ift deres patientnummer, om de er i hhv. interventionsgruppen eller kontrolgruppen, samt alder, køn og om de er afsluttet før tid eller nej
#Tilføj hospital til stamdata fra Aalborg og til Falster:
Stamdata_aalborg <- adapt_ano_redcap_stamdata %>%
  mutate(By = "Aalborg")
Stamdata_Falster <- adapt_ano_redcap_stamdata_f %>%
  mutate(By = "Falster")
#Samler de to data:
Stamdata_f_a <- bind_rows(
  Stamdata_aalborg,
  Stamdata_Falster
)
View(Stamdata_f_a)

#Overfører stamdata til haemoglobindatasættet:
Haemoglobin_Vaedier_f_a <- Haemoglobin_Vaedier_f_a %>%
  left_join(
    Stamdata_f_a,
    by = c("id_Patient", "By")
  )
View(Haemoglobin_Vaedier_f_a)






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

