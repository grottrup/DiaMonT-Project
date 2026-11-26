################
##Load Library##
################
library(haven)
library(dplyr)


#############
##Load Data##
#############
#Langtidsblodsukker
library(haven)
cgm <- read_sas("Z:/diamont_completed/adapt_ano_cgm.sas7bdat")
View(cgm)

library(haven)
cgm_f <- read_sas("Z:/diamont_completed/adapt_ano_cgm_f.sas7bdat")
View(cgm_f)


#Blodprøver for Aalborg
library(haven)
labka_a <- read_sas("Z:/diamont_completed/adapt_ano_labka_a.sas7bdat")
View(labka_a)


#Spørgeskema fra Falster (Efter langtidsblodsukker)
library(haven)
spg_afs_f <- read_sas("Z:/diamont_completed/adapt_ano_redcap_spg_afs_f.sas7bdat")
View(spg_afs_f)

#Spørgeskema fra Falster (Før langtidsblodsukker)
library(haven)
spg_ink_f <- read_sas("Z:/diamont_completed/adapt_ano_redcap_spg_ink_f.sas7bdat")
View(spg_ink_f)


#Sprøgeskema fra Aalborg
library(haven)
spg_afs <- read_sas("Z:/diamont_completed/adapt_ano_redcap_spg_afs.sas7bdat")
View(spg_afs)

library(haven)
spg_ink <- read_sas("Z:/diamont_completed/adapt_ano_redcap_spg_ink.sas7bdat")
View(spg_ink)


#Stamdata Aalborg: Alder, gruppe, køn mm
library(haven)
stamdata <- read_sas("Z:/diamont_completed/adapt_ano_redcap_stamdata.sas7bdat")
View(stamdata)

stamdata2 <- read_sas("Z:/diamont_completed/adapt_ano_redcap_stamdata2.sas7bdat")
View(stamdata2)

#Stamdata Falster: Alder, gruppe, køn mm
library(haven)
stamdata_f <- read_sas("Z:/diamont_completed/adapt_ano_redcap_stamdata_f.sas7bdat")
View(stamdata_f)

stamdata_f2 <- read_sas("Z:/diamont_completed/adapt_ano_redcap_stamdata_f2.sas7bdat")
View(stamdata_f2)

#####################
### Data Cleaning ###
#####################
#Saml alle fra Falsters hæmoglobinværdier (adapt_ano_redcap_spg_asf_f)

#Lav en kulonne fra med "Aalborg" i "adapt_Ano_labka_a" og en kulonne med "Falser" i "adapt_ano_redcap_spg_asf_f"
labka_a <- labka_a %>%
  mutate(By = "Aalborg")

spg_afs_f <- spg_afs_f %>%
  mutate(By = "Falster")
View(spg_afs_f)

spg_ink_f <- spg_ink_f %>%
  mutate(By = "Falster")
View(spg_ink_f)

#Sørger for kontinuitet i variablerne så man undgår NA værdier
labka_a <- labka_a %>%
  rename(
  TidFraInkl = MaalingTidFraInkl,
  Hb1acResultat = ResultatTekst
)

labka_a <- labka_a %>%
  mutate (Hb1acResultat = as.numeric(Hb1acResultat)
)

View(labka_a)

spg_afs_f <- spg_afs_f %>%
  rename(
    Hb1acResultat = SvarKode
  )
View(spg_afs_f)

spg_ink_f <- spg_ink_f %>%
  rename(
    Hb1acResultat = SvarKode
  )
View(spg_ink_f)

#Nyt datasæt med falsters hæmoglobinværdier:
#Laver et nyt datasæt og tilføjer efter hæmoglobinværdier
Haemoglobin_Vaedier_f_efter <- spg_afs_f %>%
  filter(Spoergsmaal == "Hæmoglobin A1c gruppe; Hb(B) (mmol/L)") %>%
  mutate(Tidspunkt = "Efter",
         Hb1acResultat = as.numeric(Hb1acResultat)
  )
View(Haemoglobin_Vaedier_f_efter)

#Laver et nyt datasæt og tilføjer før hæmoglobinværdier
Haemoglobin_Vaedier_f_før <- spg_ink_f %>%
  filter(Spoergsmaal == "Hæmoglobin A1c gruppe; Hb(B) (mmol/L)") %>%
  mutate(Tidspunkt = "Før",
         Hb1acResultat = as.numeric(Hb1acResultat)
  )
View(Haemoglobin_Vaedier_f_før)


#Overfører Aalborgs hæmoglobinværdier samt Falsters før- og efter værdier til datasættet "Hb1ac_Vaedier_samlet"
Hb1ac_Vaedier_samlet <- bind_rows(
  Haemoglobin_Vaedier_f_efter %>%
    select(id_Patient, TidFraInkl, Hb1acResultat, By, Tidspunkt),
  labka_a %>%
    select(id_Patient, TidFraInkl, Hb1acResultat, By),
  Haemoglobin_Vaedier_f_før %>%
    select(id_Patient, Hb1acResultat, By, Tidspunkt)
)
View(Hb1ac_Vaedier_samlet)

################################################################
###Kristine har rettet i en kopi af Lauras version herned til###
################################################################

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

#Laver en fælles enhed for (mmol/mol) for alle værdier:
Haemoglobin_Vaedier_f_a <- Haemoglobin_Vaedier_f_a %>%
  mutate(ResultatEnhed_faelles = "mmol/mol")
View(Haemoglobin_Vaedier_f_a)


#sletter unødvedige kulonner
Haemoglobin_Vaedier_f_a <- Haemoglobin_Vaedier_f_a %>%
  select(-TidFraInkl, -Spoergeskema,-Spoergsmaal,-SvarKode,-SvarTekst,-MaalingTidFraInkl, -NPUKode,-Analysenavn,-Analysekode,-ResultatForholdstegn,-InterntResultatTekst,-InterntResultatNumerisk,-ResultatTekst,-ResultatEnhed)
View(Haemoglobin_Vaedier_f_a)

#Nu vil jeg gerne udfylde før og efter efter numeriske værdier for Aalborg
#Kontrollér om kulonnen er alfabetisk eller numerisk:
class(Haemoglobin_Vaedier_f_a$MaalingTidFraInkl_samlet)
#Konverter til numerisk:
Haemoglobin_Vaedier_f_a <- Haemoglobin_Vaedier_f_a %>%
  mutate(MaalingTidFraInkl_samlet = as.numeric(MaalingTidFraInkl_samlet))
View(Haemoglobin_Vaedier_f_a)

Haemoglobin_Vaedier_f_a <- Haemoglobin_Vaedier_f_a %>%
  group_by(id_Patient) %>%
  mutate(
    Tidspunkt = case_when(
      By == "Aalborg" & MaalingTidFraInkl_samlet == min(MaalingTidFraInkl_samlet, na.rm = TRUE)~"Før",
      By == "Aalborg" & MaalingTidFraInkl_samlet == max(MaalingTidFraInkl_samlet, na.rm = TRUE)~"Efter",
      TRUE ~ Tidspunkt)
  )%>%
  ungroup()
View(Haemoglobin_Vaedier_f_a)


#Indsætter ift deres patientnummer, om de er i hhv. interventionsgruppen eller kontrolgruppen, samt alder, køn og om de er afsluttet før tid eller nej
#Tilføj hospital til stamdata fra Aalborg og til Falster:
Stamdata_aalborg <- adapt_ano_redcap_stamdata %>%
  mutate(By = "Aalborg")
View(Stamdata_aalborg)
Stamdata_Falster <- adapt_ano_redcap_stamdata_f %>%
  mutate(By = "Falster")
View(Stamdata_Falster)

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

#Alle Falsters "Før" MaalingTidFraInkl har NA-værdier, da de ikke har haft et målt tidspunkt. Derfor laver jeg dem om til 0 (baseline-værdi)
Haemoglobin_Vaedier_f_a <- Haemoglobin_Vaedier_f_a %>%
  mutate(
    MaalingTidFraInkl_samlet = ifelse(Tidspunkt == "Før" & is.na(MaalingTidFraInkl_samlet),0, MaalingTidFraInkl_samlet)
  )
View(Haemoglobin_Vaedier_f_a)

#Giver datasættet et nyt navn (pga stavefejl)
HaemoglobinVaerdier_samlet_f_a <- Haemoglobin_Vaedier_f_a
View(HaemoglobinVaerdier_samlet_f_a)

#Gemmer datasættet
getwd()
saveRDS(HaemoglobinVaerdier_samlet_f_a, file = "../GeneratedData/HaemoglobinVaerdier_samlet_f_a.rds")


####################
###Data beregning###
####################

#Primær
#Opdel intervention VS kontrol
#Opdel i før og efter langtidsblodsukker (baseline vs endpoint)
##Udelukker dem som ikke fra både intervention og kontrol som ikke har fulført forsøget (kode 2)
#Langtidsblodsukker



#Sekundær
#TIR (Sammenligning af kontrol og intervantionsgruppe)







#####################
### Visualizing   ###
#####################


#Saving a plot
