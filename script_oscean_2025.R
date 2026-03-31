
# MAJ mars 2026 ----

# Library ----
#library(plyr)
library(tidyverse)
library(knitr)
library(kableExtra)
# library(lubridate)
# library(RcppRoll)
# library(DT)
# library(readxl)
# library(dbplyr)
# library(RPostgreSQL)
# library(rsdmx)
library(sf)
#library(stringi)
library(terra)
library(plotly)
library(base)

# Import des données ----

controles <-
  sf::read_sf(dsn = "data/point_ctrl_BZH_20251231_wgs84.gpkg") %>%
  st_transform(crs = 2154) 

infractions <-
  sf::read_sf(dsn = "data/localisation_infrac_FAITS_20260312.shp") %>%
  st_transform(crs = 2154) 

infractions_ng <- data.table::fread(file = "data/infrac_FAITS_non_localises_20260312.csv",
                                   encoding = "UTF-8")

thematiques <- data.table::fread(file = "data/thematiques.csv",
                                         encoding = "Latin-1") %>%
  rename(thematique = "CATEGORIE DEFINITIVE")

# Formatage et fusion des données pour obtenir la table intervention ----

# Intervention = contrôles + infractions - doublons

## controles ----

controles_bzh_annee <- controles %>%
  mutate(entite = entit_ctrl,
         date = as.Date(date_ctrl),
         type = 'controle',
         annee = substr(date,1,4)) %>%
  dplyr::left_join(thematiques, 
                   by = c("theme" = "theme")) %>%
  select(entite, date, annee, type, resultat, thematique) %>%
  filter(annee == '2025' & entite %in% c('SD22','SD29','SD35','SD56', 'DRB', 'PNMI', 'BMI-NO' ))

## infractions ----

### Formatage des infractions ----

infractions_bzh_annee <- infractions %>%
  mutate(type = 'infraction',
         date = as.Date(date_saisi),
         resultat = 'infraction',
         annee = substr(date,1,4)) %>%
  dplyr::left_join(thematiques, 
                   by = c("theme" = "theme")) %>%
  filter(annee == '2025' & entite %in% c('SD22','SD29','SD35','SD56', 'DRB', 'PNMI', 'BMI-NO' )) %>%
  select(entite, date, annee, type, resultat, suite_ctrl,thematique) 

### Export/import des la couche pour exclure géographiquement les infractions hors territoire ----

sf::write_sf(obj = infractions_bzh_annee, dsn = "data/outputs/infractions_bzh_annee.gpkg")

infractions_bzh_annee <-
  sf::read_sf(dsn = "data/outputs/infractions_bzh_annee.gpkg")

## infractions_ng ----

infractions_ng_bzh_annee <- infractions_ng %>%
  mutate(type = 'infraction_ng',
         date = as.Date(date_saisine),
         resultat = "infraction", 
         annee = substr(date,1,4)) %>%
  dplyr::left_join(thematiques, 
                   by = c("theme" = "theme")) %>%
  select(entite, date, annee, type, resultat, thematique) %>%
  filter(annee == '2025' & entite %in% c('SD22','SD29','SD35','SD56', 'DRB', 'PNMI', 'BMI-NO' ))

infractions_totales <-
rbind(infractions_bzh_annee %>%
        select(entite, type, thematique)%>%
        st_drop_geometry() %>%
        as.data.frame(),
      infractions_ng_bzh_annee %>%
        select(entite, type, thematique)%>%
        st_drop_geometry() %>%
        as.data.frame())

## fusion des données et suppression des doublons controle_nc/infractions ----

interventions <- dplyr::bind_rows(controles_bzh_annee, infractions_bzh_annee, infractions_ng_bzh_annee) %>%
  filter(is.na(suite_ctrl) | suite_ctrl != 'Oui') %>%
  mutate(resultat_vf = case_when(
    type == 'infraction' ~ 'Infraction',
    type == 'infraction_ng' ~ 'Infraction non-géoréférencée',
    type == 'controle'  & resultat == 'Conforme' ~ 'Contrôle conforme',
    type == 'controle'  & resultat == 'Manquement' ~ 'Contrôle non conforme',
    type == 'controle'  & resultat == 'Infraction' ~ 'Contrôle non conforme',
    type == 'controle'  & resultat == 'Manquement et infraction' ~ 'Contrôle non conforme',
    type == 'controle'  & is.na(resultat) ~ 'Contrôle non-renseigné'))

table_interventions <- interventions

# Tables de synthese des interventions ----

## synthèse des thématiques d'interventions selon le SD ----

tot_22 <- table_interventions %>%
  filter(entite == 'SD22') %>%
  group_by() %>%
  summarise(Intervention = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "SD22")

tot_29 <- table_interventions %>%
  filter(entite == 'SD29') %>%
  group_by() %>%
  summarise(Intervention = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "SD29")

tot_35 <- table_interventions %>%
  filter(entite == 'SD35') %>%
  group_by() %>%
  summarise(Intervention = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "SD35")

tot_56 <- table_interventions %>%
  filter(entite == 'SD56') %>%
  group_by() %>%
  summarise(Intervention = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "SD56")

tot_pnmi <- table_interventions %>%
  filter(entite == 'PNMI') %>%
  group_by() %>%
  summarise(Intervention = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "PNMI")

tot_bmi <- table_interventions %>%
  filter(entite == 'BMI-NO') %>%
  group_by() %>%
  summarise(Intervention = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "BMI-NO")

tot_dr <- table_interventions %>%
  group_by() %>%
  summarise(Intervention = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "Total DR")

total_interventions_entites <-
  dplyr::bind_rows(tot_22, tot_29, tot_35, tot_56, tot_pnmi, tot_bmi, tot_dr)

thematique_22 <- table_interventions %>%
  filter(entite == 'SD22') %>%
  group_by(thematique) %>%
  summarise(nb_22 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = thematique,
              values_from = nb_22) %>%
  mutate(entite = "SD22")

thematique_29 <- table_interventions %>%
  filter(entite == 'SD29') %>%
  group_by(thematique) %>%
  summarise(nb_29 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = thematique,
              values_from = nb_29) %>%
  mutate(entite = "SD29")

thematique_35 <- table_interventions %>%
  filter(entite == 'SD35') %>%
  group_by(thematique) %>%
  summarise(nb_35 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = thematique,
              values_from = nb_35) %>%
  mutate(entite = "SD35")

thematique_56 <- table_interventions %>%
  filter(entite == 'SD56') %>%
  group_by(thematique) %>%
  summarise(nb_56 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = thematique,
              values_from = nb_56) %>%
  mutate(entite = "SD56")

thematique_pnmi <- table_interventions %>%
  filter(entite == 'PNMI') %>%
  group_by(thematique) %>%
  summarise(nb_pnmi = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = thematique,
              values_from = nb_pnmi) %>%
  mutate(entite = "PNMI")

thematique_bmi <- table_interventions %>%
  filter(entite == 'BMI-NO') %>%
  group_by(thematique) %>%
  summarise(nb_bmi = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = thematique,
              values_from = nb_bmi) %>%
  mutate(entite = "BMI-NO")

thematique_dr <- table_interventions %>%
  group_by(thematique) %>%
  summarise(nb_dr = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = thematique,
              values_from = nb_dr) %>%
  mutate(entite = "Total DR")

thematiques_interventions_entites <-
  dplyr::bind_rows(thematique_22, thematique_29, thematique_35, thematique_56, thematique_pnmi, thematique_bmi, thematique_dr)

synth_thematique_entite <- table_interventions %>%
  group_by(entite) %>%
  summarise()%>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  rbind("Total DR") %>%
  mutate(entite = fct_relevel(entite,
                              "BMI-NO",
                              "PNMI",
                              "SD22",
                              "SD29",
                              "SD35",
                              "SD56", 
                              "Total DR")) %>%
  dplyr::left_join(total_interventions_entites, by = c("entite" = "entite")) %>%
  dplyr::left_join(thematiques_interventions_entites, by = c("entite" = "entite")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rename(Service = entite)

## Synthèse des services d'intervention selon la thématique ----

entite_thematique_dr <- table_interventions %>%
  group_by(thematique) %>%
  summarise("Total DR" = n()) %>%
  st_drop_geometry() %>%
  as.data.frame()

entite_thematique_22 <- table_interventions %>%
  filter(entite == 'SD22') %>%
  group_by(thematique) %>%
  summarise(SD22 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() 

entite_thematique_29 <- table_interventions %>%
  filter(entite == 'SD29') %>%
  group_by(thematique) %>%
  summarise(SD29 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() 

entite_thematique_35 <- table_interventions %>%
  filter(entite == 'SD35') %>%
  group_by(thematique) %>%
  summarise(SD35 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() 

entite_thematique_56 <- table_interventions %>%
  filter(entite == 'SD56') %>%
  group_by(thematique) %>%
  summarise(SD56 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() 

entite_thematique_pnmi <- table_interventions %>%
  filter(entite == 'PNMI') %>%
  group_by(thematique) %>%
  summarise(PNMI = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() 

entite_thematique_bmi <- table_interventions %>%
  filter(entite == 'BMI-NO') %>%
  group_by(thematique) %>%
  summarise("BMI-NO" = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() 

synth_entite_thematique <- table_interventions %>%
  group_by(thematique) %>%
  summarise()%>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::left_join(entite_thematique_22, by = c("thematique" = "thematique")) %>%
  dplyr::left_join(entite_thematique_29, by = c("thematique" = "thematique")) %>%
  dplyr::left_join(entite_thematique_35, by = c("thematique" = "thematique")) %>%
  dplyr::left_join(entite_thematique_56, by = c("thematique" = "thematique")) %>%
  dplyr::left_join(entite_thematique_pnmi, by = c("thematique" = "thematique")) %>%
  dplyr::left_join(entite_thematique_bmi, by = c("thematique" = "thematique")) %>%
  dplyr::left_join(entite_thematique_dr, by = c("thematique" = "thematique")) %>%
  mutate_all(~replace(., is.na(.), 0)) 

## synthèse des natures et types d'intervention selon le SD ----

conformite_22 <- table_interventions %>%
  filter(entite == 'SD22') %>%
  group_by(resultat_vf) %>%
  summarise(nb_22 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_22) %>%
  mutate(entite = "SD22")

conformite_29 <- table_interventions %>%
  filter(entite == 'SD29') %>%
  group_by(resultat_vf) %>%
  summarise(nb_29 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_29) %>%
  mutate(entite = "SD29")

conformite_35 <- table_interventions %>%
  filter(entite == 'SD35') %>%
  group_by(resultat_vf) %>%
  summarise(nb_35 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_35) %>%
  mutate(entite = "SD35")

conformite_56 <- table_interventions %>%
  filter(entite == 'SD56') %>%
  group_by(resultat_vf) %>%
  summarise(nb_56 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_56) %>%
  mutate(entite = "SD56")

conformite_pnmi <- table_interventions %>%
  filter(entite == 'PNMI') %>%
  group_by(resultat_vf) %>%
  summarise(nb_pnmi = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_pnmi) %>%
  mutate(entite = "PNMI")

conformite_bmi <- table_interventions %>%
  filter(entite == 'BMI-NO') %>%
  group_by(resultat_vf) %>%
  summarise(nb_bmi = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_bmi) %>%
  mutate(entite = "BMI-NO")

conformite_dr <- table_interventions %>%
  group_by(resultat_vf) %>%
  summarise(nb_dr = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_dr) %>%
  mutate(entite = "Total DR")

controle_22 <- table_interventions %>%
  filter(type == 'controle' & entite == 'SD22') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "SD22")

controle_29 <- table_interventions %>%
  filter(type == 'controle' & entite == 'SD29') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "SD29")

controle_35 <- table_interventions %>%
  filter(type == 'controle' & entite == 'SD35') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "SD35")

controle_56 <- table_interventions %>%
  filter(type == 'controle' & entite == 'SD56') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "SD56")

controle_pnmi <- table_interventions %>%
  filter(type == 'controle' & entite == 'PNMI') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "PNMI")

controle_bmi <- table_interventions %>%
  filter(type == 'controle' & entite == 'BMI-NO') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "BMI-NO")

controle_dr <- table_interventions %>%
  filter(type == 'controle') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "Total DR")

controles_entites <-
  dplyr::bind_rows(controle_22, controle_29, controle_35, controle_56, controle_pnmi, controle_bmi, controle_dr)

conformites_interventions_entites <-
  dplyr::bind_rows(conformite_22, conformite_29, conformite_35, conformite_56, conformite_pnmi, conformite_bmi, conformite_dr)

synth_conformite_entite <- table_interventions %>%
  group_by(entite) %>%
  summarise() %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  rbind("Total DR") %>%
  mutate(entite = fct_relevel(entite,
                              "BMI-NO",
                              "PNMI",
                              "SD22",
                              "SD29",
                              "SD35",
                              "SD56",
                              "Total DR")) %>%
  dplyr::left_join(total_interventions_entites, by = c("entite" = "entite")) %>%
  dplyr::left_join(controles_entites, by = c("entite" = "entite")) %>%
  dplyr::left_join(conformites_interventions_entites, by = c("entite" = "entite")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rename(Service = entite)

## Tables complémentaires ----

synth_infractions_ng_service <- infractions_ng_bzh_annee %>% 
  st_drop_geometry() %>%
  as.data.frame() %>%
  group_by(entite, thematique) %>%
  tally() %>%
  tidyr::pivot_wider(values_from = n, names_from = "entite",   values_fill = 0)

synthese_date_service <- table_interventions %>% 
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(mois = substr(date,6,7)) %>%
  group_by(entite, mois) %>% 
  tally() %>% 
  tidyr::pivot_wider(values_from = n, names_from = "entite",   values_fill = 0)

synthese_date_thematique <- table_interventions %>% 
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(mois = substr(date,6,7)) %>%
  group_by(thematique, mois) %>% 
  tally() %>% 
  tidyr::pivot_wider(values_from = n, names_from = "thematique",   values_fill = 0)

# histogrammes ----

histo_infractions_ng_service <-
  plot_ly(synth_infractions_ng_service, 
          type = 'bar', 
          x = ~thematique, 
          y = ~SD56, color = I("darkred"), name = "SD56") %>% 
  add_bars(y = ~SD35, color = I("red"), name = "SD35") %>% 
  add_bars(y = ~SD29, color = I("#fa99a3"), name = "SD29") %>% 
  add_bars(y = ~SD22, color = I("#ffdadd"), name = "SD22") %>% 
  add_bars(y = ~PNMI, color = I("#eb9eff"), name = "PNMI") %>% 
  add_bars(y = ~`BMI-NO`, color = I("#8f0fb0"), name = "BMI-NO")  %>% 
  layout(yaxis = list(title = "Nombre d'interventions"),barmode = 'stack')  %>%
  layout(title = "Nombre d'interventions non-géoréférencées selon la tématique et le service",
         xaxis = list(title = "Thématique"))

histo_infractions_ng_service

histo_conformite_service_interventions <-
  ggplot(data = table_interventions, 
         aes(x = entite, fill = resultat_vf)) +
  geom_bar(position = "stack") + 
  scale_fill_manual(values = c("lightgreen","lightgrey", "red","#ae001f", "#700014"))+
  labs(x = "Service",
       y = "Nombre d'interventions",
       title = "Nature et conformité des interventions réalisées selon le service", 
       subtitle = "1er Janvier 2025 - 31 Décembre 2025", 
       fill = "Nature et conformité")+
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1))

histo_conformite_service_interventions

histo_conformite_thematique_interventions <-
  ggplot(data = table_interventions, 
         aes(x = thematique, fill = resultat_vf)) +
  geom_bar(position = "stack") + 
  scale_fill_manual(values = c("lightgreen","lightgrey", "red","#ae001f", "#700014"))+
  labs(x = "Thématiques",
       y = "Nombre d'interventions",
       title = "Nature et conformité des interventions réalisées selon la thématique", 
       subtitle = "1er Janvier 2025 - 31 Décembre 2025", 
       fill = "Nature et conformité")+
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1))

histo_conformite_thematique_interventions

histo_thematique_service_interventions <-
  plot_ly(synth_thematique_entite %>%
            filter(Service != 'Total DR'),
          type = 'bar', 
          x = ~Service, 
          y = ~`Hors thème`, color = I("grey"), name = "Hors thème") %>%
  add_bars(y = ~`Police en environnement marin`, color = I("#ec6270"), name = "Police en environnement marin") %>% 
  add_bars(y = ~`Pêche maritime`, color = I("#ef7bf1"), name = "Pêche maritime") %>% 
  add_bars(y = ~`Sécurité et police de la chasse`, color = I("#ae7e3b"), name = "Sécurité et police de la chasse") %>% 
  add_bars(y = ~`Espèces`, color = I("#5dff2c"), name = "Espèces") %>% 
  add_bars(y = ~`Espaces protégés`, color = I("#0da404"), name = "Espaces protégés") %>% 
  add_bars(y = ~`Pêche en eau douce`, color = I("#01ffff"), name = "Pêche en eau douce") %>% 
  add_bars(y = ~`Préservation des milieux aquatiques`, color = I("#7bbfff"), name = "Préservation des milieux aquatiques") %>% 
  add_bars(y = ~`Gestion qualitative de l'eau`, color = I("#0e01ff"), name = "Gestion qualitative de l'eau") %>% 
  add_bars(y = ~`Gestion quantitative de l'eau`, color = I("#060082"), name = "Gestion quantitative de l'eau") %>% 
  layout(yaxis = list(title = "Nombre d'interventions"),barmode = 'stack')  %>%
  layout(title = "Thématique des interventions réalisées selon le service en 2025",
         xaxis = list(title = "service"))

histo_thematique_service_interventions

histo_service_thematique_interventions <-
  plot_ly(synth_entite_thematique,
          type = 'bar', 
          x = ~thematique, 
          y = ~`SD22`, color = I("lightblue"), name = "SD22") %>%
  add_bars(y = ~`SD29`, color = I("#18d0f0"), name = "SD29") %>% 
  add_bars(y = ~`SD35`, color = I("blue"), name = "SD35") %>% 
  add_bars(y = ~`SD56`, color = I("darkblue"), name = "SD56") %>% 
  add_bars(y = ~`PNMI`, color = I("#5dff2c"), name = "PNMI") %>% 
  add_bars(y = ~`BMI-NO`, color = I("#0da404"), name = "BMI-NO") %>% 
  layout(yaxis = list(title = "Nombre d'interventions"),barmode = 'stack')  %>%
  layout(title = "Service des interventions réalisées selon la thématique en 2025",
         xaxis = list(title = "service"))

histo_service_thematique_interventions

histo_dynamique_date_service <-
  plot_ly(synthese_date_service, 
          type = 'bar', 
          x = ~mois, 
          y = ~SD56, color = I("darkblue"), name = "SD56") %>% 
  add_bars(y = ~SD35, color = I("blue"), name = "SD35") %>% 
  add_bars(y = ~SD29, color = I("#18d0f0"), name = "SD29") %>% 
  add_bars(y = ~SD22, color = I("lightblue"), name = "SD22") %>% 
  add_bars(y = ~PNMI, color = I("#5dff2c"), name = "PNMI") %>% 
  add_bars(y = ~`BMI-NO`, color = I("#0da404"), name = "BMI-NO")  %>% 
  layout(yaxis = list(title = "Nombre d'interventions"),barmode = 'stack')  %>%
  layout(title = "Nombre d'interventions réalisées par mois selon le service",
         xaxis = list(title = "Mois de l'année"))

histo_dynamique_date_service

histo_dynamique_date_thematique <-
  plot_ly(synthese_date_thematique, 
          type = 'bar', 
          x = ~mois, 
          y = ~`Hors thème`, color = I("grey"), name = "Hors thème") %>%
  add_bars(y = ~`Police en environnement marin`, color = I("#ec6270"), name = "Police en environnement marin") %>% 
  add_bars(y = ~`Pêche maritime`, color = I("#ef7bf1"), name = "Pêche maritime") %>% 
  add_bars(y = ~`Sécurité et police de la chasse`, color = I("#ae7e3b"), name = "Sécurité et police de la chasse") %>% 
  add_bars(y = ~`Espèces`, color = I("#5dff2c"), name = "Espèces") %>% 
  add_bars(y = ~`Espaces protégés`, color = I("#0da404"), name = "Espaces protégés") %>% 
  add_bars(y = ~`Pêche en eau douce`, color = I("#01ffff"), name = "Pêche en eau douce") %>% 
  add_bars(y = ~`Préservation des milieux aquatiques`, color = I("#7bbfff"), name = "Préservation des milieux aquatiques") %>% 
  add_bars(y = ~`Gestion qualitative de l'eau`, color = I("#0e01ff"), name = "Gestion qualitative de l'eau") %>% 
  add_bars(y = ~`Gestion quantitative de l'eau`, color = I("#060082"), name = "Gestion quantitative de l'eau") %>% 
  layout(yaxis = list(title = "Nombre d'interventions"),barmode = 'stack')  %>%
  layout(title = "Nombre d'interventions réalisées par mois selon la thématique",
         xaxis = list(title = "Mois de l'année"))

histo_dynamique_date_thematique

# Sauvegarde des données ----

save(infractions_bzh_annee,
     controles_bzh_annee,
     infractions_totales,
     table_interventions,
     synth_thematique_entite,
     synth_entite_thematique,
     synth_conformite_entite,
     synth_infractions_ng_service,
     synthese_date_service,
     synthese_date_thematique,
     histo_infractions_ng_service,
     histo_conformite_service_interventions,
     histo_conformite_thematique_interventions,
     histo_thematique_service_interventions,
     histo_service_thematique_interventions,
     histo_dynamique_date_service,
     histo_dynamique_date_thematique,
     file = "data/outputs/oscean_2025.RData")

# chargement des résultats

load(file = "data/outputs/oscean_2025.RData")

