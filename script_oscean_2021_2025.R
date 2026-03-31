
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

# Import et formatage des données ----

thematiques <- data.table::fread(file = "data/thematiques.csv",
                                 encoding = "Latin-1") %>%
  rename(thematique = "CATEGORIE DEFINITIVE")

## controles ----

### Formatage des données millésimées ----

# ATTENTION les intitulés de champs peuvent évoluer au fil des millésimes.
# Une adaptation à ces évolution peut être nécessaire.

controles_n <-
  sf::read_sf(dsn = "data/point_ctrl_BZH_20251231_wgs84.gpkg") %>%
  rename("geometry" = "geom") %>%
  st_transform(crs = 2154) %>%
  filter(entit_ctrl %in% c('SD22','SD29','SD35','SD56', 'DRB', 'PNMI', 'BMI-NO' ) ) %>%
    mutate(entite = entit_ctrl,
         date = as.Date(date_ctrl),
         type = 'controle',
         annee = substr(date,1,4),) %>%
  dplyr::left_join(thematiques, 
                   by = c("theme" = "theme")) %>%
  select(entite, date, annee, type, resultat, thematique, nom_commun, insee_comm) 

controles_n_1 <-
  sf::read_sf(dsn = "data/point_ctrl_2024_wgs84.shp") %>%
  st_transform(crs = 2154) %>%
  filter(entit_ctrl %in% c('SD22','SD29','SD35','SD56', 'DRB', 'PNMI', 'BMI-NO' ) )%>%
  mutate(resultat = Résultat,
         entite = entit_ctrl,
         date = as.Date(date_ctrl),
         type = 'controle',
         annee = substr(date,1,4),) %>%
  dplyr::left_join(thematiques, 
                   by = c("theme" = "theme")) %>%
  select(entite, date, annee, type, resultat, thematique, nom_commun, insee_comm) 

controles_n_2 <-
  sf::read_sf(dsn = "data/pts_ctrl_2023_12_31.shp") %>%
  st_transform(crs = 2154) %>%
  filter(entit_ctrl %in% c('SD22','SD29','SD35','SD56', 'DRB', 'PNMI', 'BMI-NO' ) ) %>%
  mutate(resultat = Résultat,
         entite = entit_ctrl,
         date = as.Date(date_ctrl),
         type = 'controle',
         annee = substr(date,1,4),) %>%
  dplyr::left_join(thematiques, 
                   by = c("theme" = "theme")) %>%
  select(entite, date, annee, type, resultat, thematique, nom_commun, insee_comm) 

controles_n_3 <-
  sf::read_sf(dsn = "data/pts_ctrl_OSCEAN_2022.shp") %>%
  st_transform(crs = 2154) %>%
  filter(entit_ctrl %in% c('SD22','SD29','SD35','SD56', 'DRB', 'PNMI', 'BMI-NO' ) ) %>%
  mutate(resultat = conformite,
         entite = entit_ctrl,
         date = as.Date(date_ctrl),
         type = 'controle',
         annee = substr(date,1,4),) %>%
  dplyr::left_join(thematiques, 
                   by = c("theme" = "theme")) %>%
  select(entite, date, annee, type, resultat, thematique, nom_commun, insee_comm) 

controles_n_4 <-
  sf::read_sf(dsn = "data/points_ctrl_OSCEAN_année_2021.shp") %>%
  st_transform(crs = 2154) %>%
  filter(entit_ctrl %in% c('SD22','SD29','SD35','SD56', 'DRB', 'PNMI', 'BMI-NO' ) ) %>%
  mutate(resultat = conformite,
         entite = entit_ctrl,
         date = as.Date(date_ctrl),
         type = 'controle',
         annee = substr(date,1,4),) %>%
  dplyr::left_join(thematiques, 
                   by = c("theme" = "theme")) %>%
  select(entite, date, annee, type, resultat, thematique, nom_commun, insee_comm) 

controles_bzh_5ans <- 
  dplyr::bind_rows(controles_n, 
                   controles_n_1, 
                   controles_n_2,
                   controles_n_3,
                   controles_n_4)

### validation cartographique ----

sf::write_sf(obj = controles_bzh_5ans, dsn = "data/outputs/controles_bzh_5ans.gpkg")

# Correction/suppression manuelle éventuelle sur Qgis des points situés en dehors du territoire.
# Attention, de nombreux points peuvent être situés en mer ou en limite territoriale.
### sélection des objets disjoints avec tampon 2km de territoires_bzh (zones larges comprenant les parties maritimes des départements)
### filtre des sélection pour entite =  'BMI-NO' -> suppression
### else, suppression des objets dont communes hors BZH et maintient des objets mal géoréférencés (pour le décompte notamment)

controles_bzh_5ans <-
  sf::read_sf(dsn = "data/outputs/controles_bzh_5ans.gpkg") %>%
  select(-nom_commun, -insee_comm)

## infractions ----

### Formatage des données ----

infractions_bzh_5ans <-
  sf::read_sf(dsn = "data/localisation_infrac_FAITS_20260312.shp") %>%
  st_transform(crs = 2154) %>%
  mutate(type = 'infraction',
         date = as.Date(date_saisi),
         resultat = 'infraction',
         annee = substr(date,1,4)) %>%
  dplyr::left_join(thematiques, 
                   by = c("theme" = "theme")) %>%
  filter(annee %in% c('2021' ,'2022' ,'2023' ,'2024' ,'2025') & entite %in% c('SD22','SD29','SD35','SD56', 'DRB', 'PNMI', 'BMI-NO' )) %>%
  select(entite, date, annee, type, resultat, suite_ctrl,thematique, commune_fa) 

### validation cartographique des infractions ----

sf::write_sf(obj = infractions_bzh_5ans, dsn = "data/outputs/infractions_bzh_5ans.gpkg")

# Correction/suppression manuelle éventuelle sur Qgis des points situés en dehors du territoire.
# Attention, de nombreux points peuvent être situés en mer ou en limite territoriale.
### sélection des objets disjoints avec tampon 2km de territoires_bzh (zones larges comprenant les parties maritimes des départements)
### filtre des sélection pour entite =  'BMI-NO' -> suppression
### else, suppression des objets dont communes hors BZH et maintient des objets mal géoréférencés (pour le décompte notamment)

infractions_bzh_5ans <-
  sf::read_sf(dsn = "data/outputs/infractions_bzh_5ans.gpkg") %>%
  select(-commune_fa)

## infractions non géoréférencées ----

infractions_ng_bzh_5ans <- data.table::fread(file = "data/infrac_FAITS_non_localises_20260312.csv",
                                   encoding = "UTF-8") %>%
    mutate(type = 'infraction_ng', 
         date = as.Date(date_saisine),
         resultat = "infraction", 
         annee = substr(date,1,4)) %>%
  dplyr::left_join(thematiques, by = c("theme" = "theme")) %>%
  mutate(thematique = case_when(
    !is.na(thematique) ~ thematique,
    is.na(thematique) & domaine == 'Mouillages' ~ 'Police en environnement marin',
    is.na(thematique) & plan_controle == 'Eau et Nature' ~ 'Préservation des milieux aquatiques')) %>%
  select(entite, date, annee, type, resultat, thematique) %>%
  filter(annee %in% c('2021' ,'2022' ,'2023' ,'2024' ,'2025')  & entite %in% c('SD22','SD29','SD35','SD56', 'DRB', 'PNMI', 'BMI-NO' ))

## infractions totales ----

infractions_totales_5ans <-
  rbind(infractions_bzh_5ans %>%
          select(entite, type)%>%
          st_drop_geometry() %>%
          as.data.frame(),
        infractions_ng_bzh_5ans %>%
          select(entite, type)%>%
          st_drop_geometry() %>%
          as.data.frame())

# Fusion des données pour obtenir la table intervention ----

## fusion des données et suppression des doublons controle_nc/infractions ----

interventions_5ans <- dplyr::bind_rows(controles_bzh_5ans, infractions_bzh_5ans, infractions_ng_bzh_5ans) %>%
  filter(is.na(suite_ctrl) | suite_ctrl != 'Oui')

interventions_5ans <- interventions_5ans %>%
  mutate(annee = as.numeric(annee),
         resultat_vf = case_when(
    type == 'infraction' ~ 'Infraction',
    type == 'infraction_ng' ~ 'Infraction non-géoréférencée',
    type == 'controle'  & resultat %in% c('Conforme', 'CONFORME') ~ 'Contrôle conforme',
    type == 'controle'  & resultat %in% c('Manquement', 'Infraction','Manquement et infraction', 'NON_CONFORME') ~ 'Contrôle non conforme',
    type == 'controle'  & (is.na(resultat) | resultat == 'EN_ATTENTE') ~ 'Contrôle non-renseigné'))

table_interventions_5ans <- interventions_5ans

sf::write_sf(obj = table_interventions_5ans, dsn = "data/outputs/interventions_bzh_5ans.gpkg")

table_interventions_5ans <-
  sf::read_sf(dsn = "data/outputs/interventions_bzh_5ans.gpkg") 

# Tables de synthese des interventions ----

## synthèse des thématiques d'interventions selon le SD ----

synthese_intervention_dr_5ans <- table_interventions_5ans %>% 
  st_drop_geometry() %>%
  as.data.frame() %>% 
  tally(n= 'Intervention') %>%
  mutate(entite = 'Total DR')

synthese_intervention_service_5ans <- table_interventions_5ans %>% 
  st_drop_geometry() %>%
  as.data.frame() %>%
  group_by(entite) %>% 
  tally(n= 'Intervention')

total_interventions_entites_5ans <- 
  dplyr::bind_rows(synthese_intervention_dr_5ans, synthese_intervention_service_5ans)

thematiques_interventions_dr_5ans <- table_interventions_5ans %>% 
  st_drop_geometry() %>%
  as.data.frame() %>%
  group_by(thematique) %>% 
  tally(n= 'Intervention') %>% 
  tidyr::pivot_wider(values_from = 'Intervention', names_from = "thematique",   values_fill = 0) %>%
  mutate(entite = "Total DR")

thematiques_interventions_services_5ans <- table_interventions_5ans %>% 
  st_drop_geometry() %>%
  as.data.frame() %>%
  group_by(thematique, entite) %>% 
  tally(n= 'Intervention') %>% 
  tidyr::pivot_wider(values_from = 'Intervention', names_from = "thematique",   values_fill = 0)

thematiques_interventions_entites_5ans <- 
  dplyr::bind_rows(thematiques_interventions_services_5ans, thematiques_interventions_dr_5ans)

synth_thematique_entite_5ans <- table_interventions_5ans %>%
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
  dplyr::left_join(total_interventions_entites_5ans, by = c("entite" = "entite")) %>%
  dplyr::left_join(thematiques_interventions_entites_5ans, by = c("entite" = "entite")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rename(Service = entite)

## Synthèse des services d'intervention selon la thématique ----

entite_thematique_dr_5ans <- table_interventions_5ans %>%
  group_by(thematique) %>%
  summarise("Total DR" = n()) %>%
  st_drop_geometry() %>%
  as.data.frame()

entite_thematique_22_5ans <- table_interventions_5ans %>%
  filter(entite == 'SD22') %>%
  group_by(thematique) %>%
  summarise(SD22 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() 

entite_thematique_29_5ans <- table_interventions_5ans %>%
  filter(entite == 'SD29') %>%
  group_by(thematique) %>%
  summarise(SD29 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() 

entite_thematique_35_5ans <- table_interventions_5ans %>%
  filter(entite == 'SD35') %>%
  group_by(thematique) %>%
  summarise(SD35 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() 

entite_thematique_56_5ans <- table_interventions_5ans %>%
  filter(entite == 'SD56') %>%
  group_by(thematique) %>%
  summarise(SD56 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() 

entite_thematique_pnmi_5ans <- table_interventions_5ans %>%
  filter(entite == 'PNMI') %>%
  group_by(thematique) %>%
  summarise(PNMI = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() 

entite_thematique_bmi_5ans <- table_interventions_5ans %>%
  filter(entite == 'BMI-NO') %>%
  group_by(thematique) %>%
  summarise("BMI-NO" = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() 

synth_entite_thematique_5ans <- table_interventions_5ans %>%
  group_by(thematique) %>%
  summarise() %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::left_join(entite_thematique_22_5ans, by = c("thematique" = "thematique")) %>%
  dplyr::left_join(entite_thematique_29_5ans, by = c("thematique" = "thematique")) %>%
  dplyr::left_join(entite_thematique_35_5ans, by = c("thematique" = "thematique")) %>%
  dplyr::left_join(entite_thematique_56_5ans, by = c("thematique" = "thematique")) %>%
  dplyr::left_join(entite_thematique_pnmi_5ans, by = c("thematique" = "thematique")) %>%
  dplyr::left_join(entite_thematique_bmi_5ans, by = c("thematique" = "thematique")) %>%
  dplyr::left_join(entite_thematique_dr_5ans, by = c("thematique" = "thematique")) %>%
  mutate_all(~replace(., is.na(.), 0)) 

## synthèse des natures et types d'intervention selon le SD ----

conformite_22_5ans <- table_interventions_5ans %>%
  filter(entite == 'SD22') %>%
  group_by(resultat_vf) %>%
  summarise(nb_22 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_22) %>%
  mutate(entite = "SD22")

conformite_29_5ans <- table_interventions_5ans %>%
  filter(entite == 'SD29') %>%
  group_by(resultat_vf) %>%
  summarise(nb_29 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_29) %>%
  mutate(entite = "SD29")

conformite_35_5ans <- table_interventions_5ans %>%
  filter(entite == 'SD35') %>%
  group_by(resultat_vf) %>%
  summarise(nb_35 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_35) %>%
  mutate(entite = "SD35")

conformite_56_5ans <- table_interventions_5ans %>%
  filter(entite == 'SD56') %>%
  group_by(resultat_vf) %>%
  summarise(nb_56 = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_56) %>%
  mutate(entite = "SD56")

conformite_pnmi_5ans <- table_interventions_5ans %>%
  filter(entite == 'PNMI') %>%
  group_by(resultat_vf) %>%
  summarise(nb_pnmi = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_pnmi) %>%
  mutate(entite = "PNMI")

conformite_bmi_5ans <- table_interventions_5ans %>%
  filter(entite == 'BMI-NO') %>%
  group_by(resultat_vf) %>%
  summarise(nb_bmi = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_bmi) %>%
  mutate(entite = "BMI-NO")

conformite_dr_5ans <- table_interventions_5ans %>%
  group_by(resultat_vf) %>%
  summarise(nb_dr = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  pivot_wider(names_from = resultat_vf,
              values_from = nb_dr) %>%
  mutate(entite = "Total DR")

controle_22_5ans <- table_interventions_5ans %>%
  filter(type == 'controle' & entite == 'SD22') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "SD22")

controle_29_5ans <- table_interventions_5ans %>%
  filter(type == 'controle' & entite == 'SD29') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "SD29")

controle_35_5ans <- table_interventions_5ans %>%
  filter(type == 'controle' & entite == 'SD35') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "SD35")

controle_56_5ans <- table_interventions_5ans %>%
  filter(type == 'controle' & entite == 'SD56') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "SD56")

controle_pnmi_5ans <- table_interventions_5ans %>%
  filter(type == 'controle' & entite == 'PNMI') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "PNMI")

controle_bmi_5ans <- table_interventions_5ans %>%
  filter(type == 'controle' & entite == 'BMI-NO') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "BMI-NO")

controle_dr_5ans <- table_interventions_5ans %>%
  filter(type == 'controle') %>%
  summarise(Contrôle = n()) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(entite = "Total DR")

controles_entites_5ans <-
  dplyr::bind_rows(controle_22_5ans, 
                   controle_29_5ans,
                   controle_35_5ans,
                   controle_56_5ans,
                   controle_pnmi_5ans,
                   controle_bmi_5ans,
                   controle_dr_5ans)

conformites_interventions_entites_5ans <-
  dplyr::bind_rows(conformite_22_5ans, 
                   conformite_29_5ans, 
                   conformite_35_5ans, 
                   conformite_56_5ans,
                   conformite_pnmi_5ans,
                   conformite_bmi_5ans,
                   conformite_dr_5ans)

synth_conformite_entite_5ans <- table_interventions_5ans %>%
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
  dplyr::left_join(total_interventions_entites_5ans, by = c("entite" = "entite")) %>%
  dplyr::left_join(controles_entites_5ans, by = c("entite" = "entite")) %>%
  dplyr::left_join(conformites_interventions_entites_5ans, by = c("entite" = "entite")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rename(Service = entite)

### Table de synthèse ----

synth_infractions_ng_service <- infractions_ng_bzh_5ans %>% 
  st_drop_geometry() %>%
  as.data.frame() %>%
  group_by(entite, annee) %>%
  tally() %>%
  tidyr::pivot_wider(values_from = n, names_from = "entite",   values_fill = 0)

synthese_date_service_5ans <- table_interventions_5ans %>% 
  st_drop_geometry() %>%
  as.data.frame() %>%
  group_by(entite, annee) %>% 
  tally() %>% 
  tidyr::pivot_wider(values_from = n, names_from = "entite",   values_fill = 0)

synthese_date_thematique_5ans <- table_interventions_5ans %>% 
  st_drop_geometry() %>%
  as.data.frame() %>% 
  group_by(thematique, annee) %>% 
  tally() %>% 
  tidyr::pivot_wider(values_from = n, names_from = "thematique",   values_fill = 0)

synthese_service_5ans <- table_interventions_5ans %>% 
  st_drop_geometry() %>%
  as.data.frame() %>% 
  group_by(entite, type, resultat_vf) %>% 
  tally() %>% 
  tidyr::pivot_wider(values_from = n, names_from = "type",   values_fill = 0)


# histogrammes ----

histo_infractions_ng_service_5ans <-
  plot_ly(synth_infractions_ng_service, 
          type = 'bar', 
          x = ~annee, 
          y = ~SD56, color = I("darkred"), name = "SD56") %>% 
  add_bars(y = ~SD35, color = I("red"), name = "SD35") %>% 
  add_bars(y = ~SD29, color = I("#fa99a3"), name = "SD29") %>% 
  add_bars(y = ~SD22, color = I("#ffdadd"), name = "SD22") %>% 
  add_bars(y = ~PNMI, color = I("#eb9eff"), name = "PNMI") %>% 
  add_bars(y = ~`BMI-NO`, color = I("#8f0fb0"), name = "BMI-NO")  %>% 
  layout(yaxis = list(title = "Nombre d'interventions"),barmode = 'stack')  %>%
  layout(title = "Nombre d'interventions non-géoréférencées selon l'année et le service",
         xaxis = list(title = "Année"))

histo_infractions_ng_service_5ans

histo_conformite_service_interventions_5ans <-
  ggplot(data = table_interventions_5ans %>%
           filter(entite != 'DRB'), 
         aes(x = entite, fill = resultat_vf)) +
  geom_bar(position = "stack") + 
  scale_fill_manual(values = c("lightgreen","lightgrey", "red","#ae001f", "#700014"))+
  labs(x = "Service",
       y = "Nombre d'interventions",
       title = "Nature et conformité des interventions réalisées selon le service", 
       subtitle = "1er Janvier 2021 - 31 Décembre 2025", 
       fill = "Nature et conformité")+
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1))

histo_conformite_service_interventions_5ans

histo_conformite_thematique_interventions_5ans <-
  ggplot(data = table_interventions_5ans %>%
           filter(entite != 'DRB'), 
         aes(x = thematique, fill = resultat_vf)) +
  geom_bar(position = "stack") + 
  scale_fill_manual(values = c("lightgreen","lightgrey", "red","#ae001f", "#700014"))+
  labs(x = "Thématiques",
       y = "Nombre d'interventions",
       title = "Nature et conformité des interventions réalisées selon la thématique", 
       subtitle = "1er Janvier 2021 - 31 Décembre 2025", 
       fill = "Nature et conformité")+
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1))

histo_conformite_thematique_interventions_5ans

histo_dynamique_date_nature_5ans <-
  ggplot(data = table_interventions_5ans, 
         aes(x = annee, fill = resultat_vf)) +
  geom_bar(position = "stack") +
  labs(x = "Année",
       y = "Nombre d'interventions",
       title = "Nature et conformité des interventions réalisées selon l'année", 
       subtitle = "1er Janvier 2021 - 31 Décembre 2025", 
       fill = "Nature et conformité") + 
  scale_fill_manual(values = c("lightgreen","lightgrey", "red","#ae001f", "#700014"))+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

histo_dynamique_date_nature_5ans

histo_thematique_service_interventions_5ans <-
  plot_ly(synth_thematique_entite_5ans %>%
            filter(Service != 'Total DR') %>%
            filter(Service != 'DRB'),
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
  layout(title = "Thématique des interventions réalisées selon le service entre 2021 et 2025",
         xaxis = list(title = "service"))

histo_thematique_service_interventions_5ans

histo_service_thematique_interventions_5ans <-
  plot_ly(synth_entite_thematique_5ans,
          type = 'bar', 
          x = ~thematique, 
          y = ~`SD22`, color = I("lightblue"), name = "SD22") %>%
  add_bars(y = ~`SD29`, color = I("#18d0f0"), name = "SD29") %>% 
  add_bars(y = ~`SD35`, color = I("blue"), name = "SD35") %>% 
  add_bars(y = ~`SD56`, color = I("darkblue"), name = "SD56") %>% 
  add_bars(y = ~`PNMI`, color = I("#5dff2c"), name = "PNMI") %>% 
  add_bars(y = ~`BMI-NO`, color = I("#0da404"), name = "BMI-NO") %>% 
  layout(yaxis = list(title = "Nombre d'interventions"),barmode = 'stack')  %>%
  layout(title = "Service des interventions réalisées selon la thématique entre 2021 et 2025",
         xaxis = list(title = "service"))

histo_service_thematique_interventions_5ans

histo_dynamique_date_service_5ans <-
  plot_ly(synthese_date_service_5ans, 
          type = 'bar', 
          x = ~annee, 
          y = ~SD56, color = I("darkblue"), name = "SD56") %>% 
  add_bars(y = ~SD35, color = I("blue"), name = "SD35") %>% 
  add_bars(y = ~SD29, color = I("#18d0f0"), name = "SD29") %>% 
  add_bars(y = ~SD22, color = I("lightblue"), name = "SD22") %>% 
  add_bars(y = ~PNMI, color = I("#5dff2c"), name = "PNMI") %>% 
  add_bars(y = ~`BMI-NO`, color = I("#0da404"), name = "BMI-NO")  %>% 
  layout(yaxis = list(title = "Nombre d'interventions"),barmode = 'stack')  %>%
  layout(title = "Nombre d'interventions réalisées selon l'année et le service",
         xaxis = list(title = "Année"))

histo_dynamique_date_service_5ans

histo_dynamique_date_thematique_5ans <-
  plot_ly(synthese_date_thematique_5ans, 
          type = 'bar', 
          x = ~annee, 
          y = ~`Hors thème`, color = I("grey"), name = "Hors thème") %>%
  add_bars(y = ~`Police en environnement marin`, color = I("#ec6270"), name = "Police en environnement marin") %>% 
  add_bars(y = ~`Pêche maritime`, color = I("#ef7bf1"), name = "Pêche maritime") %>% 
  add_bars(y = ~`Sécurité et police de la chasse`, color = I("#ae7e3b"), name = "Sécurité et police de la chasse") %>% 
  add_bars(y = ~`Police sanitaire`, color = I("#ffc96b"), name = "Police sanitaire") %>% 
  add_bars(y = ~`Espèces`, color = I("#5dff2c"), name = "Espèces") %>% 
  add_bars(y = ~`Espaces protégés`, color = I("#0da404"), name = "Espaces protégés") %>% 
  add_bars(y = ~`Pêche en eau douce`, color = I("#01ffff"), name = "Pêche en eau douce") %>% 
  add_bars(y = ~`Préservation des milieux aquatiques`, color = I("#7bbfff"), name = "Préservation des milieux aquatiques") %>% 
  add_bars(y = ~`Gestion qualitative de l'eau`, color = I("#0e01ff"), name = "Gestion qualitative de l'eau") %>% 
  add_bars(y = ~`Gestion quantitative de l'eau`, color = I("#060082"), name = "Gestion quantitative de l'eau") %>% 
  layout(yaxis = list(title = "Nombre d'interventions"),barmode = 'stack')  %>%
  layout(title = "Nombre d'interventions réalisées selon la thématique et l'année",
         xaxis = list(title = "Année"))

histo_dynamique_date_thematique_5ans

histo_thematique_intervention_date_5ans <-
  ggplot(data = table_interventions_5ans, 
         aes(x = lubridate::ymd(date), fill = thematique)) +
  geom_histogram(position = "dodge", bins = 12) +
  labs(x = "Date de l'intervention",
       y = "Nombre d'interventions",
       title = "Dynamique d'interventions selon la thématique", 
       subtitle = "1er Janvier 2021 - 31 Décembre 2025", 
       fill = "Thématique") + 
  scale_x_date(
    date_labels = "%b\n%Y",
    date_breaks = "year") +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

histo_thematique_intervention_date_5ans

histo_service_intervention_date_5ans <-
  ggplot(data = table_interventions_5ans, 
         aes(x = lubridate::ymd(date), fill = entite)) +
  geom_histogram(position = "dodge", bins = 12) +
  labs(x = "Date de l'intervention",
       y = "Nombre d'interventions",
       title = "Dynamique d'interventions selon le service", 
       subtitle = "1er Janvier 2025 - 31 Décembre 2025", 
       fill = "Service") + 
  scale_x_date(
    date_labels = "%b\n%Y",
    date_breaks = "6 month")+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

histo_service_intervention_date_5ans

# Sauvegarde des données ----

save(infractions_bzh_5ans,
     controles_bzh_5ans,
     infractions_totales_5ans,
     table_interventions_5ans,
     synth_infractions_ng_service,
     synth_thematique_entite_5ans,
     synth_entite_thematique_5ans,
     synth_conformite_entite_5ans,
     histo_infractions_ng_service_5ans,
     histo_conformite_service_interventions_5ans,
     histo_conformite_thematique_interventions_5ans,
     histo_dynamique_date_nature_5ans,
     histo_dynamique_date_service_5ans,
     histo_dynamique_date_thematique_5ans,
     histo_thematique_service_interventions_5ans,
     histo_service_thematique_interventions_5ans,
     histo_thematique_intervention_date_5ans,
     histo_service_intervention_date_5ans,
     file = "data/outputs/oscean_5ans.RData")

# chargement des résultats

load(file = "data/outputs/oscean_5ans.RData")

