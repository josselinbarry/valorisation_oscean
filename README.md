# Inventaire Bocage : OpenObs et OISON

Ce projet vise, à partir d'extractions issues de la base de donnée OSCEAN, à produire une donnée compilée et des rapports d'analyse annuels par territoire sur les interventions de police.

Il peut être réutilisé par simple adaptation régionale.

# Import des données

Les données de contrôles et d'infractions sont à usage interne et sont téléchargées depuis le serveur dédié.

# Formatage des données et notion d'intervention

Les données sont formatées puis fusionnées pour aboutir à la notion d'intervention (contrôles + infractions). Toutefois, **afin d'éviter de doublonner des interventions, les infractions issues d'un contrôle administratif ont été supprimées** car déjà comptabilisées au titre d'un contrôle non-conforme.

La couche des interventions est utilisée à des fins de représeantation cartographique directement sur Qgis.

# Rapport d'analyse

Différents histogrammes et tableurs de synthèse sont proposés à l'échelle régionale.
