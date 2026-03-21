# Liste des packages nécessaires
packages <- c(
  "shiny", "shinydashboard", "shinydashboardPlus", "leaflet", 
  "plotly", "DT", "ggiraph", "highcharter", "shinyWidgets", "shinycssloaders"
)

# Fonction d'auto-installation et chargement
load_packages <- function(pkgs) {
  new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(new_pkgs)) {
    message("Installation des packages manquants : ", paste(new_pkgs, collapse = ", "))
    install.packages(new_pkgs, dependencies = TRUE)
  }
  for (pkg in pkgs) {
    library(pkg, character.only = TRUE)
  }
}

# Exécution de la fonction
load_packages(packages)

#rsconnect::setAccountInfo(name='hafiz-sanou', token='1B525C5D044502BE7A9ECF58C3160405', secret='DCsGaGcbCk0DelSSpR8bKwqY2uFOqTAgm4yYZu9P')

# Interface Utilisateur
ui <- dashboardPage(
  
  # --- HEADER ---
  header = dashboardHeader(
    title = span(tagList(icon("chart-line"), "Observatoire Afrique Ouest")),
    titleWidth = 350
  ),
  
  # --- SIDEBAR (Menu de Gauche) ---
  sidebar = dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "tabs",
      menuItem("Présentation du Projet", tabName = "presentation", icon = icon("info-circle")),
      menuItem("Vue d'Ensemble & Carte", tabName = "overview", icon = icon("globe-africa")),
      menuItem("Démographie", tabName = "demo", icon = icon("users")),
      menuItem("Économie & Marchés", tabName = "eco", icon = icon("chart-bar")),
      menuItem("Développement Social", tabName = "social", icon = icon("graduation-cap")),
      menuItem("Santé & Pauvreté", tabName = "health", icon = icon("heartbeat")),
      menuItem("Indice Composite OSEAO", tabName = "composite", icon = icon("star")),
      menuItem("Prédictions & ML", tabName = "predict", icon = icon("brain")),
      menuItem("Exploration des Données", tabName = "data", icon = icon("table")),
      
      hr(),
      # Filtre Global - Pays (appliqué à presque tout le dashboard)
      pickerInput(
        inputId = "selected_countries",
        label = "Sélectionner les pays :",
        choices = NULL, # Sera rempli par le serveur
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE),
        selected = NULL
      ),
      # Filtre Global - Année
      sliderInput("selected_year", "Période d'analyse :", 
                  min = 2014, max = 2024, value = 2024, step = 1, sep = "")
    )
  ),
  
  # --- BODY (Contenu des Pages) ---
  body = dashboardBody(
    # Personnalisation CSS : Blanc et Bleu
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .main-header .logo { background-color: #004a99 !important; font-weight: bold; }
        .main-header .navbar { background-color: #004a99 !important; }
        .box-primary { border-top-color: #004a99 !important; }
        .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #004a99 !important; }
        .small-box { border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); }
        .box { border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.05); }
      "))
    ),
    # PAGE 1 : PRESENTATION DU PROJET
    tabItems(
      tabItem(tabName = "presentation",
              fluidRow(
                # --- Bloc Description du Projet ---
                box(
                  title = "À propos de l'Observatoire Afrique Ouest",
                  status = "primary", solidHeader = TRUE, width = 12,
                  column(8,
                         h3("Objectif du Projet"),
                         p("Ce dashboard a été conçu pour offrir une analyse multidimensionnelle du développement en Afrique de l'Ouest. 
                     Il agrège des données complexes (Démographie, Économie, Santé) pour transformer l'information brute en 
                     insights stratégiques grâce à l'Analyse en Composantes Principales (ACP) et au Machine Learning."),
                         br(),
                         h4("Technologies Utilisées :"),
                         tags$ul(
                           tags$li(strong("R Shiny & Shinydashboard :"), " Architecture de l'application"),
                           tags$li(strong("Analyse Statistique :"), " ACP pour l'Indice Composite"),
                           tags$li(strong("Machine Learning :"), " Régression linéaire pour les prévisions et Clustering Hiérarchique Ward.D2"),
                           tags$li(strong("Visualisation :"), " Highcharts, Plotly, Leaflet et Ggirafe")
                         )
                  ),
                  column(4,
                         div(style = "text-align: center;",
                             icon("chart-pie", style = "font-size: 150px; color: #004a99; opacity: 0.1;")
                         )
                  )
                )
              ),
              
              fluidRow(
                h2("L'Équipe de Développement", style = "margin-left: 20px; margin-bottom: 25px; font-weight: bold; color: #004a99;"),
                
                # --- Membre 1 : SANOU Bakaye Boubacar Hafiz ---
                userBox(
                  title = userDescription(
                    title = "SANOU Bakaye B. Hafiz",
                    subtitle = "Lead Developer & Data Scientist",
                    type = 2,
                    image = "hafiz.jpg" # Remplace par ta photo (www/hafiz.jpg)
                  ),
                  status = "primary",
                  footer = "M1 MAS - Science des Données et IA (Rennes 2)",
                  "Responsable de l'architecture logicielle R Shiny et du déploiement web. A piloté l'intégration du moteur de prédiction ML et l'optimisation de l'interface utilisateur (UI/UX)."
                ),
                
                # --- Membre 2 : SECK Ndeye Fama ---
                userBox(
                  title = userDescription(
                    title = "SECK Ndeye Fama",
                    subtitle = "Data Scientist & Statistical Analyst",
                    type = 2,
                    image = "ndeye.jpeg" # Remplace par sa photo (www/ndeye.jpg)
                  ),
                  status = "info",
                  footer = "M1 MAS - Science des Données et IA (Rennes 2)",
                  "Compétences solides en modélisation statistique. Responsable de la conception de l'Indice Composite OSEAO (via ACP) et de la validation mathématique des algorithmes de clustering."
                ),
                
                # --- Membre 3 : BOUH Fall ---
                userBox(
                  title = userDescription(
                    title = "BOUH Fall",
                    subtitle = "Statisticien et Data Analyst",
                    type = 2,
                    image = "fall.jpeg" # Remplace par sa photo (www/bouh.jpg)
                  ),
                  status = "success",
                  footer = "M1 MAS - Science des Données et IA (Rennes 2)",
                  "Spécialiste de l'ingénierie des données. En charge de la collecte, du nettoyage des indicateurs socio-économiques et du développement de la cartographie interactive Leaflet."
                )
              )
      ),
      
      # PAGE 2 : VUE D'ENSEMBLE ET CARTE
      tabItem(tabName = "overview",
              fluidRow(
                # Conteneur des KPIs
                box(
                  width = 12, status = "primary", solidHeader = TRUE, 
                  title = "Indicateurs Clés de Performance (Régionaux)",
                  column(3, valueBoxOutput("kpi_pop", width = NULL)),
                  column(3, valueBoxOutput("kpi_gdp", width = NULL)),
                  column(3, valueBoxOutput("kpi_life", width = NULL)),
                  column(3, valueBoxOutput("kpi_trend", width = NULL))
                )
              ),
              
              fluidRow(
                fluidRow(
                  box(
                    title = "Cartographie Interactive de l'Afrique de l'Ouest",
                    status = "primary", solidHeader = TRUE, width = 7,
                    leafletOutput("map_wa", height = 450)
                  ),
                  uiOutput("country_details_panel")
                )
              ),
              
              fluidRow(
                box(
                  title = "Classement des Pays",
                  status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("rank_var", "Indicateur à classer :", 
                              choices = c("PIB/Hab" = "gdp_per_capita", "Population" = "population", "Accès Électricité" = "electricity_access")),
                  highchartOutput("rank_plot", height = 450)
                ),
                box(
                  title = "Évolution de la Pauvreté & Tendances",
                  status = "primary", solidHeader = TRUE, width = 6,
                  highchartOutput("poverty_bar_plot"),
                  footer = "Moyenne régionale annuelle du taux de pauvreté."
                )
              ),
              
              fluidRow(
                box(
                  title = "Analyse Comparative (Radar)",
                  status = "primary", solidHeader = TRUE, width = 6,
                  highchartOutput("radar_comparison"),
                  footer = "Comparaison du profil sélectionné vs moyenne Afrique de l'Ouest."
                ),
                box(
                  title = "Exploration Temporelle Dynamique",
                  status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("macro_var", "Choisir l'indicateur à visualiser :",
                              choices = c("Population" = "population", 
                                          "PIB (GDP)" = "gdp", 
                                          "Taux de Chômage" = "unemployment", 
                                          "Inflation (%)" = "inflation",
                                          "Accès Électricité" = "electricity_access")),
                  girafeOutput("macro_evolution_plot",height = 400)
                )
              )
      ),
      
      # PAGE 3 : DEMOGRAPHIE
      tabItem(tabName = "demo",
              # SECTION 1 : KPIs DÉMOGRAPHIQUES
              fluidRow(
                valueBoxOutput("box_pop_total", width = 3),
                valueBoxOutput("box_fertility", width = 3),
                valueBoxOutput("box_urban_rate", width = 3),
                valueBoxOutput("box_growth", width = 3)
              ),
              
              # SECTION 2 : ANALYSE DES DYNAMIQUES (Corrélations & Langues)
              fluidRow(
                box(
                  title = "Relation : Fécondité vs Espérance de Vie (Analyse par Langue)",
                  status = "primary", solidHeader = TRUE, width = 8,
                  plotlyOutput("bubble_scatter_demo", height = 500),
                  footer = "La taille des bulles représente la population. Les couleurs groupent les pays par langue officielle."
                ),
                box(
                  title = "Poids Démographique par Groupe Linguistique",
                  status = "primary", solidHeader = TRUE, width = 4,
                  highchartOutput("pie_langue", height = 500)
                )
              ),
              
              # SECTION 3 : ANALYSE TEMPORELLE DÉTAILLÉE
              fluidRow(
                box(
                  title = "Comparaison de la Croissance de la Population",
                  status = "primary", solidHeader = TRUE, width = 6,
                  girafeOutput("pop_growth_lollipop")
                ),
                box(
                  title = "Évolution du Taux d'Urbanisation (%)",
                  status = "primary", solidHeader = TRUE, width = 6,
                  girafeOutput("urban_line_plot"),
                  footer = "Pourcentage de la population totale vivant dans les zones urbaines."
                )
              ),
              # SECTION 4 : ANALYSE AVANCÉE ET RECORDS
              fluidRow(
                # Bloc de gauche : Le graphique de densité interactif
                box(
                  title = "Analyse de Corrélation : Fécondité vs Urbanisation",
                  status = "primary", solidHeader = TRUE, width = 6,
                  girafeOutput("demo_heatmap_interactive"), 
                  br(),
                  div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 5px solid #004a99;",
                      strong("Comment interpréter ce graphique ?"),
                      p("Ce graphique montre la distribution des pays. Les zones d'ombre (contours) représentent la ", strong("densité"), " : plus le bleu est foncé, plus il y a de pays regroupés dans cette zone. 
          Généralement, on observe une diagonale : les pays à forte fécondité sont peu urbanisés, tandis que l'urbanisation s'accompagne d'une baisse de la fécondité.")
                  )
                ),
                
                # Bloc de droite : Les tableaux de records
                box(
                  title = "Tableaux d'Honneur (Top 5 - Année sélectionnée)",
                  status = "primary", solidHeader = TRUE, width = 6,
                  tabsetPanel(
                    tabPanel("📈 Population", 
                             helpText("Pays les plus peuplés (en millions)"),
                             DTOutput("tab_top_pop")),
                    tabPanel("🚀 Croissance", 
                             helpText("Taux de croissance annuel (%)"),
                             DTOutput("tab_top_growth")),
                    tabPanel("🏙️ Urbanisation", 
                             helpText("Part de la population urbaine (%)"),
                             DTOutput("tab_top_urban"))
                  )
                )
              )
      ),
      
      # PAGE 4 : ECONOMIES ET MARCHES
      tabItem(tabName = "eco",
              # SECTION 1 : Richesse et Croissance
              fluidRow(
                valueBoxOutput("box_gdp_total", width = 4),
                valueBoxOutput("box_gdp_growth", width = 4),
                valueBoxOutput("box_unemployment", width = 4),
                valueBoxOutput("box_inflation_avg", width = 3)
              ),
              
              # SECTION 2 : PERFORMANCE ÉCONOMIQUE
              fluidRow(
                box(
                  title = "Trajectoires : Croissance Réelle vs Inflation (Déflateur)",
                  status = "primary", solidHeader = TRUE, width = 8,
                  girafeOutput("eco_dual_line"),
                  footer = "Le déflateur mesure l'augmentation des prix de la production nationale."
                ),
                box(
                  title = "Poids Économique Relatif",
                  status = "primary", solidHeader = TRUE, width = 4,
                  highchartOutput("pib_treemap"),
                  footer = "Taille des carrés proportionnelle au PIB total."
                ),
              ),
              
              # SECTION 3 : ANALYSE PIB, CHOMAGE ET INFLATION
              fluidRow(
                box(
                  title = "Distribution du PIB par habitant",
                  status = "primary", solidHeader = TRUE, width = 4,
                  highchartOutput("pib_hab_polar"),
                  footer = "Comparaison de la richesse par habitant (Indice de niveau de vie)."
                ),
                box(
                  title = "Corrélation : Inflation vs Chômage (Courbe de Phillips)",
                  status = "primary", solidHeader = TRUE, width = 7,
                  plotlyOutput("phillips_curve"),
                  br(),
                  div(style = "background-color: #eef2f7; padding: 10px; border-radius: 5px;",
                      strong("Note d'analyse :"), 
                      p("Traditionnellement, il existe un arbitrage entre inflation et chômage. Ce graphique permet d'identifier les pays souffrant de 'stagflation' (inflation ET chômage élevés)."))
                )
              ),
              
              fluidRow(
                box(
                  title = "Classement de l'Inflation par Pays",
                  status = "primary", solidHeader = TRUE, width = 6,
                  highchartOutput("inflation_bar_chart"),
                  footer = "Taux d'inflation calculé à partir de l'indice des prix à la consommation."
                ),
                # Nouvelle Analyse 2 : Quadrant de Performance
                box(
                  title = "Analyse Quadrant : Croissance vs Chômage",
                  status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("growth_unemployment_quadrant"),
                  footer = "Quadrant supérieur-droit : pays en forte croissance avec faible chômage."
                )
              ),
              
              # SECTION 4 : TABLEAU DE SYNTHÈSE DES MARCHÉS
              fluidRow(
                box(
                  title = "Tableau de Bord Économique Détaillé",
                  status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("eco_table_summary")
                )
              )
      ),
      
      # PAGE 5 : SOCIAL
      tabItem(tabName = "social",
              # SECTION 1 : CONNECTIVITÉ ET ACCÈS
              fluidRow(
                valueBoxOutput("box_elec_access", width = 3),
                valueBoxOutput("box_internet", width = 3),
                valueBoxOutput("box_mobile", width = 3),
                valueBoxOutput("box_literacy", width = 3)
              ),
              
              # SECTION 2 : ÉDUCATION ET GENRE
              fluidRow(
                box(
                  title = "Écart de Genre : Scolarisation Secondaire (F vs H)",
                  status = "primary", solidHeader = TRUE, width =6,
                  girafeOutput("edu_gender_gap"),
                  footer = "Le segment relie le taux des filles (bleu clair) à celui des garçons (bleu foncé)."
                ),
                box(
                  title = "Charge de Dépendance : Focus Régional",
                  status = "primary", solidHeader = TRUE, width = 6,
                  highchartOutput("dependency_bar_advanced")
                )
              ),
              
              # SECTION 3 : INFRASTRUCTURES ET CONNECTIVITÉ
              fluidRow(
                box(
                  title = "Corrélation : Électricité vs Internet",
                  status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("elec_internet_scatter"),
                  footer = "L'accès à l'énergie est-il le moteur de la connectivité digitale ?"
                ),
                box(
                  title = "Évolution de l'équipement Mobile",
                  status = "primary", solidHeader = TRUE, width = 6,
                  highchartOutput("mobile_evolution_area")
                )
              ),
              
              # SECTION 4 : LEADERBOARD SOCIAL
              fluidRow(
                box(
                  title = "Le Fossé Numérique : Élec vs Internet",
                  status = "primary", solidHeader = TRUE, width = 6,
                  girafeOutput("digital_divide_plot"),
                  footer = "Compare la maturité infrastructurelle vs l'adoption digitale."
                ),
                box(
                  title = "Matrice de Transition Éducative",
                  status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("edu_transition_matrix"),
                  footer = "Axe X : Alphabétisation générale | Axe Y : Scolarisation secondaire des filles."
                ),
      )),
      
      # PAGE 6 : SANTE ET PAUVRETE 
      tabItem(tabName = "health",
              # SECTION 1 : INDICES DE VULNÉRABILITÉ
              fluidRow(
                valueBoxOutput("box_infant_mort", width = 4),
                valueBoxOutput("box_maternal_mort", width = 4),
                valueBoxOutput("box_gini", width = 4)
              ),
              
              # SECTION 2 : SANTÉ MATERNELLE ET INFANTILE
              fluidRow(
                box(
                  title = "Corrélation : Mortalité Infantile vs Maternelle",
                  status = "danger", solidHeader = TRUE, width = 8,
                  plotlyOutput("mortality_scatter"),
                  footer = "Cette corrélation forte indique souvent une fragilité globale du système de santé."
                ),
                box(
                  title = "Répartition des Inégalités (Indice Gini)",
                  status = "danger", solidHeader = TRUE, width = 4,
                  highchartOutput("gini_polar_chart"),
                  footer = "Un score élevé indique une forte concentration des richesses."
                )
              ),
              
              # SECTION 3 : PAUVRETÉ ET RÉPARTITION
              fluidRow(
                box(
                  title = "Le Paradoxe : Taux de Pauvreté vs Indice Gini",
                  status = "primary", solidHeader = TRUE, width = 6,
                  girafeOutput("poverty_gini_bubble"),
                  br(),
                  div(style = "background-color: #fcf8e3; padding: 10px; border-radius: 5px; border-left: 5px solid #8a6d3b;",
                      strong("Analyse du Data Scientist :"), 
                      p("Certains pays peuvent avoir peu de pauvreté mais des inégalités extrêmes. Ce graphique identifie les zones de tension sociale."))
                ),
                box(
                  title = "Top Records : Urgences Sociales",
                  status = "primary", solidHeader = TRUE, width = 6,
                  DTOutput("social_emergency_table")
                )
              )
      ),
      
      # PAGE 7 : INDICE COMPOSITE
      tabItem(tabName = "composite",
              fluidRow(
                # Méthodologie et Score Moyen
                box(
                  title = "Méthodologie de l'Indice Composite OSEAO (Approche ACP)",
                  status = "warning", solidHeader = TRUE, width = 12,
                  column(8, 
                         p("L'Indice OSEAO utilise l'Analyse en Composantes Principales (ACP) pour agréger 7 variables socio-économiques."),
                         p(strong("Pourquoi l'ACP ?"), "Contrairement à une moyenne, l'ACP attribue des poids mathématiques basés sur la variance réelle des données de l'année choisie, évitant les biais subjectifs."),
                         p(tags$i("Variables incluses : PIB (log), Électricité, Espérance de vie, Urbanisation, Alphabétisation, Mortalité Infantile (inv), Internet."))
                  ),
                  column(4, 
                         infoBox("Score Moyen Régional", textOutput("avg_score_text"), icon = icon("calculator"), width = 12, color = "orange")
                  )
                )
              ),
              
              fluidRow(
                # Graphique de contribution (L'audit)
                box(
                  title = "Audit des Poids : Contribution des Variables au Score (%)",
                  status = "primary", solidHeader = TRUE, width = 12,
                  highchartOutput("var_contributions_plot", height = "300px"),
                  footer = "Ce graphique montre quel facteur a le plus influencé le classement pour l'année sélectionnée."
                )
              ),
              
              fluidRow(
                # Classement Général
                box(
                  title = "Classement Performance Globale (Score 0-100)",
                  status = "primary", solidHeader = TRUE, width = 7,
                  highchartOutput("composite_rank_bar"),
                  footer = "Le score est normalisé : 100 représente le leader de la sélection."
                ),
                # Analyse Radar
                box(
                  title = "Profil Détaillé (Radar Chart)",
                  status = "primary", solidHeader = TRUE, width = 5,
                  selectInput("country_radar", "Pays à analyser :", choices = NULL),
                  highchartOutput("radar_profil_pays")
                )
              ),
              
              fluidRow(
                # Heatmap
                box(
                  title = "Matrice de Performance Comparative (Heatmap)",
                  status = "primary", solidHeader = TRUE, width = 12,
                  highchartOutput("heatmap_composite"),
                  footer = "Couleurs foncées = Performance supérieure."
                )
              )
      ),
      
      # PAGE 8 : PREDICTIONS ET ML
      tabItem(tabName = "predict",
              fluidRow(
                box(
                  title = tagList(icon("gear"), "Configuration de la Prédiction"), 
                  status = "warning", solidHeader = TRUE, width = 12,
                  column(6, 
                         selectInput("predict_var", "Indicateur à projeter :", 
                                     choices = list(
                                       "Économie" = c("PIB Total" = "gdp", "PIB/Hab" = "gdp_per_capita", "Croissance PIB" = "gdp_growth", "Inflation" = "inflation", "Chômage" = "unemployment"),
                                       "Social & Santé" = c("Indice OSEAO" = "score_final", "Espérance de Vie" = "life_expectancy", "Taux de Pauvreté" = "poverty_rate", "Mortalité Infantile" = "infant_mortality", "Écart Genre Scolaire" = "edu_gap"),
                                       "Infrastructure" = c("Accès Électricité" = "electricity_access", "Utilisateurs Internet" = "internet_users", "Abonnés Mobile" = "mobile_users"),
                                       "Démographie" = c("Population Totale" = "population", "Taux de Fécondité" = "fertility_rate", "Urbanisation" = "urban_population")
                                     )),
                         helpText("Modèle : Régression linéaire projetant la tendance historique (2014-2024) jusqu'en 2030. 
          Écart Genre : Différence (Garçons - Filles) dans le secondaire ; un score proche de 0 indique la parité.")
                  ),
                  column(6,
                         # Sélecteur d'année pour le graphique de droite
                         uiOutput("ui_rank_year_pred") 
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = tagList(icon("chart-line"), "Trajectoire Prédictive jusqu'en 2030"), 
                  status = "primary", solidHeader = TRUE, width = 8,
                  # CORRECTION ICI : highchartOutput au lieu de girafeOutput
                  withSpinner(highchartOutput("forecast_plot")), 
                  footer = "Les traits pleins sont historiques, les pointillés sont les projections."
                ),
                box(
                  title = tagList(icon("list-ol"), "Classement Projeté"), 
                  status = "primary", solidHeader = TRUE, width = 4,
                  highchartOutput("pred_rank_chart"),
                  footer = "Comparatif basé sur l'année cible choisie."
                )
              ),
              
              fluidRow(
                box(
                  title = tagList(icon("layer-group"), "Segmentation des Trajectoires (IA Clustering)"), 
                  status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("cluster_plot"),
                  footer = tagList(
                    strong("Comment lire ce graphique ? "), br(),
                    "L'algorithme regroupe les pays ayant des profils similaires sans intervention humaine.", br(),
                    "• ", strong("Axe Horizontal :"), " Position actuelle du pays.", br(),
                    "• ", strong("Axe Vertical :"), " Vitesse de progression projetée d'ici 2030.", br(),
                    "• ", em("Les pays de même couleur partagent des trajectoires de développement communes.")
                  )
                )
              )
      ),
      
      # PAGE 9 : EXPLORATION DE DONNEES
      tabItem(tabName = "data",
              box(
                title = "Base de données complète (2014-2024)",
                status = "primary", width = 12,
                DTOutput("table_raw")
              )
      )
    )
  )
)