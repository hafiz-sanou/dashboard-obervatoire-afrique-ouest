# Liste complète de tes packages
packages <- c(
  "shiny", "dplyr", "ggplot2", "leaflet", "DT", "highcharter", 
  "rnaturalearth", "rnaturalearthdata", "sf", "viridisLite", 
  "FactoMineR", "zoo", "tidyr", "shinydashboard", "shinydashboardPlus",
  "shinyWidgets", "shinycssloaders", "ggiraph", "plotly"
)

# Fonction d'auto-installation et de chargement silencieux
load_packages <- function(pkgs) {
  # Identification des packages manquants
  missing_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  
  # Installation si nécessaire
  if (length(missing_pkgs)) {
    message("Configuration initiale en cours (installation des dépendances)...")
    install.packages(missing_pkgs, dependencies = TRUE)
  }
  
  # Chargement de tous les packages
invisible(lapply(pkgs, library, character.only = TRUE))
}


# Lancement de la configuration
#load_packages(packages)

# Fonction d'imputation intelligente
impute_data <- function(df) {
  df %>%
    group_by(country) %>%
    arrange(year) %>%
    mutate(across(
      .cols = where(is.numeric) & !all_of("year"), 
      .fns = function(x) {
        # 1. Si toute la colonne est vide pour un pays, on ne peut pas inventer (on met 0 ou moyenne régionale)
        if (all(is.na(x))) return(rep(0, length(x)))
        
        # 2. Interpolation linéaire pour les trous entre deux années
        x <- na.approx(x, na.rm = FALSE)
        
        # 3. Porter la dernière valeur connue vers le futur (extrapolation)
        x <- na.locf(x, na.rm = FALSE)
        
        # 4. Porter la première valeur connue vers le passé
        x <- na.locf(x, fromLast = TRUE, na.rm = FALSE)
        
        return(x)
      }
    )) %>%
    ungroup()
}

server <- function(input, output, session) {
  
  # Chargement des données
  data <- reactive({
    df <- read.csv("west_africa_dashboard_data.csv", stringsAsFactors = FALSE)
    df <- impute_data(df)
    
    # Calcul de l'inflation par pays
    df <- df %>%
      group_by(country) %>%
      arrange(year) %>%
      mutate(inflation = (custumer_price_index - lag(custumer_price_index)) / lag(custumer_price_index) * 100) %>%
      ungroup()
    
    return(df)
  })
  
  data_enriched <- reactive({
    req(data())
    df <- data() %>%
      mutate(edu_gap = secondary_school_male - secondary_school_female)
    return(df)
  })
  
  ##  ---  COTE SERVER PAGE OVERVIEW  --
  
  # Téléchargement des frontières mondiales
  world_sf <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Créer un objet spatial spécifique à l'Afrique de l'Ouest
  wa_iso <- c("BJ", "BF", "CV", "CI", "GM", "GH", "GN", "GW", "LR", "ML", "MR", "NE", "NG", "SN", "SL", "TG")
  
  wa_map_data <- world_sf %>% 
    filter(iso_a2 %in% wa_iso)
  
  # Mise à jour des filtres de la sidebar
  observe({
    countries <- unique(data()$country)
    updatePickerInput(session, "selected_countries", choices = countries, selected = countries[1:5])
  })
  
  # Données filtrées selon les inputs
  filtered_data <- reactive({
    req(input$selected_countries, input$selected_year)
    data_enriched() %>%
      filter(country %in% input$selected_countries,
             year == input$selected_year)
  })
  
  # Exemple de KPI (Population totale de la sélection)
  output$kpi_pop <- renderValueBox({
    total_pop <- sum(filtered_data()$population, na.rm = TRUE)
    valueBox(
      format(round(total_pop / 1e6, 1), big.mark = " "), "Millions d'habitants",
      icon = icon("users"), color = "blue"
    )
  })
  
  # Référentiel exhaustif pour l'Afrique de l'Ouest
  country_info <- data.frame(
    iso2c = c("BJ", "BF", "CV", "CI", "GM", "GH", "GN", "GW", "LR", "ML", "NE", "NG", "SN", "SL", "TG"),
    capitale = c("Porto-Novo", "Ouagadougou", "Praia", "Yamoussoukro", "Banjul", "Accra", "Conakry", "Bissau", "Monrovia", "Bamako", "Niamey", "Abuja", "Dakar", "Freetown", "Lomé"),
    langue = c("Français", "Français", "Portugais", "Français", "Anglais", "Anglais", "Français", "Portugais", "Anglais", "Français", "Français", "Anglais", "Français", "Anglais", "Français"),
    president = c("Patrice Talon", "Ibrahim Traoré", "José Maria Neves", "Alassane Ouattara", "Adama Barrow", "Nana Akufo-Addo", "Mamadi Doumbouya", "Umaro Sissoco Embaló", "Joseph Boakai", "Assimi Goïta", "Abdourahamane Tiani", "Bola Tinubu", "Bassirou Diomaye Faye", "Julius Maada Bio", "Faure Gnassingbé"),
    ressources = c("Coton, Karité", "Or, Zinc", "Tourisme, Pêche", "Cacao, Café, Pétrole", "Arachides, Tourisme", "Or, Cacao, Pétrole", "Bauxite, Or", "Noix de Cajou", "Minerai de Fer, Caoutchouc", "Or, Coton", "Uranium, Pétrole", "Pétrole, Gaz, Agriculture", "Pêche, Phosphates, Arachides", "Diamants, Rutile", "Phosphates, Commerce")
  )
  
  # 1. Variable réactive pour stocker le pays cliqué
  selected_country_data <- reactiveVal(NULL)
  
  # 2. Capture du clic sur la carte
  observeEvent(input$map_wa_shape_click, {
    click <- input$map_wa_shape_click
    
    # On récupère les infos du pays cliqué via la jointure
    info <- country_info %>% 
      filter(iso2c == click$id) # L'ID du shape doit être l'iso2c
    
    selected_country_data(info)
  })
  
  # Carte Leaflet
  output$map_wa <- renderLeaflet({
    # wa_map_data doit être défini hors du server ou chargé ici
    leaflet(wa_map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(layerId = ~iso_a2, label = ~name, fillColor = "#f0f0f0", weight = 1)
  })
  
  observe({
    req(input$map_wa_shape_click)
    proxy <- leafletProxy("map_wa")
    
    # Réinitialise les couleurs et colorie seulement le pays cliqué
    proxy %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = wa_map_data %>% filter(iso_a2 == input$map_wa_shape_click$id),
        fillColor = "#2171b5",
        color = "#004a99",
        weight = 4,
        fillOpacity = 0.9,
        group = "highlight"
      )
  })
  
  output$country_details_panel <- renderUI({
    data <- selected_country_data()
    
    if (is.null(data)) {
      return(box(width = 4, status = "info", "Cliquez sur un pays pour voir les détails."))
    }
    
    flag_url <- paste0("https://flagcdn.com/w160/", tolower(data$iso2c), ".png")
    # Note : lc() est une fonction pour mettre en minuscule si besoin
    
    box(
      title = span(tagList(icon("info-circle"), paste("Fiche Pays :", data$iso2c))),
      status = "primary", solidHeader = TRUE, width = 5,
      tags$div(
        style = "text-align: center; margin-bottom: 15px;",
        tags$img(src = paste0("https://flagcdn.com/w160/", tolower(data$iso2c), ".png"), 
                 style = "border: 1px solid #ddd; border-radius: 4px; box-shadow: 2px 2px 5px rgba(0,0,0,0.1);")
      ),
      tags$div(
        style = "font-size: 15px;",
        tags$p(tags$b("🏛 Capitale : "), data$capitale),
        tags$p(tags$b("👤 Président : "), data$president),
        tags$p(tags$b("🗣 Langue : "), data$langue),
        tags$hr(style = "margin: 10px 0;"),
        tags$p(tags$b("🚀 Poumon Économique :")),
        tags$p(style = "color: #2c3e50; font-style: italic;", data$ressources)
      )
    )
  })
  
  # Graphique de classement Highcharter
  output$rank_plot <- renderHighchart({
    
    # SECURITÉ : On attend que l'utilisateur ait sélectionné des pays
    req(input$selected_countries)
    
    # Préparation des données pour le classement
    df_rank <- filtered_data() %>%
      select(country, var_to_plot = all_of(input$rank_var)) %>% # On renomme la colonne dynamiquement
      filter(!is.na(var_to_plot)) %>%                           # On enlève les valeurs vides
      arrange(desc(var_to_plot))                                # On trie du plus grand au plus petit
    
    # Création du graphique Highcharts
    hchart(df_rank, "bar", hcaes(x = country, y = var_to_plot), name = input$rank_var) %>%
      hc_colors("#004a99") %>% # Ton bleu thématique
      hc_title(text = paste("Classement par", input$rank_var)) %>%
      hc_subtitle(text = paste("Année", input$selected_year)) %>%
      hc_xAxis(title = list(text = "Pays")) %>%
      hc_yAxis(title = list(text = "Valeur")) %>%
      hc_exporting(enabled = TRUE) %>% # Permet de télécharger le graph (Petit bouton en haut à droite)
      hc_add_theme(hc_theme_smpl())    # Un thème épuré blanc
  })
  
  # KPIs Additionnels
  output$kpi_gdp <- renderValueBox({
    avg_gdp <- mean(filtered_data()$gdp_growth, na.rm = TRUE)
    valueBox(
      paste0(round(avg_gdp, 2), "%"), "Croissance PIB Moyenne",
      icon = icon("chart-line"), color = "blue"
    )
  })
  
  output$kpi_life <- renderValueBox({
    avg_life <- mean(filtered_data()$life_expectancy, na.rm = TRUE)
    valueBox(
      round(avg_life, 1), "Espérance de vie (ans)",
      icon = icon("heart"), color = "teal"
    )
  })
  
  output$demo_evolution <- renderGirafe({
    # On utilise toutes les années pour le graphique d'évolution
    df_evol <- data() %>% filter(country %in% input$selected_countries)
    
    gg <- ggplot(df_evol, aes(x = year, y = population, color = country)) +
      geom_line_interactive(aes(tooltip = country, data_id = country), size = 1) +
      geom_point_interactive(aes(tooltip = paste0(country, " (", year, "): ", round(population/1e6, 2), "M"), data_id = country)) +
      theme_minimal() +
      scale_color_brewer(palette = "Paired") +
      labs(title = "Évolution de la population (2014-2024)", x = "Année", y = "Population")
    
    girafe(ggobj = gg, width_svg = 7, height_svg = 4)
  })
  
  output$poverty_bar_plot <- renderHighchart({
    # Calcul de la moyenne régionale par année
    poverty_avg <- data() %>%
      group_by(year) %>%
      summarise(mean_poverty = mean(poverty_rate, na.rm = TRUE))
    
    hchart(poverty_avg, "column", hcaes(x = year, y = mean_poverty), name = "Taux de Pauvreté Moyen") %>%
      hc_colors("#448aff") %>% # Un bleu plus clair pour varier
      hc_title(text = "Évolution du Taux de Pauvreté de 2014 à 2024") %>%
      hc_tooltip(pointFormat = "<b>Année:</b> {point.x}<br><b>Moyenne Régionale:</b> {point.y:.2f}%") %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$macro_evolution_plot <- renderGirafe({
    req(input$selected_countries, input$macro_var)
    
    # Calcul de la moyenne des pays sélectionnés pour la variable choisie
    macro_data <- data() %>%
      filter(country %in% input$selected_countries) %>%
      group_by(year) %>%
      summarise(
        ValeurMoyenne = mean(get(input$macro_var), na.rm = TRUE)
      )
    
    # Création du ggplot "High Level"
    gg <- ggplot(macro_data, aes(x = year, y = ValeurMoyenne)) +
      # Zone ombrée en bleu transparent
      geom_area_interactive(aes(tooltip = paste0("Année: ", year, "\nValeur: ", round(ValeurMoyenne, 2))),
                            fill = "#004a99", alpha = 0.2) +
      # Ligne principale
      geom_line_interactive(size = 1.2, color = "#004a99") +
      # Points interactifs
      geom_point_interactive(aes(tooltip = paste0("Année: ", year, "\nValeur: ", round(ValeurMoyenne, 2)),
                                 data_id = year), 
                             size = 3, color = "#004a99") +
      theme_minimal() +
      labs(x = "Année", y = input$macro_var) +
      scale_x_continuous(breaks = seq(2014, 2024, 1)) +
      theme(
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold")
      )
    
    # Rendu avec effet de zoom au survol
    girafe(ggobj = gg, width_svg = 7, height_svg = 4,
           options = list(
             opts_hover(css = "fill:#448aff;stroke:white;"),
             opts_toolbar(saveaspng = TRUE)
           ))
  })
  
  # KPI de Tendance (Comparaison t vs t-1)
  output$kpi_trend <- renderValueBox({
    req(input$macro_var)
    
    curr_year <- input$selected_year
    prev_year <- curr_year - 1
    
    # Calcul des moyennes pour l'année T et T-1
    val_curr <- data() %>% filter(year == curr_year) %>% summarise(m = mean(get(input$macro_var), na.rm=T)) %>% pull(m)
    val_prev <- data() %>% filter(year == prev_year) %>% summarise(m = mean(get(input$macro_var), na.rm=T)) %>% pull(m)
    
    # Sécurité si l'année 2013 n'existe pas
    if(length(val_prev) == 0 || is.na(val_prev)) {
      return(valueBox("N/A", "Tendance non disp.", icon = icon("question"), color = "black"))
    }
    
    diff <- val_curr - val_prev
    icon_trend <- if(diff >= 0) "arrow-up" else "arrow-down"
    color_trend <- if(diff >= 0) "green" else "red"
    
    valueBox(
      paste0(ifelse(diff > 0, "+", ""), round(diff, 2)), 
      paste("Evolution vs", prev_year),
      icon = icon(icon_trend), color = color_trend
    )
  })
  
  # Radar Chart (Benchmark multidimensionnel)
  output$radar_comparison <- renderHighchart({
    req(input$selected_countries)
    
    # Fonction de calcul pour normaliser 5 piliers de 0 à 100
    get_radar_data <- function(df_input) {
      df_input %>% summarise(
        Economie = mean(gdp_growth, na.rm = TRUE) * 10, # Scale pour visibilité
        Sante = mean(life_expectancy, na.rm = TRUE),
        Education = mean(literacy_rate, na.rm = TRUE),
        Infrastructure = mean(electricity_access, na.rm = TRUE),
        Digital = mean(internet_users, na.rm = TRUE)
      ) %>% tidyr::pivot_longer(cols = everything())
    }
    
    # Moyenne des pays sélectionnés
    avg_sel <- get_radar_data(filtered_data())
    # Moyenne de toute la région pour la même année
    avg_reg <- get_radar_data(data() %>% filter(year == input$selected_year))
    
    highchart() %>%
      hc_chart(polar = TRUE, type = "area") %>%
      hc_title(text = "Profil Socio-Économique") %>%
      hc_xAxis(categories = avg_sel$name, tickmarkPlacement = 'on', lineWidth = 0) %>%
      hc_yAxis(gridLineInterpolation = 'polygon', lineWidth = 0, min = 0, max = 100) %>%
      hc_series(
        list(name = "Votre Sélection", data = round(avg_sel$value, 1), color = "rgba(0, 74, 153, 0.5)", pointPlacement = 'on'),
        list(name = "Moyenne Afrique Ouest", data = round(avg_reg$value, 1), color = "rgba(200, 200, 200, 0.3)", pointPlacement = 'on')
      ) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  
  
  ##  ---  COTE SERVER PAGE DEMOGRAPHIIE  --
  
  output$box_pop_total <- renderValueBox({
    val <- sum(filtered_data()$population, na.rm = TRUE)
    valueBox(format(round(val/1e6, 1), big.mark=" "), "Population Totale (Millions)", icon = icon("users"), color = "blue")
  })
  
  output$box_fertility <- renderValueBox({
    val <- mean(filtered_data()$fertility_rate, na.rm = TRUE)
    valueBox(round(val, 2), "Taux de Fécondité Moyen", icon = icon("baby"), color = "teal")
  })
  
  output$bubble_scatter_demo <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = fertility_rate, y = life_expectancy, 
                                     size = population, color = official_language, 
                                     text = paste("Pays:", country))) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(3, 15)) +
      theme_minimal() +
      labs(x = "Taux de Fécondité", y = "Espérance de Vie") +
      scale_color_brewer(palette = "Set1")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$pie_langue <- renderHighchart({
    lang_data <- filtered_data() %>%
      group_by(official_language) %>%
      summarise(total_pop = sum(population, na.rm = TRUE))
    
    hchart(lang_data, "pie", hcaes(x = official_language, y = total_pop)) %>%
      hc_title(text = "Répartition par Langue Officielle") %>%
      hc_tooltip(pointFormat = "<b>{point.percentage:.1f}%</b> de la population sélectionnée") %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$pop_growth_lollipop <- renderGirafe({
    gg <- ggplot(filtered_data(), aes(x = reorder(country, population_growth), y = population_growth)) +
      geom_segment_interactive(aes(xend = country, yend = 0), color = "grey") +
      geom_point_interactive(aes(tooltip = paste0(country, ": ", round(population_growth, 2), "%")), 
                             size = 4, color = "#004a99") +
      coord_flip() +
      theme_minimal() +
      labs(x = "", y = "Taux de Croissance Annuel (%)")
    
    girafe(ggobj = gg, width_svg = 6, height_svg = 4)
  })
  
  output$urban_line_plot <- renderGirafe({
    req(input$selected_countries)
    df_urban <- data() %>% filter(country %in% input$selected_countries)
    
    gg <- ggplot(df_urban, aes(x = year, y = urban_population, color = country, fill = country)) +
      geom_line_interactive(aes(tooltip = country), size = 1) +
      geom_area(alpha = 0.1, position = "identity") +
      theme_minimal() +
      labs(x = "Année", y = "Population Urbaine (%)") +
      scale_x_continuous(breaks = seq(2014, 2024, 2))
    
    girafe(ggobj = gg, width_svg = 6, height_svg = 4)
  })
  
  output$demo_heatmap_interactive <- renderGirafe({
    gg <- ggplot(data(), aes(x = fertility_rate, y = urban_population)) +
      # Couche de densité (statique en fond)
      stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.2) +
      # Couche des pays (interactive au survol)
      geom_point_interactive(aes(color = official_language, 
                                 tooltip = paste0(country, "\nFécondité: ", fertility_rate, 
                                                  "\nUrbanisation: ", urban_population, "%"),
                                 data_id = country), 
                             size = 3, alpha = 0.7) +
      scale_fill_gradient(low = "#deebf7", high = "#08306b") +
      theme_minimal() +
      labs(x = "Indice de Fécondité (enfants/femme)", y = "Taux d'Urbanisation (%)", color = "Langue") +
      theme(legend.position = "bottom")
    
    girafe(ggobj = gg, width_svg = 6, height_svg = 5, 
           options = list(opts_hover(css = "stroke:black;stroke-width:2pt;")))
  })
  

  # Fonction pour générer les DT
  render_pro_table <- function(df, var_col, label_val) {
    df_tab <- df %>%
      select(country, !!sym(var_col)) %>%
      arrange(desc(!!sym(var_col))) %>%
      head(5) %>%
      rename(Pays = country, !!label_val := !!sym(var_col))
    
    datatable(df_tab, options = list(dom = 't', ordering = FALSE), rownames = FALSE) %>%
      formatStyle(label_val, fontWeight = 'bold', color = '#004a99')
  }
  
  output$tab_top_pop <- renderDT({ 
    # Pour la population, on divise par 1M pour la lisibilité
    filtered_data() %>% 
      mutate(population = round(population / 1e6, 2)) %>%
      render_pro_table("population", "Pop. (Millions)") 
  })
  
  output$tab_top_growth <- renderDT({ 
    render_pro_table(filtered_data(), "population_growth", "Croissance (%)") 
  })
  
  output$tab_top_urban <- renderDT({ 
    render_pro_table(filtered_data(), "urban_population", "Urbain (%)") 
  })
 
  
  ##  ---  COTE SERVER PAGE ECONOMIE ET MARCHES  --
  
  output$pib_treemap <- renderHighchart({
    hchart(filtered_data(), "treemap", hcaes(x = country, value = gdp, color = gdp)) %>%
      hc_title(text = "Distribution du PIB Régional") %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$phillips_curve <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = unemployment, y = inflation, color = country, size = gdp_per_capita)) +
      geom_point(alpha = 0.7) +
      theme_minimal() +
      labs(x = "Taux de Chômage (%)", y = "Taux d'Inflation (%)")
    
    ggplotly(p)
  })
  
  output$eco_dual_line <- renderGirafe({
    req(input$selected_countries)
    
    df_eco <- data() %>% 
      filter(country %in% input$selected_countries) %>%
      # SÉCURITÉ : On s'assure que tout est bien numérique
      mutate(
        year = as.numeric(as.character(year)),
        gdp_growth = as.numeric(as.character(gdp_growth)),
        gdp_deflator_growth = as.numeric(as.character(gdp_deflator_growth))
      ) %>%
      # On enlève les lignes où les deux valeurs sont NA pour éviter les bugs
      filter(!is.na(gdp_growth) | !is.na(gdp_deflator_growth))
    
    gg <- ggplot(df_eco, aes(x = year)) +
      # AJOUT DE POINTS : Pour voir les données même s'il n'y a qu'une année
      geom_point_interactive(aes(y = gdp_growth, color = "Croissance PIB", 
                                 tooltip = paste(country, "Croissance:", round(gdp_growth, 2), "%")), size = 2) +
      geom_line_interactive(aes(y = gdp_growth, color = "Croissance PIB"), size = 1) +
      
      # Ligne du déflateur
      geom_point_interactive(aes(y = gdp_deflator_growth, color = "Inflation (Déflateur)", 
                                 tooltip = paste("Déflateur:", round(gdp_deflator_growth, 2), "%")), shape = 17, size = 2) +
      geom_line_interactive(aes(y = gdp_deflator_growth, color = "Inflation (Déflateur)"), linetype = "dashed") +
      
      facet_wrap(~country, scales = "free_y") + 
      scale_color_manual(values = c("Croissance PIB" = "#004a99", "Inflation (Déflateur)" = "#ff4444")) +
      theme_minimal() +
      # On force l'affichage des années proprement sur l'axe X
      scale_x_continuous(breaks = seq(2014, 2024, 2)) +
      labs(y = "Taux Annuel (%)", x = "Année", color = "Indicateur") +
      theme(
        legend.position = "bottom",
        panel.spacing = unit(2, "lines"), # Plus d'espace entre les pays
        strip.text = element_text(face = "bold", size = 12) # Noms des pays en gras
      )
    
    girafe(ggobj = gg, width_svg = 10, height_svg = 6)
  })
  
  output$inflation_bar_chart <- renderHighchart({
    df_inf <- filtered_data() %>% arrange(desc(inflation))
    
    hchart(df_inf, "column", hcaes(x = country, y = inflation), name = "Taux d'Inflation (%)") %>%
      hc_colors("#d32f2f") %>% # Rouge pour l'inflation
      hc_title(text = "Comparatif des Taux d'Inflation") %>%
      hc_yAxis(title = list(text = "%")) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$pib_hab_polar <- renderHighchart({
    hchart(filtered_data(), "column", hcaes(x = country, y = gdp_per_capita), polar = TRUE) %>%
      hc_title(text = "Richesse Relative (PIB/Hab)") %>%
      hc_colors("#004a99") %>%
      hc_yAxis(gridLineInterpolation = 'polygon', labels = list(enabled = FALSE)) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$growth_unemployment_quadrant <- renderPlotly({
    avg_growth <- mean(data()$gdp_growth, na.rm = TRUE)
    avg_unemp <- mean(data()$unemployment, na.rm = TRUE)
    
    p <- ggplot(filtered_data(), aes(x = unemployment, y = gdp_growth, text = country)) +
      geom_vline(xintercept = avg_unemp, linetype = "dotted", color = "grey") +
      geom_hline(yintercept = avg_growth, linetype = "dotted", color = "grey") +
      geom_point(aes(size = gdp, color = gdp_growth), alpha = 0.8) +
      scale_color_gradient(low = "red", high = "green") +
      theme_minimal() +
      labs(x = "Taux de Chômage (%)", y = "Croissance PIB (%)")
    
    ggplotly(p)
  })
  
  output$eco_table_summary <- renderDT({
    tab_data <- filtered_data() %>%
      select(country, gdp_per_capita, gdp_growth, inflation, unemployment) %>%
      rename(Pays = country, `PIB/Hab ($)` = gdp_per_capita, `Croissance (%)` = gdp_growth, 
             `Inflation (%)` = inflation, `Chômage (%)` = unemployment)
    
    datatable(tab_data, options = list(pageLength = 5, dom = 'ftp')) %>%
      formatStyle('Croissance (%)', 
                  color = styleInterval(0, c('red', 'green'))) %>%
      formatCurrency('PIB/Hab ($)', currency = "$", digits = 0)
  })
  
  
  ##  ---  COTE SERVER PAGE DEVELOPPEMENT SOCIAL --
  
  output$edu_gender_gap <- renderGirafe({
    df_edu <- filtered_data() %>%
      filter(!is.na(secondary_school_female), !is.na(secondary_school_male))
    
    gg <- ggplot(df_edu) +
      geom_segment_interactive(aes(x = reorder(country, secondary_school_female), 
                                   xend = country, 
                                   y = secondary_school_female, 
                                   yend = secondary_school_male), color = "#e0e0e0", size = 1.5) +
      geom_point_interactive(aes(x = country, y = secondary_school_female, color = "Femmes",
                                 tooltip = paste("Filles:", secondary_school_female, "%")), size = 4) +
      geom_point_interactive(aes(x = country, y = secondary_school_male, color = "Hommes",
                                 tooltip = paste("Garçons:", secondary_school_male, "%")), size = 4) +
      coord_flip() +
      scale_color_manual(values = c("Femmes" = "#ff4081", "Hommes" = "#004a99")) +
      theme_minimal() +
      labs(x = "", y = "Taux de scolarisation (%)", color = "Genre")
    
    girafe(ggobj = gg, width_svg = 7, height_svg = 5)
  })
  
  output$elec_internet_scatter <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = electricity_access, y = internet_users, 
                                     color = country, size = population)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "#cccccc", linetype = "dashed", size = 0.5) +
      theme_minimal() +
      labs(x = "Accès à l'Électricité (%)", y = "Utilisateurs Internet (%)")
    
    ggplotly(p)
  })
  
  output$mobile_evolution_area <- renderHighchart({
    req(input$selected_countries)
    
    df_mobile <- data() %>% 
      filter(country %in% input$selected_countries) %>%
      mutate(year = as.numeric(as.character(year)))
    
    hchart(df_mobile, "area", hcaes(x = year, y = mobile_users, group = country)) %>%
      hc_title(text = "Expansion du Parc Mobile") %>%
      hc_yAxis(title = list(text = "Abonnés mobile (pour 100 hab.)")) %>%
      hc_plotOptions(area = list(stacking = "normal", marker = list(enabled = FALSE))) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$digital_divide_plot <- renderGirafe({
    df_div <- filtered_data() %>%
      mutate(gap = electricity_access - internet_users)
    
    gg <- ggplot(df_div, aes(x = reorder(country, gap), y = gap)) +
      geom_bar_interactive(aes(fill = gap, tooltip = paste("Ecart:", round(gap, 1), "%")), stat = "identity") +
      coord_flip() +
      scale_fill_gradient(low = "#9ecae1", high = "#08306b") +
      theme_minimal() +
      labs(x = "", y = "Écart Électricité - Internet (%)")
    
    girafe(ggobj = gg, width_svg = 6, height_svg = 4)
  })
  
  output$edu_transition_matrix <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = literacy_rate, y = secondary_school_female, text = country)) +
      geom_point(aes(size = population, color = official_language), alpha = 0.7) +
      geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
      theme_minimal() +
      labs(x = "Taux d'alphabétisation global (%)", y = "Scolarisation Filles (sec.) %")
    
    ggplotly(p)
  })
  
  output$dependency_bar_advanced <- renderHighchart({
    df_dep <- filtered_data() %>% arrange(desc(dependency_ratio))
    
    hchart(df_dep, "bar", hcaes(x = country, y = dependency_ratio), 
           name = "Ratio de Dépendance", 
           color = "#e67e22") %>% # La couleur se met directement ici pour un bar chart simple
      hc_title(text = "Nombre de dépendants pour 100 actifs") %>%
      hc_tooltip(pointFormat = "Pour 100 travailleurs, il y a <b>{point.y}</b> personnes à charge (enfants/vieux).") %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  
  
  ##  ---  COTE SERVER PAGE SANTE ET PAUVRETE --
  
  output$mortality_scatter <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = infant_mortality, y = maternal_mortality, text = country)) +
      geom_point(aes(size = population, color = infant_mortality), alpha = 0.7) +
      scale_color_gradient(low = "#ffccbc", high = "#d84315") +
      theme_minimal() +
      labs(x = "Mortalité Infantile (pour 1000)", y = "Mortalité Maternelle (pour 100 000)")
    
    ggplotly(p)
  })
  
  output$gini_polar_chart <- renderHighchart({
    df_gini <- filtered_data() %>% arrange(desc(gini_index))
    
    hchart(df_gini, "column", hcaes(x = country, y = gini_index), polar = TRUE) %>%
      hc_title(text = "Indice d'Inégalités (Gini)") %>%
      hc_colors("#5e35b1") %>% # Violet pour la justice sociale
      hc_yAxis(max = 100, title = list(text = "Score Gini")) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$poverty_gini_bubble <- renderGirafe({
    gg <- ggplot(filtered_data(), aes(x = poverty_rate, y = gini_index)) +
      geom_point_interactive(aes(size = population, color = official_language, 
                                 tooltip = paste(country, "\nGini:", gini_index, "\nPauvreté:", poverty_rate, "%")), 
                             alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "grey", linetype = "dashed") +
      theme_minimal() +
      labs(x = "Taux de Pauvreté (%)", y = "Indice Gini (Inégalités)")
    
    girafe(ggobj = gg, width_svg = 6, height_svg = 4)
  })
  
  output$social_emergency_table <- renderDT({
    tab <- filtered_data() %>%
      select(country, poverty_rate, infant_mortality, gini_index) %>%
      rename(Pays = country, `Pauvreté (%)` = poverty_rate, `Mortalité Inf.` = infant_mortality, `Gini` = gini_index)
    
    datatable(tab, options = list(dom = 't', pageLength = 5)) %>%
      formatStyle('Mortalité Inf.', 
                  background = styleColorBar(range(filtered_data()$infant_mortality, na.rm=T), 'pink'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  
  
  ##  ---  COTE SERVER PAGE COMPOSITE ---
  
  # Fonction de normalisation (0-100)
  normalize_index <- function(x) {
    if(all(is.na(x))) return(x)
    rng <- range(x, na.rm = TRUE)
    if(rng[1] == rng[2]) return(rep(50, length(x))) 
    (x - rng[1]) / (rng[2] - rng[1]) * 100
  }
  
  # Objet réactif pour l'indice (Calcul ACP)
  reactive_composite <- reactive({
    req(data_enriched(), input$selected_countries)
    
    # On prend toutes les années pour les pays sélectionnés, pas seulement filtered_data()
    df_pca_raw <- data_enriched() %>%
      filter(country %in% input$selected_countries) %>% 
      mutate(
        inv_mortality = 1 / (infant_mortality + 1),
        gdp_log = log(gdp_per_capita + 1)
      ) %>%
      select(country, year, gdp_log, electricity_access, life_expectancy, 
             urban_population, literacy_rate, inv_mortality, internet_users) %>%
      na.omit()
    
    # Le calcul de l'ACP
    pca_res <- FactoMineR::PCA(df_pca_raw[, 3:9], scale.unit = TRUE, ncp = 1, graph = FALSE)
    raw_scores <- pca_res$ind$coord[, 1]
    
    if(cor(raw_scores, df_pca_raw$gdp_log) < 0) raw_scores <- -raw_scores
    
    df_pca_raw$score_final <- (raw_scores - min(raw_scores)) / (max(raw_scores) - min(raw_scores)) * 100
    
    # Normalisation des piliers
    df_pca_raw <- df_pca_raw %>%
      mutate(
        eco   = normalize_index(gdp_log),
        soc   = normalize_index(literacy_rate),
        infra = normalize_index(electricity_access),
        sante = normalize_index(life_expectancy),
        urb   = normalize_index(urban_population)
      )
    
    return(df_pca_raw)
  })
  
  # Graphique des contributions
  output$var_contributions_plot <- renderHighchart({
    req(reactive_composite())
    
    # Extraction des contributions de l'ACP
    df_pca_input <- reactive_composite() %>% 
      select(gdp_log, electricity_access, life_expectancy, urban_population, literacy_rate, inv_mortality, internet_users)
    
    pca_res <- FactoMineR::PCA(df_pca_input, scale.unit = TRUE, ncp = 1, graph = FALSE)
    contribs <- as.data.frame(pca_res$var$contrib[, 1])
    contribs$Variable <- c("Richesse (PIB)", "Électricité", "Santé", "Urbanisation", "Éducation", "Mortalité (Inv)", "Internet")
    colnames(contribs)[1] <- "Valeur"
    
    hchart(contribs %>% arrange(desc(Valeur)), "column", hcaes(x = Variable, y = Valeur, color = Valeur)) %>%
      hc_colorAxis(stops = color_stops(5, rev(viridisLite::inferno(10)))) %>%
      hc_yAxis(title = list(text = "Contribution (%)")) %>%
      hc_tooltip(pointFormat = "Impact : {point.y:.1f}%")
  })
  
  # Score Moyen
  output$avg_score_text <- renderText({
    req(reactive_composite(), input$selected_year)
    # Filtrer sur l'année sélectionnée avant de faire la moyenne
    df_annee <- reactive_composite() %>% filter(year == input$selected_year)
    moyenne <- mean(df_annee$score_final, na.rm = TRUE)
    paste0(round(moyenne, 1), "/100")
  })
  
  # Graphique Barres (Classement)
  output$composite_rank_bar <- renderHighchart({
    req(reactive_composite(), input$selected_year)
    
    # Filtrage
    df <- reactive_composite() %>% 
      filter(year == input$selected_year) %>% 
      arrange(desc(score_final))
    
    hchart(df, "bar", hcaes(x = country, y = score_final, color = score_final)) %>%
      hc_title(text = paste("Performance Multidimensionnelle en", input$selected_year)) %>%
      hc_colorAxis(stops = color_stops(5, rev(viridisLite::inferno(10))))
  })
  
  # Radar Chart
  output$radar_profil_pays <- renderHighchart({
    req(input$country_radar, input$selected_year, reactive_composite())
    
    # Filtrage (country et year)
    df_radar <- reactive_composite() %>%
      filter(country == input$country_radar, year == input$selected_year) %>%
      select(eco, soc, infra, sante, urb) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Dimension", values_to = "Score")
    
    highchart() %>%
      hc_chart(polar = TRUE, type = "area") %>%
      hc_xAxis(categories = df_radar$Dimension) %>%
      hc_yAxis(min = 0, max = 100, gridLineInterpolation = 'polygon') %>%
      hc_add_series(name = paste(input$country_radar, "-", input$selected_year), 
                    data = df_radar$Score, color = "#2171b5", fillOpacity = 0.4)
  })
  
  # Heatmap
  output$heatmap_composite <- renderHighchart({
    req(reactive_composite(), input$selected_year)
    
    # Filtrage
    df_heat <- reactive_composite() %>%
      filter(year == input$selected_year) %>%
      select(country, eco, soc, infra, sante, urb) %>%
      tidyr::pivot_longer(cols = -country, names_to = "Pilier", values_to = "Score")
    
    hchart(df_heat, "heatmap", hcaes(x = Pilier, y = country, value = round(Score,1))) %>%
      hc_title(text = paste("Comparaison des Piliers en", input$selected_year)) %>%
      hc_colorAxis(stops = color_stops(5, rev(viridisLite::magma(10))))
  })
  
  # Mise à jour sélecteur radar
  observe({
    req(input$selected_countries)
    updateSelectInput(session, "country_radar", choices = input$selected_countries)
  })
  
  
  
  ##  ---  COTE SERVER PAGE PREDICTIONS ET ML ---
  
  # Données filtrées pour la vue annuelle (Page 1 à 5)
  filtered_data <- reactive({
    req(input$selected_countries, input$selected_year)
    data_enriched() %>%
      filter(country %in% input$selected_countries,
             year == input$selected_year)
  })
  
  # --- Rendre le sélecteur d'année visible ---
  output$ui_rank_year_pred <- renderUI({
    selectInput("rank_year_p", "Année cible du classement :", 
                choices = 2025:2030, selected = 2030)
  })
  
  # Moteur de calcul
  predict_engine <- reactive({
    req(input$selected_countries, input$predict_var)
    
    df_source <- if(input$predict_var == "score_final") {
      req(reactive_composite())
      reactive_composite()
    } else {
      req(data_enriched())
      data_enriched()
    }
    
    future_years <- 2025:2030
    results <- data.frame()
    
    for(cntry in input$selected_countries) {
      country_df <- df_source %>% 
        filter(country == cntry) %>% 
        select(country, year, val = !!sym(input$predict_var)) %>%
        filter(!is.na(val)) %>% arrange(year)
      
      if(nrow(country_df) >= 3) {
        model <- lm(val ~ year, data = country_df)
        preds <- predict(model, newdata = data.frame(year = future_years))
        
        if(!input$predict_var %in% c("inflation", "gdp_growth", "edu_gap")) preds <- pmax(preds, 0)
        
        hist <- country_df %>% select(country, year, value = val) %>% mutate(type = "Historique")
        proj <- data.frame(country = cntry, year = future_years, value = as.numeric(preds), type = "Projection")
        
        # On lie l'historique à la projection
        last_point <- hist %>% filter(year == max(year)) %>% mutate(type = "Projection")
        results <- rbind(results, hist, last_point, proj)
      }
    }
    return(results)
  })
  
  # Rendu du sélecteur d'année
  output$ui_rank_year_pred <- renderUI({
    selectInput("rank_year_p", "Année cible du classement :", 
                choices = 2025:2030, selected = 2030)
  })
  
  # 3. Graphique de Gauche (Trajectoire)
  output$forecast_plot <- renderHighchart({
    req(predict_engine())
    df <- predict_engine()
    
    hc <- highchart() %>%
      hc_xAxis(title = list(text = "Année")) %>%
      hc_yAxis(title = list(text = input$predict_var)) %>%
      hc_tooltip(shared = TRUE)
    
    for(cntry in unique(df$country)){
      df_h <- df %>% filter(country == cntry, type == "Historique")
      df_p <- df %>% filter(country == cntry, type == "Projection")
      
      hc <- hc %>% 
        hc_add_series(name = paste(cntry, "(Hist)"), data = list_parse(df_h %>% select(x = year, y = value)), dashStyle = "Solid") %>%
        hc_add_series(name = paste(cntry, "(Proj)"), data = list_parse(df_p %>% select(x = year, y = value)), dashStyle = "Dash", linkedTo = ":previous")
    }
    hc
  })
  
  # Graphique de Droite (Classement)
  output$pred_rank_chart <- renderHighchart({
    req(predict_engine(), input$rank_year_p)
    df_rank <- predict_engine() %>% 
      filter(year == input$rank_year_p, type == "Projection") %>%
      arrange(desc(value))
    
    hchart(df_rank, "column", hcaes(x = country, y = value)) %>%
      hc_colors("#f39c12") %>%
      hc_title(text = paste("Projection", input$rank_year_p))
  })
  
  # Segmentation Avancée (Clustering Auto-adaptatif)
  output$cluster_plot <- renderPlotly({
    req(predict_engine())
    
    # Préparation des données
    df_clust <- predict_engine() %>%
      group_by(country) %>%
      summarise(
        current = value[type == "Historique" & year == max(year[type == "Historique"])][1],
        future = value[year == 2030][1],
        .groups = 'drop'
      ) %>%
      mutate(growth = (future - current) / pmax(current, 0.1) * 100) %>%
      na.omit()
    
    # On ne lance l'algorithme que si on a assez de pays
    if(nrow(df_clust) > 4) {
      
      # Normalisation des données (Indispensable pour comparer Niveau et Croissance)
      matrix_clust <- scale(df_clust[, 2:3])
      
      # Calcul de la distance euclidienne et Clustering Hiérarchique (Méthode de Ward)
      dist_mat <- dist(matrix_clust, method = "euclidean")
      hc_model <- hclust(dist_mat, method = "ward.D2")
      
      # Détermination automatique du nombre de groupes (Optimisation de la silhouette)
      # On teste de 2 à 4 clusters et on prend le plus cohérent
      suppressWarnings({
        pkgs <- c("cluster")
        if(!require(cluster)) install.packages("cluster")
        library(cluster)
        
        avg_sil <- sapply(2:4, function(k) {
          cut_hc <- cutree(hc_model, k = k)
          summary(silhouette(cut_hc, dist_mat))$avg.width
        })
        best_k <- which.max(avg_sil) + 1
      })
      
      # Application du meilleur découpage
      df_clust$cluster <- as.factor(cutree(hc_model, k = best_k))
      
      # Création du graphique interactif
      p <- ggplot(df_clust, aes(x = current, y = growth, color = cluster, text = country)) +
        geom_point(size = 5, alpha = 0.8) +
        geom_vline(xintercept = mean(df_clust$current), linetype = "dashed", color = "grey") +
        geom_hline(yintercept = mean(df_clust$growth), linetype = "dashed", color = "grey") +
        scale_color_brewer(palette = "Set1") +
        labs(
          title = paste("Analyse de Segmentation Automatique (", best_k, "groupes détectés )"),
          x = "Niveau Actuel (Indicateur)",
          y = "Dynamique de Croissance (%)"
        ) +
        theme_minimal()
      
      ggplotly(p, tooltip = "text") %>%
        layout(annotations = list(
          x = 1, y = -0.1, text = "Pointillés : Moyennes régionales", 
          showarrow = F, xref='paper', yref='paper', font=list(size=10)
        ))
      
    } else {
      # Message si pas assez de données
      style_text <- list(x = 0.5, y = 0.5, text = "Besoin d'au moins 5 pays pour segmenter", showarrow = F)
      plot_ly() %>% layout(annotations = style_text)
    }
  })
  
  # Rendu de la Table (Page Exploration)
  output$table_raw <- renderDT({
    datatable(data(), options = list(pageLength = 10, scrollX = TRUE), filter = 'top')
  })
}