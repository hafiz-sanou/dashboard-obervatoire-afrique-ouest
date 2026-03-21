# Liste des packages nécessaires
packages <- c("WDI","tidyverse","countrycode")

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
library(WDI)
library(tidyverse)
library(countrycode)

countries <- c(
  "SEN", # Sénégal
  "MLI", # Mali
  "BFA", # Burkina Faso
  "NER", # Niger
  "CIV", # Côte d'Ivoire
  "BEN", # Bénin
  "TGO", # Togo
  "GIN", # Guinée
  "NGA", # Nigeria
  "GHA", # Ghana
  "SLE", # Sierra Leone
  "LBR", # Libéria
  "GMB", # Gambie
  "GNB", # Guinée-Bissau
  "CPV"  # Cap-Vert
)

indicators <- c(
  
  # DEMOGRAPHIE
  population = "SP.POP.TOTL",
  population_growth = "SP.POP.GROW",
  fertility_rate = "SP.DYN.TFRT.IN",
  life_expectancy = "SP.DYN.LE00.IN",
  urban_population = "SP.URB.TOTL.IN.ZS",
  
  # ECONOMIE
  gdp = "NY.GDP.MKTP.CD",
  gdp_per_capita = "NY.GDP.PCAP.CD",
  unemployment = "SL.UEM.TOTL.ZS",
  gdp_growth = "NY.GDP.MKTP.KD.ZG",
  gdp_deflator_growth = "NY.GDP.DEFL.KD.ZG",
  custumer_price_index = "FP.CPI.TOTL",
  
  # SOCIAL
  literacy_rate = "SE.ADT.LITR.ZS",
  electricity_access = "EG.ELC.ACCS.ZS",
  internet_users = "IT.NET.USER.ZS",
  
  # EDUCATION GENRE
  secondary_school_female = "SE.SEC.ENRR.FE",
  secondary_school_male = "SE.SEC.ENRR.MA",
  
  # SANTE
  infant_mortality = "SP.DYN.IMRT.IN",
  maternal_mortality = "SH.STA.MMRT",
  
  # INEGALITES
  poverty_rate = "SI.POV.DDAY",
  gini_index = "SI.POV.GINI",
  
  # DEMOGRAPHIE STRUCTURE
  dependency_ratio = "SP.POP.DPND",
  
  # DIGITAL
  mobile_users = "IT.CEL.SETS.P2"
)

data_raw <- WDI(
  country = countries,
  indicator = indicators,
  start = 2014,
  end = 2024,
  extra = TRUE
)

View(data_raw)

languages <- tibble(
  iso2c = c("SN","ML","BF","NE","CI","BJ","TG","GN","NG","GH","SL","LR","GM","GW","CV"),
  official_language = c(
    "French","French","French","French","French",
    "French","French","French",
    "English","English","English","English","English",
    "Portuguese","Portuguese"
  )
)
data <- data_raw %>%
  left_join(languages, by = "iso2c")

dataset_final <- data %>%
  select(
    country,
    iso2c,
    official_language,
    year,
    population,
    population_growth,
    fertility_rate,
    life_expectancy,
    urban_population,
    gdp,
    gdp_per_capita,
    unemployment,
    gdp_growth,
    gdp_deflator_growth,
    custumer_price_index,
    literacy_rate,
    electricity_access,
    internet_users,
    secondary_school_female,
    secondary_school_male,
    infant_mortality,
    maternal_mortality,
    poverty_rate,
    gini_index,
    dependency_ratio,
    mobile_users
  )

head(dataset_final)

write.csv(dataset_final, "west_africa_dashboard_data.csv", row.names = FALSE)