# Carregar dependências
library(here)
library(dplyr)
library(DBI)
library(RSQLite)

DB_PATH <- here::here("data", "population_data.sqlite")
BASE_URL <- "https://population.un.org/dataportalapi/api/v1"
HEADER <- c("Authorization" = Sys.getenv("WPP_ONU_BEARER"))
source('R/download_pages_WPP.R')

#Criar tabelas, se necessário
db = dbConnect(RSQLite::SQLite(), DB_PATH)

# ---- Data ----
dbExecute(
  db,
  "CREATE TABLE data (
      timeId INTEGER NOT NULL,
      locationId INTEGER NOT NULL,
      indicatorId INTEGER NOT NULL,
      sexId INTEGER NOT NULL,
      ageId INTEGER NOT NULL,
      categoryId INTEGER NOT NULL,
      sourceId INTEGER NOT NULL,
      variantId INTEGER NOT NULL,
      estimateTypeId INTEGER NOT NULL,
      estimateMethodId INTEGER NOT NULL,
      value REAL NOT NULL,
      FOREIGN KEY (timeId) REFERENCES times (timeId) ON DELETE CASCADE,
      FOREIGN KEY (categoryId) REFERENCES categories (categoryId) ON DELETE CASCADE,
      FOREIGN KEY (locationId) REFERENCES locations (locationId) ON DELETE CASCADE,
      FOREIGN KEY (indicatorId) REFERENCES indicators (indicatorId) ON DELETE CASCADE,
      FOREIGN KEY (sexId) REFERENCES sexs (sexId),
      FOREIGN KEY (ageId) REFERENCES ages (ageId),
      FOREIGN KEY (variantId) REFERENCES variants (variantId) ON DELETE CASCADE,
      FOREIGN KEY (estimateTypeId,estimateMethodId) REFERENCES estimates (estimateTypeId, estimateMethodId),
      FOREIGN KEY (sourceId) REFERENCES sources (sourceId),
      PRIMARY KEY (timeId, locationId, indicatorId, sexId, ageId, categoryId, sourceId, variantId, estimateTypeId, estimateMethodId)
    )"
)

# ---- MetaData ----

dbExecute(db, "CREATE TABLE metadata_data (
      timeId INTEGER NOT NULL,
      locationId INTEGER NOT NULL,
      indicatorId INTEGER NOT NULL,
      processed_at TEXT NOT NULL,
      status TEXT NOT NULL,
      PRIMARY KEY (timeId, locationId, indicatorId)
    )")

# ---- categories ----
dbExecute(db, "CREATE TABLE categories (
      categoryId INTEGER NOT NULL,
      category TEXT NOT NULL,
      PRIMARY KEY (categoryId)
    )")

# ---- variants ----
dbExecute(db, "CREATE TABLE variants (
      variantId INTEGER NOT NULL,
      variant TEXT NOT NULL,
      variantShortName TEXT NOT NULL,
      variantLabel TEXT NOT NULL,
      PRIMARY KEY (variantId)
    )")

# ---- estimates ----
dbExecute(db, "CREATE TABLE estimates (
      estimateTypeId INTEGER NOT NULL,
      estimateType TEXT NOT NULL,
      estimateMethodId INTEGER NOT NULL,
      estimateMethod TEXT NOT NULL,
      PRIMARY KEY (estimateTypeId, estimateMethodId)
    )")

# ---- sexs ----
dbExecute(db, "CREATE TABLE sexs (
      sexId INTEGER NOT NULL,
      sex TEXT NOT NULL,
      PRIMARY KEY (sexId)
    )")

# ---- ages ----
dbExecute(db, "CREATE TABLE ages (
      ageId INTEGER NOT NULL,
      ageLabel TEXT NOT NULL,
      ageStart INTEGER NOT NULL,
      ageEnd INTEGER,
      ageMid REAL NOT NULL,
      PRIMARY KEY (ageId)
    )")

# ---- sources ----
dbExecute(db, "CREATE TABLE sources (
      sourceId INTEGER NOT NULL,
      source TEXT NOT NULL,
      revision INTEGER NOT NULL,
      PRIMARY KEY (sourceId)
    )")

# ---- Locations -----

make_table_sql <- function(df, table_name, pk = NULL) {
  fields <- sapply(df, function(col) {
    if (is.numeric(col)) "REAL"
    else if (is.integer(col)) "INTEGER"
    else "TEXT"
  })
  
  field_defs <- paste(names(fields), fields, collapse = ", ")
  if (!is.null(pk)) {
    field_defs <- c(field_defs, sprintf("PRIMARY KEY (%s)", pk))
  }
  sql <- sprintf("CREATE TABLE %s (\n  %s\n)", table_name, paste(field_defs, collapse = ",\n  "))
  return(sql)
}

# Nações Unidas
destfile_onu <- here::here("data", "WPP2024_F01_LOCATIONS.xlsx")
download.file('https://population.un.org/wpp/assets/Excel%20Files/4_Metadata/WPP2024_F01_LOCATIONS.xlsx',
              destfile = destfile_onu, mode = "wb")
locations = readxl::read_xlsx(destfile_onu, sheet = 'DB')|>
  dplyr::rename(
    idx = Index,
    locationId=LocID,
    location=Location,
    location_iso2code=ISO2_Code,
    location_iso3code=ISO3_Code)

# IBGE
destfile_ibge <- here::here("data", "paises_e_territorios_codigos_e_abreviacoes.xls")
download.file('https://ftp.ibge.gov.br/Registro_Civil/Codigos_dos_paises/paises_e_territorios_codigos_e_abreviacoes.xls',
              destfile = destfile_ibge, mode = "wb")

locations_ibge = readxl::read_xls(destfile_ibge,
                              sheet = 'Planilha1', col_names = c('locationId_IBGE', 'location_pt','ISO3_Code'),
                              range = 'A4:C244') |>
  dplyr::filter(!is.na(ISO3_Code)) |>
  inner_join(locations, by = c('ISO3_Code' = 'location_iso3code')) |>
  select('locationId_IBGE', 'location_pt', 'idx')

locations_outras = readxl::read_xlsx(here::here("data", "locations_traduzidas.xlsx"))|>
  dplyr::select(
    idx = Index,
    location_pt=location_pt_revisada)

locations_pt = locations_ibge |>
  bind_rows(locations_outras)

locations = locations |>
  dplyr::left_join(locations_pt, by = 'idx')

sql = make_table_sql(locations, "locations", pk = "locationId")
dbExecute(db, sql)
dbWriteTable(db, "locations", locations, append = TRUE)
rm(locations, locations_ibge, locations_pt, locations_outras)

# ---- indicators -----

indicators = jsonlite::fromJSON(RCurl::getURL(
  'https://population.un.org/dataportalapi/api/v1/indicators',
  .opts = list(httpheader = HEADER, followlocation = TRUE)))$data |>
  rename(indicatorId = id,
         indicator = name,
         indicatorDisplayName = displayName)

sql = make_table_sql(indicators, "indicators", pk = "indicatorId")
dbExecute(db, sql)
dbWriteTable(db, "indicators", indicators, append = TRUE)
rm(indicators)

# ---- times -----
times = jsonlite::fromJSON(RCurl::getURL('https://population.un.org/dataportalapi/api/v1/dimensions/times/None',
                                         .opts = list(httpheader = HEADER, followlocation = TRUE))) |>
  rename(timeId = id,
         timeLabel = label)

sql = make_table_sql(times, "times", pk = "timeId")
dbExecute(db, sql)
dbWriteTable(db, "times", times, append = TRUE)
rm(times)

# Fechar conexão
dbExecute(db, "PRAGMA foreign_keys = ON;")
dbDisconnect(db)
rm(db)
cat("Processing completed successfully!\n")
