# Carregar dependências
library(here)
library(dplyr)
library(DBI)
library(RSQLite)
library(tibble)

# Configurações globais
BASE_URL <- "https://population.un.org/dataportalapi/api/v1"
HEADER <- c("Authorization" = Sys.getenv("WPP_ONU_BEARER"))
DB_PATH <- here::here("data", "population_data.sqlite")
source('R/download_pages_WPP.R')

# Verificar se o banco já existe
if (!file.exists(DB_PATH)) {
  source("R/create_database.R")
}

# SQL simples com essas constantes aí em cima
sql_basic = function(query){
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  df = dbGetQuery(con, query)
  dbDisconnect(con)
  return(df)
}

# Função para processar dados de um local e ano específico
process_location_data <- function(locationId, timeLabel, indicatorId) {
  target <- paste0(BASE_URL,
                   "/data/indicators/", indicatorId,
                   "/locations/", locationId,
                   "/start/", timeLabel, "/end/", timeLabel,
                   "?pagingInHeader=false&format=json")
  # Fazer o download e processar os dados
  download_pages_WPP(target, HEADER)
}

insert_if_not_exists <- function(conn, table_name, data, key_columns) {
  # Ler os registros existentes da tabela
  existing_data <- dbReadTable(conn, table_name)
  # Identificar os novos registros com base nas colunas-chave
  new_data <- anti_join(data, existing_data, by = key_columns)
  # Inserir apenas os novos registros
  if (nrow(new_data) > 0) {
    dbWriteTable(conn, table_name, new_data, append = TRUE, row.names = FALSE)
  }
}

# Função para salvar dados no banco de dados
save_to_database <- function(data, db) {
  #save indicator
  indicators = data |>
    select(indicatorId, indicator, indicatorDisplayName) |>
    distinct()
  insert_if_not_exists(db, "indicators", indicators, key_columns = "indicatorId")

  #save source
  sources = data |>
    select(sourceId, source, revision) |>
    distinct()
  insert_if_not_exists(db, "sources", sources, key_columns = "sourceId")

  #save variant
  variants = data |>
    select(variantId, variant, variantShortName, variantLabel) |>
    distinct()
  insert_if_not_exists(db, "variants", variants, key_columns = "variantId")

  #save time
  times = data |>
    select(timeId, timeLabel, timeMid) |>
    distinct()
  insert_if_not_exists(db, "times", times, key_columns = "timeId")

  #save estimates
  estimates = data |>
    select(estimateTypeId, estimateType, estimateMethodId, estimateMethod) |>
    distinct()
  insert_if_not_exists(db, "estimates", estimates, key_columns = c("estimateTypeId","estimateMethodId"))

  #save sex
  sexs = data |>
    select(sexId, sex) |>
    distinct()
  insert_if_not_exists(db, "sexs", sexs, key_columns = "sexId")

  #save ages
  ages = data |>
    select(ageId, ageLabel,ageStart,ageEnd,ageMid) |>
    distinct()
  insert_if_not_exists(db, "ages", ages, key_columns = "ageId")
  
  #save categories
  categories = data |>
    select(categoryId, category) |>
    distinct()
  insert_if_not_exists(db, "categories", categories, key_columns = "categoryId")

  #save values
  values = data |>
    select(timeId,locationId, indicatorId, sexId, ageId, sourceId, variantId, categoryId,
      estimateTypeId, estimateMethodId, value)
  dbWriteTable(db, "data", values, append = TRUE, row.names = FALSE)
}

# Função para salvar metadados no banco de dados
save_metadata <- function(locationId, timeId, indicatorId, db) {
  metadata <- data.frame(
    timeId = timeId,
    locationId = locationId,
    indicatorId = indicatorId,
    processed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    status = "Processed"
  )
  dbWriteTable(db, "metadata_data", metadata, append = TRUE, row.names = FALSE)
}

# Função para verificar se os dados já foram processados
check_metadata <- function(timeId, locationId, indicatorId, db) {
  sql_query = paste0("SELECT COUNT(*) AS count FROM metadata_data WHERE timeId = ", timeId,
                     " AND locationId = ", locationId, 
                     " AND indicatorId = ", indicatorId,
                     " AND status = 'Processed'")
  query <- dbGetQuery(db, sql_query)
  if (is.null(query$count) || is.na(query$count)) {
    return(FALSE)
  }
  return(query$count > 0)
}

insert_all_data <- function(locations, times, indicators, db) {
  for (t in 1:nrow(times)) {
    timeLabel = times[t,]$timeLabel
    timeId=times[t,]$timeId
    for (l in 1:nrow(locations)) {
      location = locations[l,]$location
      locationId = locations[l,]$locationId
      for (i in 1:nrow(indicators)) {
        indicator = indicators[i,]$indicator
        indicatorId = indicators[i,]$indicatorId
        cat(paste0("\nProcessing Year: ", timeLabel, " (", timeId, ")", 
                   " - Location: ", location, " (", locationId, ")", 
                   " - Indicator: ", indicator, " (", indicatorId, ")", 
                   "\n"))
        if (check_metadata(timeLabel, locationId, indicatorId, db)) {
          cat(paste0("\n>>>>> Skipping: Year: ", timeLabel, " (", timeId, ")", 
                     " - Location: ", location, " (", locationId, ")", 
                     " - Indicator: ", indicator, " (", indicatorId, ")", 
                     "\n"))
          next
        }
        wpp_raw <- process_location_data(locationId, timeLabel, indicatorId)
        save_metadata(locationId, timeLabel, indicatorId, db)
        save_to_database(wpp_raw, db)
      }
    }
  }
}

# ----
#EXECUÇÃO DO PROGRAMA
main_insert <- function(locations, times, imdicators) {
  # Conectar ao banco de dados
  db <- dbConnect(RSQLite::SQLite(), DB_PATH)
  # Processar os dados de população
  insert_all_data(locations, times, indicators, db)
  # Fechar conexão
  RSQLite::dbDisconnect(db)
  cat("\rProcessing completed successfully!\n")
}

con <- dbConnect(RSQLite::SQLite(), DB_PATH)
dbExecute(con, "VACUUM;")
dbDisconnect(con)
rm(con)

