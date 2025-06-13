# Conexão ao banco de dados
db_connect <- function() {
  db_path <- here::here("data", "population_data.sqlite")
  RSQLite::dbConnect(RSQLite::SQLite(), db_path)
}

# Função para executar consultas SQL
db_query <- function(query, conn, params = NULL) {
  if (!is.null(params)) {
    return(RSQLite::dbGetQuery(conn, query, params = params))
  }
  return(RSQLite::dbGetQuery(conn, query))
}

# Encontrar localização
loc_find <- function(input, conn) {
  query <-
  " SELECT DISTINCT l.*
    FROM locations AS l
    INNER JOIN data AS dpas
        ON l.locationId = dpas.locationId
    WHERE l.location_iso3code = ?
       OR l.location_iso2code = ?
       OR l.locationId = ?
       OR LOWER(l.location) = LOWER(?);"
  input_upper <- toupper(input)
  numeric_input <- suppressWarnings(as.numeric(input))
  result <- db_query(query, conn, params = list(input_upper, input_upper, numeric_input, input))
  # print('location')
  # print(result)
  if (nrow(result) == 0) return(NULL)
  result
}

# Encontrar ano
year_find <- function(input, conn) {
  query <- "
    SELECT DISTINCT t.*
    FROM times as t
    INNER JOIN data AS dpas
        ON t.timeId = dpas.timeId
    WHERE timeLabel = ?
       OR timeMid = ?;
  "
  result <- db_query(query, conn, params = list(as.character(input), as.character(input)))
  # print('year')
  # print(result)
  if (nrow(result) == 0) return(NULL)
  result
}
# Fatores de sexo e idade ano
construct_factors_age_and_sex <- function(db, pop_data) {
  query_age <- paste0(
  "SELECT DISTINCT a.ageId, a.ageLabel, a.ageStart, a.ageEnd, a.ageMid
  FROM ages a
  INNER JOIN data d
  ON a.ageId = d.ageId
  ORDER BY a.ageStart;")
  ages <- db_query(query_age, db, params = NULL)
  if (nrow(ages) == 0) return(NULL)

  query_age <- paste0(
    "SELECT DISTINCT s.sexId, s.sex
  FROM sexs s
  INNER JOIN data d
  ON s.sexId = d.sexId
  WHERE s.sexId = 1 OR s.sexId = 2
  ORDER BY s.sexId;")
  sexs <- db_query(query_age, db, params = NULL)
  if (nrow(sexs) == 0) return(NULL)
  list(ages=ages, sexs=sexs)
}

#' Generate Population Pyramid Data
#'
#' The `pop_pyr_data` function retrieves and processes population data for a given location and year
#' from the World Population Prospects database. It returns a dataset formatted for creating
#' population pyramid visualizations.
#'
#' @param location A character or numeric input specifying the location. This can be:
#'   - ISO3 code (e.g., "BRA" for Brazil)
#'   - ISO2 code (e.g., "BR")
#'   - A numeric `locationId` (e.g., 76 for Brazil)
#'   - The name of the location (e.g., "Brazil").
#' @param year A character or numeric input specifying the year. This can be:
#'   - The year label (e.g., "2020").
#'   - The `timeMid` value (e.g., 2020).
#'
#' @return A dataframe containing the processed population data, including the following columns:
#'   - `ageId`: Age group ID.
#'   - `sexId`: Sex ID (1 for Male, 2 for Female).
#'   - `sexage_population`: Population count for each sex and age group (negative for males, positive for females).
#'   - `total_population`: Total population across all groups.
#'   - `relative_population`: Proportional population for each group relative to the total population.
#'   - `age_f`: Factorized age group for visualization.
#'   - `sex_f`: Factorized sex group for visualization.
#'
#' @details The function retrieves data from a SQLite database (`population_data.sqlite`) included in the package.
#'   The database must include the required tables:
#'   - `locations`: Contains location information.
#'   - `times`: Contains temporal data.
#'   - `data`: Contains population data.
#'
#' @examples
#' # Generate population pyramid data for Brazil in 2020
#' data <- pop_pyr_data("Brazil", 2000)
#' head(data)
#'
#' @export
pop_pyr_data <- function(location, year, variant = c("Median","all")) {
  conn <- db_connect() # Conectar ao banco

  # Localização e ano
  loc_data <- loc_find(location, conn)
  year_data <- year_find(year, conn)
  if (is.null(loc_data) || is.null(year_data)) {
    stop("Location or time inexistent or not available") }

  loc_id <- loc_data$locationId
  year_id <- year_data$timeId

  # Consulta de dados populacionais
  VARIANT_ID <- 4
  query <- paste0(
    "SELECT * FROM data WHERE locationId = ", loc_id,
    " AND variantId =",VARIANT_ID, " AND timeId = ", year_id,
    " AND ageId != 188",
    " AND (sexId = 1 OR sexId = 2);"
  )
  pop_data <- db_query(query, conn)
  age_and_sex_f = construct_factors_age_and_sex(conn,pop_data)
  on.exit(RSQLite::dbDisconnect(conn)) # Garantir desconexão
  metadata = list(variant='Median', location =loc_data, year=year_data,source='')

  # Processar os dados
  data <- pop_data |>
    dplyr::group_by(.data$locationId, .data$sexId, .data$ageId) |>
    dplyr::summarise(sexage_population = sum(.data$value), .groups = 'drop') |>
    dplyr::ungroup() |>
    dplyr::mutate(
      sexage_population = dplyr::if_else(.data$sexId == 1, -.data$sexage_population, .data$sexage_population),
      total_population = sum(abs(.data$sexage_population)),
      relative_population = .data$sexage_population / .data$total_population,
      age_f = factor(.data$ageId, ordered=TRUE,
                     levels=age_and_sex_f$ages$ageId,
                     labels=age_and_sex_f$ages$ageLabel),
      sex_f = factor(.data$sexId,
                     levels=age_and_sex_f$sexs$sexId,
                     labels=age_and_sex_f$sexs$sex)
    )
  return(data)
}
#' Generate a Population Pyramid
#'
#' The `pop_pyr` function generates a population pyramid for a specified location and year
#' using data from the World Population Prospects. The pyramid is visualized as a
#' horizontal bar chart, showing the distribution of population by age and sex.
#'
#' @param location A character or numeric input specifying the location. This can be:
#'   - ISO3 code (e.g., "BRA" for Brazil)
#'   - ISO2 code (e.g., "BR")
#'   - A numeric `locationId` (e.g., 76 for Brazil)
#'   - The name of the location (e.g., "Brazil").
#' @param year A character or numeric input specifying the year. This can be:
#'   - The year label (e.g., "2020").
#'   - The `timeMid` value (e.g., 2020).
#'
#' @return A ggplot object representing the population pyramid. The x-axis shows
#'   age groups, and the y-axis represents the relative population percentage. Males
#'   are represented with negative values, and females with positive values.
#'
#' @details The function retrieves data from a SQLite database (`population_data.sqlite`)
#'   included in the package. The database must include the required tables:
#'   - `locations`: Contains location information.
#'   - `times`: Contains temporal data.
#'   - `data_population_age5_and_sex`: Contains population data by age and sex.
#'
#'   If the specified location or year is not found, the function will stop with an error.
#'
#' @examples
#' # Example 1: Population pyramid for Brazil in 2020
#' pop_pyr("Brazil", 2000)
#'
#' # Example 2: Population pyramid for Brazil using ISO3 code and timeMid
#' pop_pyr("BRA", 2000)
#'
#' # Example 3: Population pyramid for a locationId
#' pop_pyr(76, "2000")
#'
#' @importFrom RSQLite dbConnect dbDisconnect dbGetQuery SQLite
#' @importFrom dplyr group_by summarise ungroup mutate if_else
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip labs scale_y_continuous scale_fill_manual scale_x_discrete theme_minimal
#' @importFrom scales label_percent
#'
#' @export
pop_pyr <- function(location, year) {

  # Consulta de dados populacionais
  data <- pop_pyr_data(location, year)

  # Criar o gráfico
  ggplot2::ggplot(data, ggplot2::aes(
    x = .data$age_f, y = .data$relative_population,
    group = .data$sex_f, fill = .data$sex_f
  )) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      name = 'Population (%)',
      labels = scales::label_percent(big.mark = '.', decimal.mark = ','),
      breaks = seq(-.16, .16, .02),limits = c(-.08,.08)
    ) +
    ggplot2::scale_fill_manual(
      name = 'Sex',
      values = c('#5C88DAFF','#CC0C00FF')
    ) +
    ggplot2::scale_x_discrete(name = 'Age Group') +
    ggplot2::theme_minimal()
}
