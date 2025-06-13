library('here')
library('dplyr')
library('ggplot2')
library('tibble')
library('ggsci')
library('readr')
library('stringr')
library('tidyr')

source('R/pop_pyr.R')
source('R/prepare_data.R')
#### Inicializações ----

#### LOCATIONS
query = "SELECT * FROM locations"
locations_all <- sql_basic(query)

query = "SELECT * FROM indicators"
indicators_all <- sql_basic(query)
write_csv(indicators_all,'data/indicators_all.csv') # ==> Excel e manual
indicator_traduzidos = readxl::read_excel('data/indicadores_Traduzidos.xlsx', sheet = 'Planilha1')
indicators_all = indicator_traduzidos %>%
  select(indicatorId, indicator, indicatorDisplayName)

query = "SELECT * FROM times"
times_all <- sql_basic(query)

ages_all <- sql_basic("SELECT * FROM ages")

metadata_data <- sql_basic("SELECT * FROM metadata_data")
sexs_all = sql_basic("SELECT * FROM sexs") %>%
  mutate(sex = case_when(
    sex == 'Male' ~ 'Masculino',
    sex == 'Female' ~ 'Feminino',
    sex == 'Both sexes' ~ 'Ambos os Sexos'
  ))

indicator_list <- paste0("'", unique(metadata_data$indicatorId), "'", collapse = ", ")
query = paste0("SELECT DISTINCT indicatorId, indicator, indicatorDisplayName FROM indicators WHERE indicatorId IN (", indicator_list ,")")
indicators_metadata <- sql_basic(query)

variants_all <- sql_basic("SELECT * FROM variants")


#### Figura 1 - Prehistoria ----

library('jsonlite')

readr::write_rds(read.csv("https://ourworldindata.org/grapher/population.csv?v=1&csvType=full&useColumnShortNames=true"),
                 file='ourworldindata_historic_population.rds')

query = "SELECT d.value, 
    i.indicator AS indicator, 
    l.location AS location,
    CAST(t.timeMid AS INT) AS year
    FROM data d
    JOIN indicators i ON d.indicatorId = i.indicatorId
    JOIN locations l ON d.locationID = l.locationID
    JOIN times t ON d.timeId = t.timeId
    WHERE d.locationID = 900 
    AND d.indicatorId IN (49) 
    AND d.sexId = 3 
    AND d.variantId = 4"

popWPP_df = sql_basic(query)%>%
  tidyr::pivot_wider(
    names_from = indicator, 
    values_from = value
  )

figura1_df = readr::read_rds('ourworldindata_historic_population.rds')%>%
  filter(Entity == "World" & Year < 1950)%>%
  select(Year, population_historical)%>%
  rename(Population = population_historical)%>%
  mutate(Estimate = 'Historical')

figura1_df = figura1_df %>%
  bind_rows(popWPP_df%>%
              rename(Year=year, Population = `Total population by sex`)%>%
              select(Year, Population)%>%
              mutate(Estimate = 'WPP'))

world_population_e90 <- tibble::tibble(
  Year = c(2000, 2050, 2100, 2150, 2200, 2250, 2300),
  TFR_0.75 = c(6.0, 6.8, 6.2, 4.5, 3.0, 1.5, 0.5),
  TFR_1.00 = c(6.0, 7.5, 7.0, 5.8, 4.0, 2.0, 0.8),
  TFR_1.25 = c(6.0, 8.0, 8.0, 6.8, 5.0, 3.0, 1.5),
  TFR_1.50 = c(6.0, 8.5, 9.0, 8.2, 7.0, 5.0, 3.5),
  TFR_1.75 = c(6.0, 9.0, 10.0, 9.5, 9.0, 7.5, 6.0),
  TFR_2.00 = c(6.0, 9.5, 11.0, 11.0, 11.5, 12.0, 12.5),
  TFR_2.25 = c(6.0, 10.0, 12.0, 12.5, 13.0, 13.5, 14.0),
  TFR_2.50 = c(6.0, 10.5, 13.0, 14.0, 15.0, 16.0, 17.0)) %>%
  pivot_longer(cols = -Year, names_to = "Estimate", values_to = "Population") %>%
  mutate( Population = Population*10^9)

figura1_df = figura1_df %>%
  bind_rows(world_population_e90)
  # bind_rows(tibble::tibble(
  #   Year = rep(c(2100, 2150, 2200, 2250, 2300),2),
  #   Population = c(10.0, 9.5, 9.0, 7.5, 6.0,13.0, 14.0, 15.0, 16.0, 17.0)*10^9,
  #   Estimate = c(rep('TFR 1.75',5),rep('TFR 2.50',5))))
  


figura1_years_historical = c(seq(-10000,0,1000),
                     seq(100,1700,100),
                     seq(1710,1950,10),
                     seq(1951,2100,1),
                     seq(2150,2300,50))

figura1_years_hightligthed <- c(-10000, -5000, -3000, -1000, 0, 500, 1000, 1500, 1800, 1900, 2000, 2100 , 2300)


figura1_rescale_years <- function(ano) {
  case_when(
    ano <= -3000 ~ (ano + 10000) / 500,        # New Stone Age (compressed)
    ano <= -1000 ~ ((ano + 3000) / 200) + 14,  # Bronze Age
    ano <= 500 ~ ((ano + 1000) / 100) + 24,    # Iron Age
    ano <= 1500 ~ ((ano - 500) / 50) + 34,     # Middle Ages
    ano <= 2000 ~ ((ano - 1500) / 10) + 54,    # Modern Ages
    ano > 2000 ~ ((ano - 2000) / 2) + 104      # Future (expanded)
  )
}

figura1_df = figura1_df %>%
  mutate(Year_historical = figura1_rescale_years(Year))

figura1_rescale_years = figura1_df %>%
  filter(Year %in% figura1_years_hightligthed) %>%
  mutate(Year_label = format(Year,big.mark = ".", decimal.mark = ",", scientific = FALSE, digits = 1, justify = 'left'),
         Year_label = str_pad(Year_label, width = max(nchar(Year_label)), side = "right"))


gg_ls = list()

gg_ls[['generico']] = ggplot() +
  geom_area(data = figura1_df %>%
              filter(Year < 2100), 
            aes(x = Year_historical, y = Population), linewidth = .5, fill = "#00B5E2FF",color='grey30')+
  geom_line(data = figura1_df %>%
              filter(Year >= 2100),
            aes(x = Year_historical, y = Population, group = Estimate, color = Estimate), linewidth = .5) +
  geom_label(data = figura1_rescale_years,
             aes(label = Year_label, x=Year_historical),
             y = max(figura1_df$Population) * 1.05, angle = 90, hjust = 1, size = 3, color = "grey40") +
  scale_x_continuous(
    name = "Períodos Históricos",
    breaks = c(0, 14, 24, 34, 54, 104),
    labels = c("New Stone Age", "Bronze Age", "Iron Age", "Middle Ages", "Modern Ages", "Future"))+
  hrbrthemes::theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1),
        panel.grid.minor.x = element_blank())

gg_ls[['decimal']] = gg_ls[['generico']] +
  scale_y_continuous(name = "Population (bi)",
                     labels = scales::label_number(scale = 1e-9, accuracy = 0.1, big.mark = ','))+
  labs(subtitle="Linear Scale")

gg_ls[['log10']] = gg_ls[['generico']] +
  scale_y_log10(name = "Population (bi)",labels = scales::label_log(base = 10,
                                                                    digits = 2),
                breaks = 10^seq(6,11,2))+
  coord_cartesian(ylim = c(1e6, 1e11))+
  labs(subtitle="Logarithmic Scale")

library('patchwork')
gg = gg_ls[['decimal']]/gg_ls[['log10']] +
  plot_layout(ncol = 1, guides = 'collect') & theme(legend.position = 'bottom')

ggsave('figures/figura1_decimal.jpeg', device = 'jpeg', plot = gg_ls[['decimal']], width = 30, height = 15, dpi = 300, units = 'cm')
ggsave('figures/figura1_log10.jpeg', device = 'jpeg', plot = gg_ls[['log10']], width = 30, height = 15, dpi = 300, units = 'cm')

#### Figura 2 População total em ----
# Regiões de menor desenvolvimento socioeconômico,
# Regiões em desenvolvimento socioeconômico, e
# Regiões de alto desenvolvimento socioeconômico
locations <- sql_basic("SELECT * FROM locations WHERE locationId IN (901, 902, 941, 934)")
indicators = sql_basic("SELECT * FROM indicators WHERE indicatorId IN (49)")
years <- seq(1950, 2100)
query = paste0("SELECT * FROM times WHERE timeLabel IN ('",
               paste(years, collapse = "', '"), "')")
times <- sql_basic(query)
main_insert(locations, times, indicators)

query = "SELECT d.value, 
    i.indicator AS indicator, 
    l.location AS location,
    l.locationId AS locationId,
    CAST(t.timeMid AS INT) AS year
    FROM data d
    JOIN indicators i ON d.indicatorId = i.indicatorId
    JOIN locations l ON d.locationID = l.locationID
    JOIN times t ON d.timeId = t.timeId
    WHERE d.locationId IN (901, 934, 941)
    AND d.indicatorId IN (49) 
    AND d.sexId = 3 
    AND d.variantId = 4"

figura2_df = sql_basic(query) %>%
  mutate(location_f = factor(locationId,levels = c(941, 934, 901),
                             labels = c('Regiões menos dessenvolvidas',
                                        'Regiões em desenvolvimento',
                                        'Regiões desenvolvidas'),
                             # labels = c('Least developed regions','Developing regions','Developed regions'), 
                             ordered = TRUE))%>%
  arrange(desc(location_f))


gg = ggplot(data = figura2_df) +
  geom_area(aes(x = year,y = value, fill = location_f)) +
  scale_x_continuous(name = "Ano", breaks=seq(1950,2100,25))+
  scale_y_continuous(
    name = "Population (bi)",
    breaks = seq(0,10^10,10^9),
    labels = scales::label_number(scale = 1e-9, accuracy = 1)) +
  scale_fill_startrek(name = NULL)+
  hrbrthemes::theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        panel.grid.minor.x = element_blank())

ggsave('figures/figura2.jpeg', device = 'jpeg', plot = gg, width = 24, height = 24*0.5, dpi = 300, units = 'cm')

#### Figura 3 Crescimento Brazil ----
locations = sql_basic("SELECT * FROM locations WHERE location = 'Brazil'")
indicators = sql_basic("SELECT * FROM indicators WHERE indicatorId IN (49, 51)")
years <- seq(1950, 2100)
query = paste0("SELECT * FROM times WHERE timeLabel IN ('",
               paste(years, collapse = "', '"), "')")
times <- sql_basic(query)
main_insert(locations, times, indicators)

rate_scale <- function(primary, secondary, 
                       round = c(min = "floor", max = "ceiling"),
                       scale = c(primary = 1, secondary = 1)) {
  if (all(is.na(primary)) || all(is.na(secondary))) {
    stop("Erro: As colunas primary ou secondary contêm apenas valores NA.")
  }
  primary_scaled <- primary / scale["primary"]
  secondary_scaled <- secondary / scale["secondary"]
  min_primary <- match.fun(round["min"])(min(primary_scaled, na.rm = TRUE)) * scale["primary"]
  max_primary <- match.fun(round["max"])(max(primary_scaled, na.rm = TRUE)) * scale["primary"]
  min_secondary <- match.fun(round["min"])(min(secondary_scaled, na.rm = TRUE)) * scale["secondary"]
  max_secondary <- match.fun(round["max"])(max(secondary_scaled, na.rm = TRUE)) * scale["secondary"]
  scale_factor <- (max_primary - min_primary) / (max_secondary - min_secondary)
  list(
    scaled = (secondary_scaled - min_secondary) * scale_factor + min_primary,
    scale_factor = scale_factor,
    min_primary = min_primary,
    max_primary = max_primary,
    min_secondary = min_secondary,
    max_secondary = max_secondary
  )
}

query = "SELECT d.value, 
    i.indicator AS indicator, 
    l.location AS location,
    CAST(t.timeMid AS INT) AS year
    FROM data d
    JOIN indicators i ON d.indicatorId = i.indicatorId
    JOIN locations l ON d.locationID = l.locationID
    JOIN times t ON d.timeId = t.timeId
    WHERE d.locationId = 76 
    AND d.indicatorId IN (49, 51) 
    AND d.sexId = 3 
    AND d.variantId = 4"

figura3_df = sql_basic(query)%>%
  tidyr::pivot_wider(
    names_from = indicator, 
    values_from = value
  )%>%
  arrange(year)
rate_scaled = rate_scale(figura3_df$`Total population by sex`,figura3_df$`Rate of population change`, 
                         scale = c(secondary=1, primary=1e6))
figura3_df$rate_scaled = rate_scaled$scaled

gg = ggplot(data = figura3_df) +
  geom_line(aes(x = year,y = `Total population by sex`, colour = "População"), linewidth = 2) +
  geom_line(aes(x = year,y = rate_scaled, linetype = "Taxa de Crescimento"), 
            linewidth = 2, color = "#FFCD00FF") +
  scale_x_continuous(name = "Ano", breaks=seq(1950,2100,10))+
  scale_y_continuous(
    name = "População (mi)",
    labels = scales::label_number(scale = 1e-6, accuracy = 0.1), # Ajusta para bilhões
    sec.axis = sec_axis(transform = ~ (. - rate_scaled$min_primary) / rate_scaled$scale_factor + rate_scaled$min_secondary,
                        name = "Taxa de crescimento populacional (%)",
                        labels=scales::label_number(accuracy = 0.1))) +
  scale_colour_manual(
    name = NULL, # Nome da legenda
    values = c("População"="#00B5E2FF"), # Cores
    labels = c("População Total") # Rótulos da legenda
  ) +
  scale_linetype_manual(
    name = "Legenda", # Nome da legenda
    values = c("Taxa de Crescimento" = "solid"),
    labels = c("Taxa de Crescimento")
  ) +
  hrbrthemes::theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        panel.grid.minor.x = element_blank())

ggsave('figures/figura3.jpeg', device = 'jpeg', plot = gg, width = 24, height = 24*0.5, dpi = 300, units = 'cm')


#### Figura 4 Pirâmide Mundo ----

query = "SELECT * FROM locations WHERE locationID = 900"
locations <- sql_basic(query)
query = "SELECT * FROM indicators WHERE indicatorId IN (49,46)"
indicators <- sql_basic(query)
years <- c(1950,2000,2025,2050)
query = paste0("SELECT * FROM times WHERE timeLabel IN ('",
               paste(years, collapse = "', '"), "')")
times <- sql_basic(query)
main_insert(locations, times, indicators)

query = "SELECT d.value, 
    i.indicator AS indicator, 
    l.location AS location,
    a.ageStart AS age,
    s.sex AS sex,
    CAST(t.timeMid AS INT) AS year
    FROM data d
    JOIN indicators i ON d.indicatorId = i.indicatorId
    JOIN ages a ON d.ageId = a.ageId
    JOIN sexs s ON d.sexId = s.sexId
    JOIN locations l ON d.locationID = l.locationID
    JOIN times t ON d.timeId = t.timeId
    WHERE d.locationId = 900
    AND d.timeId IN (1,51,101,151)
    AND d.indicatorId IN (46) 
    AND d.sexId IN (1,2)  
    AND d.variantId = 4"

figura4_df = sql_basic(query) %>%
  mutate(sexage_population = if_else(sex == 'Male', -value, value))%>%
  group_by(year,location)%>%
  mutate(total_population = sum(value))%>%
  ungroup()%>%
  mutate(relative_population = sexage_population/total_population,
         age_f = factor(age, levels = seq(0,100,5),
                        labels = c(paste0(seq(0,95,5),"-",seq(4,99,5)),'100+'),
                        ordered = TRUE)
         )

gg = ggplot(figura4_df, 
       aes(x = age_f, y = sexage_population, 
           group = sex, fill = sex)) +
  facet_wrap(~year,scales = "fixed") +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(
    name = 'População (mi)',
    labels = function(x) {
      scales::label_number(
        big.mark = '.', decimal.mark = ',', scale = 10^-6
      )(abs(x))
    },
    breaks = seq(-10^9,10^9,10^8)) +
  scale_fill_manual(
    name = 'Sexo',
    values = c('#5C88DAFF','#CC0C00FF'),
    labels = c("Masculino","Feminino")
  ) +
  scale_x_discrete(name = 'Grupo de Idade') +
  theme_minimal()+
  theme(
    axis.text.x = element_text(),
    axis.ticks.x = element_line()
  )+
  geom_label(data = figura4_df %>%
               distinct(year, total_population),
             aes(x = Inf, y = Inf, 
                 label = paste0("População=", 
                                scales::label_number(big.mark = '.', 
                                                     decimal.mark = ',',
                                                     scale = 10^-9)(total_population), " bi")),
             hjust = 1.0, vjust = 1.0, size = 3, color = "black", inherit.aes = FALSE)

ggsave('figures/figura4.jpeg', device = 'jpeg', plot = gg, width = 24, height = 18, dpi = 300, units = 'cm')


figura4_df %>%
      filter(year == 2000 & age_f >= '50-54')%>%
      mutate(sexage_population = abs(sexage_population))%>%
      summarise(mais50 = sum(sexage_population),
               total_population = first(total_population))

figura4_df %>%
  filter(year == 2025 & age_f >= '75-79')%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais75 = sum(sexage_population),
            total_population = first(total_population))
        
figura4_df %>%
  filter(year == 1950)%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais50 = sum(sexage_population),
            total_population = first(total_population))

figura4_df %>%
  filter(year == 2025)%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais75 = sum(sexage_population),
            total_population = first(total_population))

figura4_df %>%
  filter(year == 2050 & age_f >= '25-29')%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais75 = sum(sexage_population),
            total_population = first(total_population))




#### Figura 5 Pirâmide Brasil ----

query = "SELECT * FROM locations WHERE locationID = 76"
locations <- sql_basic(query)
query = "SELECT * FROM indicators WHERE indicatorId IN (49,46)"
indicators <- sql_basic(query)
years <- c(1950,2000,2025,2050)
query = paste0("SELECT * FROM times WHERE timeLabel IN ('",
               paste(years, collapse = "', '"), "')")
times <- sql_basic(query)
main_insert(locations, times, indicators)

query = "SELECT d.value, 
    i.indicator AS indicator, 
    l.location AS location,
    a.ageStart AS age,
    s.sex AS sex,
    CAST(t.timeMid AS INT) AS year
    FROM data d
    JOIN indicators i ON d.indicatorId = i.indicatorId
    JOIN ages a ON d.ageId = a.ageId
    JOIN sexs s ON d.sexId = s.sexId
    JOIN locations l ON d.locationID = l.locationID
    JOIN times t ON d.timeId = t.timeId
    WHERE d.locationId = 76
    AND d.timeId IN (1,51,101,151)
    AND d.indicatorId IN (46) 
    AND d.sexId IN (1,2)  
    AND d.variantId = 4"

figura5_df = sql_basic(query) %>%
  mutate(sexage_population = if_else(sex == 'Male', -value, value))%>%
  group_by(year,location)%>%
  mutate(total_population = sum(value))%>%
  ungroup()%>%
  mutate(relative_population = sexage_population/total_population,
         age_f = factor(age, levels = seq(0,100,5),
                        labels = c(paste0(seq(0,95,5),"-",seq(4,99,5)),'100+'),
                        ordered = TRUE)
  )

gg = ggplot(figura5_df, 
            aes(x = age_f, y = sexage_population, 
                group = sex, fill = sex)) +
  facet_wrap(~year,scales = "fixed") +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(
    name = 'População (mi)',
    labels = function(x) {
      scales::label_number(
        big.mark = '.', decimal.mark = ',', scale = 10^-6
      )(abs(x))
    },
    breaks = seq(-10^7,10^7,2*10^6)) +
  scale_fill_manual(
    name = 'Sexo',
    values = c('#5C88DAFF','#CC0C00FF'),
    labels = c("Masculino","Feminino")
  ) +
  scale_x_discrete(name = 'Grupo de Idade') +
  theme_minimal()+
  theme(
    axis.text.x = element_text(),
    axis.ticks.x = element_line()
  )+
  geom_label(data = figura5_df %>%
               distinct(year, total_population),
             aes(x = Inf, y = Inf, 
                 label = paste0("População=", 
                                scales::label_number(big.mark = '.', 
                                                     decimal.mark = ',',
                                                     scale = 10^-6)(total_population), " mi")),
             hjust = 1.0, vjust = 1.0, size = 3, color = "black", inherit.aes = FALSE)

ggsave('figures/figura5.jpeg', device = 'jpeg', plot = gg, width = 24, height = 18, dpi = 300, units = 'cm')


figura4_df %>%
  filter(year == 2000 & age_f >= '50-54')%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais50 = sum(sexage_population),
            total_population = first(total_population))

figura4_df %>%
  filter(year == 2025 & age_f >= '75-79')%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais75 = sum(sexage_population),
            total_population = first(total_population))

figura4_df %>%
  filter(year == 1950)%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais50 = sum(sexage_population),
            total_population = first(total_population))

figura4_df %>%
  filter(year == 2025)%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais75 = sum(sexage_population),
            total_population = first(total_population))

figura4_df %>%
  filter(year == 2050 & age_f >= '25-29')%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais75 = sum(sexage_population),
            total_population = first(total_population))



#### Figura 6 Pirâmides Korea ----

query = "SELECT * FROM indicators WHERE indicatorId IN (49,46)"
indicators <- sql_basic(query)

query = "SELECT * FROM locations WHERE location = 'Republic of Korea'"
locations <- sql_basic(query)

years <- c(1950,2000,2025,2050,2075, 2100)
query = paste0("SELECT * FROM times WHERE timeLabel IN ('",
               paste(years, collapse = "', '"), "')")
times <- sql_basic(query)
main_insert(locations, times, indicators)


query = "SELECT d.value, 
    i.indicator AS indicator, 
    l.location AS location,
    a.ageStart AS age,
    s.sex AS sex,
    CAST(t.timeMid AS INT) AS year
    FROM data d
    JOIN indicators i ON d.indicatorId = i.indicatorId
    JOIN ages a ON d.ageId = a.ageId
    JOIN sexs s ON d.sexId = s.sexId
    JOIN locations l ON d.locationID = l.locationID
    JOIN times t ON d.timeId = t.timeId
    WHERE d.locationId = 410
    AND d.timeId IN (1,51,76,101,126,151)
    AND d.indicatorId IN (46) 
    AND d.sexId IN (1,2)  
    AND d.variantId = 4"

figura6_df = sql_basic(query) %>%
  mutate(sexage_population = if_else(sex == 'Male', -value, value))%>%
  group_by(year,location)%>%
  mutate(total_population = sum(value))%>%
  ungroup()%>%
  mutate(relative_population = sexage_population/total_population,
         age_f = factor(age, levels = seq(0,100,5),
                        labels = c(paste0(seq(0,95,5),"-",seq(4,99,5)),'100+'),
                        ordered = TRUE)
  )

gg = ggplot(figura6_df, 
            aes(x = age_f, y = sexage_population, 
                group = sex, fill = sex)) +
  facet_wrap(~year,scales = "fixed") +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(
    name = 'População (mi)',
    labels = function(x) {
      scales::label_number(
        big.mark = '.', decimal.mark = ',', scale = 10^-6
      )(abs(x))
    },
    breaks = seq(-10^7,10^7,1*10^6)) +
  scale_fill_manual(
    name = 'Sexo',
    values = c('#5C88DAFF','#CC0C00FF'),
    labels = c("Masculino","Feminino")
  ) +
  scale_x_discrete(name = 'Grupo de Idade') +
  theme_minimal()+
  theme(
    axis.text.x = element_text(),
    axis.ticks.x = element_line()
  )+
  geom_label(data = figura6_df %>%
               distinct(year, total_population),
             aes(x = Inf, y = Inf, 
                 label = paste0("População=", 
                                scales::label_number(big.mark = '.', 
                                                     decimal.mark = ',',
                                                     scale = 10^-6)(total_population), " mi")),
             hjust = 1.0, vjust = 1.0, size = 3, color = "black", inherit.aes = FALSE)

ggsave('figures/figura6.jpeg', device = 'jpeg', plot = gg, width = 24, height = 18, dpi = 300, units = 'cm')


figura6_df %>%
  filter(year == 2000 & age_f >= '50-54')%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais50 = sum(sexage_population),
            total_population = first(total_population))

figura6_df %>%
  filter(year == 2025 & age_f >= '75-79')%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais75 = sum(sexage_population),
            total_population = first(total_population))

figura6_df %>%
  filter(year == 1950)%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais50 = sum(sexage_population),
            total_population = first(total_population))

figura6_df %>%
  filter(year == 2025)%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais75 = sum(sexage_population),
            total_population = first(total_population))

figura6_df %>%
  filter(year == 2050 & age_f >= '25-29')%>%
  mutate(sexage_population = abs(sexage_population))%>%
  summarise(mais75 = sum(sexage_population),
            total_population = first(total_population))


summary(figura6_df %>%
          filter(year == 1950)%>%
          select(total_population))


#### Figura 7 Fecunidade Brasil, Mundo e Paises Desenvolvidos ----
locations <- sql_basic("SELECT * FROM locations WHERE locationId IN (76, 900, 901)")
indicators = sql_basic("SELECT * FROM indicators WHERE indicatorId IN (51, 19)")
years <- seq(1950, 2100)
query = paste0("SELECT * FROM times WHERE timeLabel IN ('",
               paste(years, collapse = "', '"), "')")
times <- sql_basic(query)
main_insert(locations, times, indicators)

query = "SELECT d.value, 
    i.indicator AS indicator, 
    l.location AS location,
    CAST(t.timeMid AS INT) AS year
    FROM data d
    JOIN indicators i ON d.indicatorId = i.indicatorId
    JOIN locations l ON d.locationID = l.locationID
    JOIN times t ON d.timeId = t.timeId
    WHERE d.locationId IN (76,900,901) 
    AND d.indicatorId IN (51, 19) 
    AND d.sexId = 3 
    AND d.variantId = 4"

figura7_df = sql_basic(query)%>%
  tidyr::pivot_wider(
    names_from = indicator, 
    values_from = value
  )%>%
  arrange(year)
rate_scaled = rate_scale(primary=figura7_df$`Rate of population change`,secondary=figura7_df$`Total fertility rate`, 
                         scale = c(secondary=1, primary=1))
figura7_df$rate_scaled = rate_scaled$scaled


custom_labeller <- function(labels) {
  letters_seq <- paste0("(",letters[seq_along(labels)], "). ")  # Gera "a) ", "b) ", "c) "
  setNames(paste0(letters_seq, labels), labels)  # Concatena com o título original
}

gg = ggplot(data = figura7_df) +
  facet_wrap(~location, labeller = labeller(location = custom_labeller)) +
  geom_line(aes(x = year,y = `Rate of population change`, colour = "Taxa de Crescimento"), linewidth = 1) +
  geom_line(aes(x = year,y = rate_scaled, linetype = "Taxa de Fecunidade Total"), 
            linewidth = 1, color = "#FFCD00FF") +
  scale_x_continuous(name = "Ano", breaks=seq(1950,2100,25))+
  scale_y_continuous(
    name = "Taxa de Crescimento (%)",
    labels = scales::label_number(scale = 1, accuracy = 0.1),
    sec.axis = sec_axis(transform = ~ (. - rate_scaled$min_primary) / rate_scaled$scale_factor + rate_scaled$min_secondary,
                        name = "TFR",
                        labels=scales::label_number(accuracy = 0.1))) +
  scale_colour_manual(
    name = NULL, # Nome da legenda
    values = c("Taxa de Crescimento"="#00B5E2FF"), # Cores
    labels = c("Taxa de Crescimento") # Rótulos da legenda
  ) +
  scale_linetype_manual(
    name = "Legenda", # Nome da legenda
    values = c("Taxa de Fecunidade Total" = "solid"),
    labels = c("Taxa de Fecunidade Total")
  ) +
  hrbrthemes::theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom")

ggsave('figures/figura7.jpeg', device = 'jpeg', plot = gg, width = 24, height = 24*0.5, dpi = 300, units = 'cm')

#### Figura 8 Quadro criado no Excel (Tables.xlsx) ----

#### Figura 9 Razào de Dependencias em ----
# Regiões de menor desenvolvimento socioeconômico,
# Regiões em desenvolvimento socioeconômico, e
# Regiões de alto desenvolvimento socioeconômico
locations <- sql_basic("SELECT * FROM locations WHERE locationId IN (901, 941, 934)")
indicators = sql_basic("SELECT * FROM indicators WHERE indicatorId IN (83, 84, 85, 86)")
years <- seq(1950, 2100)
query = paste0("SELECT * FROM times WHERE timeLabel IN ('",
               paste(years, collapse = "', '"), "')")
times <- sql_basic(query)
main_insert(locations, times, indicators)

query = "SELECT 
    d.value,
    i.indicator AS indicator,
    d.indicatorId AS indicatorId,
    l.location AS location,
    l.locationId AS locationId,
    a.ageLabel AS age,
    d.ageId as ageId,
    d.variantId AS variant,
    CAST(t.timeMid AS INT) AS year
    FROM data d
    JOIN indicators i ON d.indicatorId = i.indicatorId
    JOIN locations l ON d.locationID = l.locationID
    JOIN times t ON d.timeId = t.timeId
    JOIN ages a ON d.ageId = a.ageId
    WHERE d.locationId IN (901, 934, 941)
    AND d.indicatorId IN (83,84,86) 
    AND d.sexId = 3 
    AND d.variantId = 4
    AND d.ageId IN (1000,1005,1015)"

#data = sql_basic("SELECT * from data WHERE ageId = 1015")

figura9_df = sql_basic(query) %>%
  mutate(location_f = factor(locationId,levels = c(941, 934, 901),
                             labels = c('Regiões menos desenvolvidas',
                                        'Regiões em desenvolvimento',
                                        'Regiões desenvolvidas'),
                             #labels = c('Least developed regions','Developing regions','Developed regions'), 
                             ordered = TRUE),
         indicator_f = factor(indicatorId, levels = c(86, 84, 83),
                              labels = c('Razão de Depedência Total',
                                         'Razão de Dependência de Idosos',
                                         'Razão de Dependência de Jovens')))%>%
  arrange(desc(location_f))


gg = ggplot(data = figura9_df) +
  facet_wrap(~location_f, labeller = labeller(location_f = custom_labeller)) +
  geom_area(data = figura9_df%>%
              filter(ageId %in% c(1000,1005)), aes(x = year,y = value, fill = indicator_f)) +
  geom_line(data = figura11_df%>%
              filter(ageId %in% c(1015)), aes(x = year,y = value), linewidth = 1)+
  scale_x_continuous(name = "Ano", breaks=seq(1950,2100,25))+
  scale_y_continuous(
    name = "Razão de Depedência",
    breaks = seq(0,100,25),
    labels = scales::label_number(scale = 1, accuracy = 0.1)) +
  scale_fill_startrek(name = NULL)+
  hrbrthemes::theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom")

ggsave('figures/figura9.jpeg', device = 'jpeg', plot = gg, width = 24, height = 24*0.5, dpi = 300, units = 'cm')

#### Figura 10 Razào de Dependencias em ----
# Regiões de menor desenvolvimento socioeconômico,
# Regiões em desenvolvimento socioeconômico, e
# Regiões de alto desenvolvimento socioeconômico
locations <- sql_basic("SELECT * FROM locations WHERE locationId IN (76)")
indicators = sql_basic("SELECT * FROM indicators WHERE indicatorId IN (83, 84, 85, 86)")
years <- seq(1950, 2100)
query = paste0("SELECT * FROM times WHERE timeLabel IN ('",
               paste(years, collapse = "', '"), "')")
times <- sql_basic(query)
main_insert(locations, times, indicators)

query = "SELECT 
    d.value,
    i.indicator AS indicator,
    d.indicatorId AS indicatorId,
    l.location AS location,
    l.locationId AS locationId,
    a.ageLabel AS age,
    d.ageId as ageId,
    d.variantId AS variant,
    CAST(t.timeMid AS INT) AS year
    FROM data d
    JOIN indicators i ON d.indicatorId = i.indicatorId
    JOIN locations l ON d.locationID = l.locationID
    JOIN times t ON d.timeId = t.timeId
    JOIN ages a ON d.ageId = a.ageId
    WHERE d.locationId IN (76)
    AND d.indicatorId IN (83,84,86) 
    AND d.sexId = 3 
    AND d.variantId = 4
    AND d.ageId IN (1000,1005,1015)"

#data = sql_basic("SELECT * from data WHERE ageId = 1015")

figura10_df = sql_basic(query) %>%
  mutate(indicator_f = factor(indicatorId, levels = c(86, 84, 83),
                              labels = c('Razão de Depedência Total',
                                         'Razão de Dependência de Idosos',
                                         'Razão de Dependência de Jovens')))


gg = ggplot() +
  geom_area(data = figura10_df%>%
              filter(ageId %in% c(1000,1005)), aes(x = year,y = value, fill = indicator_f)) +
  geom_line(data = figura10_df%>%
              filter(ageId %in% c(1015)), aes(x = year,y = value), linewidth = 1)+
  scale_x_continuous(name = "Ano", breaks=seq(1950,2100,10))+
  scale_y_continuous(
    name = "Razão de Dependência",
    breaks = seq(0,100,10),
    labels = scales::label_number(scale = 1, accuracy = 0.1)) +
  scale_fill_startrek(name = NULL)+
  hrbrthemes::theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom')

ggsave('figures/figura10.jpeg', device = 'jpeg', plot = gg, width = 24, height = 24*0.5, dpi = 300, units = 'cm')


#### Figura 11 Grandes Grupos de Idade Censos Brasileiros ----

library('jsonlite')
library('httr')

browseURL("https://apisidra.ibge.gov.br/")

monta_url_sidra <- function(...) {
  args <- list(...)
  encode_param <- function(x) {
    if (is.list(x)) x <- unlist(x)
    paste(x, collapse = ",")
  }
  path_parts <- unlist(
    lapply(names(args), function(k) {
      v <- encode_param(args[[k]])
      paste0(k, "/", v)
    })
  )
  base_url <- "https://apisidra.ibge.gov.br/values"
  path <- paste0("/", paste(path_parts, collapse = "/"))
  url_completa <- paste0(base_url, path,"/f/u/h/y")
  return(url_completa)
}

consulta_sidra <- function(url) {
  res <- httr::GET(url)
  
  if (httr::status_code(res) != 200) {
    erro_texto <- httr::content(res, as = "text", encoding = "UTF-8")
    warning("Erro na requisição (", httr::status_code(res), "): ", erro_texto)
    return(NULL)
  }
  
  conteudo <- httr::content(res, as = "text", encoding = "UTF-8")
  dados <- jsonlite::fromJSON(conteudo)
  df <- as.data.frame(dados)
  return(df)
}

parse_url_sidra <- function(url) {
  path <- sub("^https://apisidra\\.ibge\\.gov\\.br/values/", "", url)
  path <- sub("/f/u/h/y$", "", path)
  parts <- unlist(strsplit(path, "/"))
  keys <- parts[seq(1, length(parts), by = 2)]
  values <- parts[seq(2, length(parts), by = 2)]
  param_list <- setNames(lapply(values, function(x) {
    if (grepl(",", x)) {
      as.numeric(unlist(strsplit(x, ",")))
    } else if (x == "all") {
      x
    } else {
      as.numeric(x)
    }
  }), keys)
  return(param_list)
}

#browseURL("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=1552")
sidra_path <- monta_url_sidra(
  t = 1552,
  n1 = 'all',
  v = 'allxp',
  p = 'last%201',
  c1 = 0,
  c2 = 0,
  c286 = 0,
  c287 = c(6653,93095,93096,93097,93098,93099,93100))
figura11_1552_raw = consulta_sidra(sidra_path)
temp_1 = figura11_1552_raw[1,]%>%
  mutate(across(everything(), ~ snakecase::to_snake_case(.x)))
names(figura11_1552_raw) = temp_1
figura11_1152 = figura11_1552_raw[-1,c(4,8,12)]

#browseURL("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=9514")
sidra_path <- monta_url_sidra(
  t = 9514,
  P = 2022,
  v = 93,
  c2 = 6794,
  C286 = 113635,
  C287 = c(93095,93096,93097,93098,49108,49109,60040,60041,6653),
  N1 = 1
)

figura11_9514_raw = consulta_sidra(sidra_path)
temp_1 = figura11_9514_raw[1,]%>%
  mutate(across(everything(), ~ snakecase::to_snake_case(.x)))
names(figura11_9514_raw) = temp_1
figura11_9514 = figura11_9514_raw[-1,c(4,5,9)]


figura11_df = figura11_9514 %>%
  bind_rows(figura11_1152)%>%
  mutate(valor = as.numeric(valor),
         ano = factor(ano, levels = c('2010', '2022'), ordered = TRUE),
         idade = if_else(
           idade %in% c("85 a 89 anos","80 a 84 anos","90 a 94 anos","95 a 99 anos",
                        "80 a 89 anos","100 anos ou mais","90 a 99 anos"),
           "80 anos ou mais",idade))

figura11_df$idade_f = factor(figura11_df$idade, levels = unique(figura11_df$idade), ordered = TRUE)
figura11_df = figura11_df %>%
  group_by(ano, idade, idade_f) %>%
  summarise(valor = sum(valor), .groups = 'drop')

n_anos <- length(levels(figura13_df$ano))
pos_ano <- match("2010", levels(figura13_df$ano))
offset <- (pos_ano - 0.5) / n_anos - 0.5

 
figura13_diff_df <- figura13_df %>%
  group_by(ano,idade,idade_f) %>%
  summarise(valor = sum(valor), .groups = 'drop')%>%
  arrange(idade_f,ano) %>%
  mutate(diff = 
           case_when(
             ano %in% c(2022) ~ valor - lag(valor),
             .default = NA),
         diff_relativa = 100*round(diff / lag(valor),digits=3))%>%
  filter(!is.na(diff))%>%
  mutate(
    x_adjusted = as.numeric(idade_f) + offset * 0.9
  )

gg = ggplot(data=figura13_df) +
  geom_bar(aes(fill = ano, y = valor, x = idade_f),stat = "identity", 
           position = position_dodge(width = dodge_width)) +
  geom_segment(
    data = figura13_diff_df,
    aes(
      x = x_adjusted,
      xend = x_adjusted,
      y = valor - diff,
      yend = valor),
    color = "black",
    linewidth = 1,
    arrow = arrow(length = unit(0.1, "inches"), ends = "last", type = "closed")) +
  geom_label(data = figura13_diff_df, 
             aes(x = x_adjusted, y = valor - diff/2, 
                 label = paste0("+", diff_relativa, "%"), group = ano), 
             color = "black", size = 4, hjust = 0.5) +
  scale_x_discrete(name = "Grupos de Idades")+
  scale_y_continuous(
    name = "População (mi)",
    labels = scales::label_number(scale = 1/1e6, accuracy = 0.1))+
  scale_fill_startrek(name = "Anos") +
  hrbrthemes::theme_ipsum() +
  theme(axis.text.x = element_text(angle = 0, vjust = .5))

ggsave('figures/figura11.jpeg', device = 'jpeg', plot = gg, width = 24, height = 24*0.5, dpi = 300, units = 'cm')


#### Figura 12 Potencial de Suporte ----
locations <- sql_basic("SELECT * FROM locations WHERE 
      location IN ('France','Italy','Cuba','Developed regions','Brazil', 'Republic of Korea')")
indicators = sql_basic("SELECT * FROM indicators WHERE indicatorId IN (85)")
years <- seq(1950, 2100)
query = paste0("SELECT * FROM times WHERE timeLabel IN ('",
               paste(years, collapse = "', '"), "')")
times <- sql_basic(query)
main_insert(locations, times, indicators)

query = "SELECT 
    d.value,
    i.indicator AS indicator,
    l.location AS location,
    a.ageLabel AS age,
    a.ageStart AS ageStart,
    a.ageEnd AS ageEnd,
    CAST(t.timeMid AS INT) AS year
    FROM data d
    JOIN indicators i ON d.indicatorId = i.indicatorId
    JOIN locations l ON d.locationID = l.locationID
    JOIN times t ON d.timeId = t.timeId
    JOIN ages a ON d.ageId = a.ageId
    WHERE l.location IN ('France','Italy','Cuba','Developed regions','Brazil', 'Republic of Korea')
    AND d.indicatorId IN (85) 
    AND a.ageLabel = '[20-69/70+]'
    AND d.sexId = 3 
    AND d.variantId = 4"

figura12_df = sql_basic(query)%>%
  mutate(location_f = factor(location,
                             levels = c('Developed regions','France','Italy','Cuba','Brazil', 'Republic of Korea'),
                             labels = c('Regiões Desenvolvidas','França','Itália','Cuba','Brasil', 'Corea do Sul'),
                             ordered = TRUE))%>%
  arrange(desc(location))%>%
  mutate(indicator = 'Razão de suporte potencial (20-69 anos/70 anos ou mais)')

gg = ggplot(data = figura12_df) +
  geom_line(aes(x = year,y = value, colour = location_f), linewidth = 1) +
  geom_hline(yintercept = 5, color='grey50', linetype = 'dashed') + 
  scale_x_continuous(name = "Year", breaks=seq(1950,2100,10))+
  scale_y_continuous(
    name = "Razão de suporte potencial (20-69/70+)", 
    labels = scales::label_number(scale = 1, accuracy = 0.1),
    breaks = seq(0,50,5)) +
  scale_colour_startrek(
    name = "Legenda") +
  hrbrthemes::theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom")

ggsave('figures/figura12.jpeg', device = 'jpeg', plot = gg, width = 24, height = 24*0.7, dpi = 300, units = 'cm')

#### Figura 13 Pirâmides ----

url <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/2024/DTB_2024.zip"
destfile <- "data/DTB_2024.zip"
download.file(url, destfile, mode = "wb")
unzip("data/DTB_2024.zip", exdir = "data/dtb")
dtb_area_estudo = readxl::read_excel('data/dtb/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.xls',
                        range = "A8:I5578",
                        col_names = c('uf_co','uf_no',
                                      'rgint_co','rgint_no',
                                      'rgim_co','rgim_no',
                                      'muni_co','muni_co7','muni_no'))%>%
  mutate(across(c(uf_co,rgint_co,rgim_co,muni_co,muni_co7), as.numeric))%>%
  filter(rgint_co %in% 3304:3305)

browseURL("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=200")
sidra_path <- monta_url_sidra(
  t = 200, 
  v = 93,
  c1 = 0,
  c2 = c(4,5),
  C58 = c(1140:1155,2503,6802,6803,92963:92965),
  n6 = dtb_area_estudo$muni_co7,
  p = c(1970,1980,1991,2000,2010)
)

rm(list = ls(pattern = "^figura13"))

figura13_200_raw = consulta_sidra(sidra_path)
temp_1 = figura13_200_raw[1,]%>%
  mutate(across(everything(), ~ snakecase::to_snake_case(.x)))
names(figura13_200_raw) = temp_1

figura13_200 = figura13_200_raw[-1,c(4,7:11)]%>%
  rename(muni_co = município_código,
         muni_no = município,
         idade = grupo_de_idade)%>%
  mutate(across(c('valor','muni_co','ano'), ~ as.numeric(.x)))%>%
  mutate(idade = stringr::str_extract(idade, "^\\d+"),
         idade = if_else(
           idade %in% c("80","85","90","95","100"),"80+",idade),
         idade_f = factor(idade, 
                          levels = c(as.character(seq(0,75,5)),'80+'),
                          labels = c(as.character(seq(0,75,5)),'80+'),
                          ordered = TRUE))

browseURL("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=9514")
sidra_path <- monta_url_sidra(
  t = 9514, 
  v = 93,
  c2 = c(4,5),
  C287 = c(93070,93084:93098,49108,49109,60040,60041,6653),
  n6 = dtb_area_estudo$muni_co7,
  p = 2022,
  C286 = 113635
)

figura13_9514_raw = consulta_sidra(sidra_path)
temp_1 = figura13_9514_raw[1,]%>%
  mutate(across(everything(), ~ snakecase::to_snake_case(.x)))
names(figura13_9514_raw) = temp_1

figura13_9514 = figura13_9514_raw[-1,c(4,6:10)]%>%
  rename(muni_co = município_código,
         muni_no = município)%>%
  mutate(across(c('valor','muni_co','ano'), ~ as.numeric(.x)))%>%
  mutate(idade = stringr::str_extract(idade, "^\\d+"),
         idade = if_else(
           idade %in% c("80","85","90","95","100"),"80+",idade),
         idade_f = factor(idade, 
                          levels = c(as.character(seq(0,75,5)),'80+'),
                          labels = c(as.character(seq(0,75,5)),'80+'),
                          ordered = TRUE))

figura13_df = bind_rows(figura13_9514,figura13_200) %>%
  group_by(ano, idade, idade_f, sexo)%>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")%>%
  group_by(ano)%>%
  mutate(total = sum(valor, na.rm = TRUE),
         poprel = if_else(sexo == 'Mulheres', valor/total, -valor/total))

gg = ggplot(data=figura13_df) +
  geom_bar(aes(fill = sexo, y = poprel, x = idade_f),stat = "identity", 
           position = position_stack())+
  facet_wrap(~ano) +
  coord_flip() + 
  scale_y_continuous(
    name = 'População (%)',
    labels = function(x) {
      scales::label_percent(
        big.mark = '.', decimal.mark = ','
      )(abs(x))
    },
    limits = c(-0.08,0.08),
    breaks = seq(-.06,.06,.03)) +
  scale_fill_manual(
    name = 'Sexo',
    values = c('#5C88DAFF','#CC0C00FF')) +
  scale_x_discrete(name = 'Grupos de Idade') +
  ggplot2::theme_minimal()+
  geom_label(data = figura13_df %>%
               distinct(ano, total),
             aes(x = Inf, y = Inf, 
                 label = paste0("População=", 
                                scales::label_number(big.mark = '.', 
                                                     decimal.mark = ',',
                                                     scale = 10^-3)(total), " k")),
             hjust = 1.0, vjust = 1.0, size = 3, color = "black", inherit.aes = FALSE)
  
ggsave('figures/figura13.jpeg', device = 'jpeg', plot = gg, width = 24, height = 24*0.6, dpi = 300, units = 'cm')

#### Figura 14 Pirâmides ----

figura14_df = bind_rows(figura13_9514,figura13_200) %>%
  filter(ano == '2022')%>%
  inner_join(dtb_area_estudo %>%
               select(-muni_no), by = c("muni_co" = "muni_co7"))%>%
  group_by(idade, idade_f, sexo, rgim_co, rgim_no)%>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")%>%
  group_by(rgim_co, rgim_no)%>%
  mutate(total = sum(valor, na.rm = TRUE),
         poprel = if_else(sexo == 'Mulheres', valor/total, -valor/total))


gg = ggplot(data=figura14_df) +
  geom_bar(aes(fill = sexo, y = poprel, x = idade_f),stat = "identity", 
           position = position_stack())+
  facet_wrap(~rgim_no) +
  coord_flip() + 
  scale_y_continuous(
    name = 'População (%)',
    labels = function(x) {
      scales::label_percent(
        big.mark = '.', decimal.mark = ','
      )(abs(x))
    },
    limits = c(-0.08,0.08),
    breaks = seq(-.06,.06,.03)) +
  scale_fill_manual(
    name = 'Sexo',
    values = c('#5C88DAFF','#CC0C00FF')) +
  scale_x_discrete(name = 'Grupo de Idade') +
  ggplot2::theme_minimal()+
  geom_label(data = figura14_df %>%
               distinct(rgim_no, total),
             aes(x = Inf, y = Inf, 
                 label = paste0("População=", 
                                scales::label_number(big.mark = '.', 
                                                     decimal.mark = ',',
                                                     scale = 10^-3)(total), " k")),
             hjust = 1.0, vjust = 1.0, size = 3, color = "black", inherit.aes = FALSE)

ggsave('figures/figura14.jpeg', device = 'jpeg', plot = gg, width = 24, height = 24*0.6, dpi = 300, units = 'cm')



#### Figura 15 Razão de Sexos ----
library('readxl')
library('snakecase')
library('stringi')

browseURL("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=202")
sidra_path <- monta_url_sidra(
  t = 202, 
  v = 93,
  c2 = c(4,5),
  c1 = 0,
  n6 = dtb_area_estudo$muni_co7,
  p = c(1970, 1980, 1991, 2000, 2010)
)

figura15_202_raw = consulta_sidra(sidra_path)
temp_1 = figura15_202_raw[1,]%>%
  mutate(across(everything(), ~ snakecase::to_snake_case(.x)))
names(figura15_202_raw) = temp_1

figura15_202 = figura15_202_raw[-1,c(4,6,8:10)]%>%
  rename(muni_co = município_código,
         muni_no = município)%>%
  mutate(across(c('valor','muni_co','ano'), ~ as.numeric(.x)))

browseURL("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=9514")
sidra_path <- monta_url_sidra(
  t = 9514, 
  v = 93,
  c2 = c(4,5),
  c287 = 100362,
  c286 = 113635,
  n6 = dtb_area_estudo$muni_co7,
  p = 2022
)

figura15_9514_raw = consulta_sidra(sidra_path)
temp_1 = figura15_9514_raw[1,]%>%
  mutate(across(everything(), ~ snakecase::to_snake_case(.x)))
names(figura15_9514_raw) = temp_1

figura15_9514 = figura15_9514_raw[-1,c(4,6,9:11)]%>%
  rename(muni_co = município_código,
         muni_no = município)%>%
  mutate(across(c('valor','muni_co','ano'), ~ as.numeric(.x)))

dtb_area_estudo_resumo = dtb_area_estudo %>%
  mutate(muni_no_resumo = case_when(
    muni_no == 'Armação dos Búzios' ~ 'Búzios',
    muni_no == 'Arraial do Cabo' ~ 'Arr Cabo',
    muni_no == 'Bom Jesus do Itabapoana' ~ 'B J Itabapoana',
    muni_no == 'Campos dos Goytacazes' ~ 'Campos',
    muni_no == 'Cardoso Moreira' ~ 'Cardoso M',
    muni_no == 'Casimiro de Abreu' ~ 'Casimiro A',
    muni_no == 'Conceição de Macabu' ~ 'C Macabu',
    muni_no == 'Iguaba Grande' ~ 'Iguaba G',
    muni_no == 'Laje do Muriaé' ~ 'Laje Muriaé',
    muni_no == 'Rio das Ostras' ~ 'R Ostras',
    muni_no == 'Santo Antônio de Pádua' ~ 'S  Pádua',
    muni_no == 'São Fidélis' ~ 'S Fidélis',
    muni_no == 'São Francisco de Itabapoana' ~ 'S F Itabapoana',
    muni_no == 'São João da Barra' ~ 'S J Barra',
    muni_no == 'São José de Ubá' ~ 'S J Ubá',
    muni_no == 'São Pedro da Aldeia' ~ 'S P Aldeia',
    .default = muni_no))%>%
  select(muni_co7, muni_no, muni_no_resumo, rgim_co, rgim_no)%>%
  mutate(muni_no_resumo = str_pad(muni_no_resumo, 
                                  width = 14, 
                                  side = "right", pad = " "))

figura15_df = bind_rows(figura15_9514,figura15_202) %>%
  pivot_wider(names_from = sexo, values_from = valor) %>%
  mutate(rs = round(Homens / Mulheres, digits = 2))%>%
  select(-Homens,-Mulheres)%>%
  mutate(ano = factor(ano, levels = c("1970", "1980", "1991", "2000", "2010", "2022"),
                      ordered = T))%>%
  inner_join(dtb_area_estudo_resumo, by = c("muni_co" = "muni_co7"))

library('CGPfunctions')
library('ggrepel')
newggslopegraph_custom <- function(dataframe, Times, Measurement, Grouping, Data.label = NULL, 
                                   Title = "No title given",
                                   TitleJustify = "left",
                                   LineThickness = 1, 
                                   LineColor = "ByGroup", 
                                   DataTextColor = "black", DataLabelPadding = 0.05, 
                                   DataLabelLineSize = 0, 
                                   DataLabelFillColor = "white", WiderLabels = FALSE, 
                                   ReverseYAxis = FALSE, 
                                   ReverseXAxis = FALSE, RemoveMissing = TRUE,
                                   ShowLeftLabels = FALSE, ShowRightLabels = TRUE) {
  
  theme_set(theme_bw())
  
  XTextSize = 8
  YTextSize = 3
  TitleTextSize = 10
  DataTextSize = 2.5
  
  MySpecial <- list(
    scale_x_discrete(position = "top"),
    theme(legend.position = "none",
          panel.border = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x.top = element_text(size = XTextSize, face = "bold"),
          axis.ticks = element_blank(),
          plot.title = element_text(size = TitleTextSize, face = "bold")
    )
  )
  
  if (length(match.call()) <= 4) {
    stop("Not enough arguments passed requires a dataframe, plus at least three variables")
  }
  
  argList <- as.list(match.call()[-1])
  NTimes <- deparse(substitute(Times))
  NMeasurement <- deparse(substitute(Measurement))
  NGrouping <- deparse(substitute(Grouping))
  NData.label <- if (is.null(argList$Data.label)) NMeasurement else deparse(substitute(Data.label))
  
  Times <- enquo(Times)
  Measurement <- enquo(Measurement)
  Grouping <- enquo(Grouping)
  Data.label <- enquo(Data.label)
  
  if (ReverseXAxis) {
    dataframe[[NTimes]] <- forcats::fct_rev(dataframe[[NTimes]])
  }
  
  NumbOfLevels <- nlevels(factor(dataframe[[NTimes]]))
  if (WiderLabels) {
    MySpecial <- c(MySpecial, expand_limits(x = c(0, NumbOfLevels + 1)))
  }
  if (ReverseYAxis) {
    MySpecial <- c(MySpecial, scale_y_reverse())
  }
  
  if (length(LineColor) > 1) {
    if (length(LineColor) < length(unique(dataframe[[NGrouping]]))) {
      message(paste0("\nYou gave me ", length(LineColor),
                     " colors I'm recycling colors because you have ",
                     length(unique(dataframe[[NGrouping]])), " ", NGrouping, "s\n"))
      LineColor <- rep(LineColor, length.out = length(unique(dataframe[[NGrouping]])))
    }
    LineGeom <- list(geom_line(aes_(color = Grouping), size = LineThickness),
                     scale_color_manual(values = LineColor))
  } else {
    if (LineColor == "ByGroup") {
      LineGeom <- list(geom_line(aes_(color = Grouping, alpha = 1), size = LineThickness))
    } else {
      LineGeom <- list(geom_line(aes_(), size = LineThickness, color = LineColor))
    }
  }
  
  if (anyNA(dataframe[[NMeasurement]])) {
    if (RemoveMissing) {
      dataframe <- dataframe %>% group_by(!!Grouping) %>%
        filter(!anyNA(!!Measurement)) %>% droplevels()
    } else {
      dataframe <- dataframe %>% filter(!is.na(!!Measurement))
    }
  }
  
  p <- dataframe %>% ggplot(aes_(group = Grouping, y = Measurement, x = Times)) + LineGeom
  
  if (ShowLeftLabels) {
    p <- p + geom_text_repel(data = dataframe %>% filter(!!Times == min(!!Times)),
                             aes_(label = Grouping),
                             hjust = "left", box.padding = 0.1, point.padding = 0.1,
                             segment.color = "gray", segment.alpha = 0.6, fontface = "plain",
                             size = YTextSize, nudge_x = -1.95, direction = "y", force = 0.5,
                             max.iter = 3000)
  }
  
  if (ShowRightLabels) {
    p <- p + geom_text_repel(data = dataframe %>% filter(!!Times == max(!!Times)),
                             aes_(label = Grouping),
                             hjust = "right", box.padding = 0.1, point.padding = 0.1,
                             segment.color = "gray", segment.alpha = 0.6, fontface = "plain",
                             size = YTextSize, nudge_x = 1.95, direction = "y", force = 0.5,
                             max.iter = 3000)
  }
  
  p +
    geom_label(aes_string(label = NData.label), size = DataTextSize,
               label.padding = unit(DataLabelPadding, "lines"),
               label.size = DataLabelLineSize, color = DataTextColor,
               fill = DataLabelFillColor) +
    MySpecial +
    labs(title = Title)
}



gg = list()
for(imediata in unique(figura15_df$rgim_no)){
  gg[[imediata]] = newggslopegraph(dataframe = figura15_df %>%
                                     filter(rgim_no == imediata),
                                   LineColor =  pal_startrek("uniform")(7),
                                   Times = ano,
                                   Measurement = rs,
                                   Grouping = muni_no_resumo,
                                   RemoveMissing = FALSE,
                                   Title = imediata,
                                   SubTitle = NULL,
                                   Caption = NULL)
}

library('patchwork')

gg = (gg[[1]]+gg[[2]]+gg[[3]])/(gg[[4]]+gg[[5]]+plot_spacer())

ggsave('figures/figura15.jpeg', device = 'jpeg', plot = gg, width = 3600, height = 1800, dpi = 300, units = 'px')

#### Figura 16 Crescimento ----


browseURL("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=202")
sidra_path <- monta_url_sidra(
  t = 202, 
  v = 93,
  c2 = 0,
  c1 = 0,
  n6 = dtb_area_estudo$muni_co7,
  p = c(1970, 1980, 1991, 2000, 2010)
)

figura16_202_raw = consulta_sidra(sidra_path)
temp_1 = figura16_202_raw[1,]%>%
  mutate(across(everything(), ~ snakecase::to_snake_case(.x)))
names(figura16_202_raw) = temp_1

figura16_202 = figura16_202_raw[-1,c(4,8:10)]%>%
  rename(muni_co = município_código,
         muni_no = município)%>%
  mutate(across(c('valor','muni_co','ano'), ~ as.numeric(.x)))

browseURL("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=9514")
sidra_path <- monta_url_sidra(
  t = 9514, 
  v = 93,
  c2 = 6794,
  c287 = 100362,
  c286 = 113635,
  n6 = dtb_area_estudo$muni_co7,
  p = 2022
)

figura16_9514_raw = consulta_sidra(sidra_path)
temp_1 = figura16_9514_raw[1,]%>%
  mutate(across(everything(), ~ snakecase::to_snake_case(.x)))
names(figura16_9514_raw) = temp_1

figura16_9514 = figura16_9514_raw[-1,c(4,8:10)]%>%
  rename(muni_co = município_código,
         muni_no = município)%>%
  mutate(across(c('valor','muni_co','ano'), ~ as.numeric(.x)))


figura16_df = bind_rows(figura16_9514, figura16_202) %>%
  inner_join(dtb_area_estudo_resumo %>%
              select(-muni_no),
            by = c("muni_co" = "muni_co7"))%>%
  group_by(muni_co)%>%
  mutate(crescimento = 1/(ano - lag(ano))*log(valor/lag(valor)))%>%
  mutate(periodo = factor(ano, levels = c('1970', '1980', '1991', '2000', '2010', '2022'), 
                      labels = c('delete', '1970-80', '1980-91', '1991-00', '2000-10', '2010-22'),
                      ordered = TRUE))%>%
  ungroup()%>%
  filter(periodo != 'delete')

gg_ls = list()
for(imediata in unique(figura16_df$rgim_no)){
  gg_ls[[imediata]] = ggplot(figura16_df %>%
           filter(rgim_no == imediata), aes(x = muni_no_resumo, 
                        y = crescimento, fill = crescimento)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    geom_hline(yintercept = 0, color = "grey50", linewidth = .2)+
    facet_grid(. ~ periodo) +
    scale_fill_distiller(guide = guide_colorbar(barwidth = 20, barheight = 0.5),
                         palette = "RdYlBu", direction = -1,
                         labels = scales::percent_format(scale = 100,accuracy = 1)) + 
    scale_y_continuous(labels = scales::percent_format(scale = 100,accuracy = 1))+
    theme_minimal() +
    theme(axis.text.y = element_text(size = 10, hjust = 0),
          axis.text.x = element_text(size = 8, hjust = 0),
          legend.position = 'bottom') +
    labs(x = NULL, 
         y = "Taxa de crescimento (%)",
         fill = NULL,
         title = imediata)
} 
gg = gg_ls[[1]]/gg_ls[[2]]/gg_ls[[3]]/gg_ls[[4]]/gg_ls[[5]]

ggsave('figures/figura16.jpeg', device = 'jpeg', plot = gg, width = 2400, height = 3600, dpi = 300, units = 'px')

