### Ecosystem services loss/gain dlinked to the construction of a PV plant

# loading packages (install pacman package: 'install.packages("pacman")')


pacman::p_load(
  here,
  tidyverse,
  viridis,
  gt,
  gtExtras,
  readxl,
  showtext,
  ggfittext,
  ggsci
)

ref_yr <- 2022 # reference year for the analysis

# defining path to LCA invetories

path_to_data <- # substitute with the path of here you stored the inventario_casi_fotovoltaico.xlsx file
  here(
    "data",
    "lca_inventari",
    "inventario_casi_fotovoltaico.xlsx"
  )

case_sheets <- #identifying inventory file names
  readxl::excel_sheets(path_to_data) |> 
  str_subset("kwh") # functional unit: kWh produced

# Loading inventories

inventari_raw <- map( # a named list
  case_sheets,
  \(x) readxl::read_excel(
    path = path_to_data,
    sheet = x
  )
) |> setNames(case_sheets)

### Computing land use changes

# a function to tidy the inventories and to compute land use changes

compute_delta_area <- function(x){ # to be executed later
  
  inventari_raw[[x]] |> 
    filter(!str_detect(Substance, "Occupation")) |> 
    dplyr::select(!Total) |> 
    pivot_longer(!c(Substance, Compartment, Unit), names_to = "Fase", values_to = "Area") |> 
    mutate(Substance = str_remove(Substance, "Transformation, "),
           From_To = word(Substance, 1),
           Substance = str_remove(Substance, "from |to "),
           Area = if_else(
             From_To == "from",
             Area * -1,
             Area
           ),
           .keep = "unused") |> 
    group_by(Substance, Fase) |> 
    summarise(Area = sum(Area)) |> 
    ungroup()
  
}

# Actually computing land use changes (delta area)

delta_area_l <- # result is a list
  map( # a list
    case_sheets,
    \(x) compute_delta_area(x) # function defined earlier
  ) |> 
  setNames(case_sheets)

delta_area <- # binds all datasets and removes the "total" phase to avoid double counting
  bind_rows(delta_area_l,
            .id = "inventario") |> 
  filter(Fase != "Totale") 

delta_area <- # converting areas from m2 to hectares
  delta_area |> 
  mutate(ha = Area * 0.0001,
         .keep = "unused")


### Loading correspondence table between SimaPro land use classes and land use classes from the ESVD database (CICES)

corr_table <- 
  readxl::read_excel(
    here("data", "ecosystems_corr_table.xlsx"),
    sheet = "corr_table"
  )

delta_area <- delta_area |> # joining the correspondence table to the land use change table
  left_join(corr_table, by = c("Substance" = "sima_pro")) |> 
  mutate(inventario = str_remove(inventario, "_kwh"))

### Loading ecosystem service values from the ESVD database (April 24 version, latest available)

esvd_raw <- read_csv(here("data", "esvd_apr24.csv")) # subustiture with the path where you saved the esvd_apr24.csv file

esvd_raw <- # renaming
  esvd_raw |> 
  mutate(macro_cices = case_when(
    str_starts(CICES, "1") ~ "Provisioning (Biotic)",
    str_starts(CICES, "2") ~ "Regulation & Maintenance (Biotic)",
    str_starts(CICES, "3") ~ "Cultural (Biotic)",
    str_starts(CICES, "4") ~ "Provisioning (Abiotic)",
    str_starts(CICES, "5") ~ "Regulation & Maintenance (Abiotic)",
    str_starts(CICES, "6") ~ "Cultural (Abiotic)"
  ))



cices <- # loading CICES ES classification
  read_excel(here("data", "ecosystems_corr_table.xlsx"), # substitute with the path to which you saved the ecosystems_corr_table.xlsx file
                    sheet = "es_cices")

anthropogenic_biozones_to_exclude <- # defining CICES biozones to exclude from the analysis (anthropogenic)
  setdiff(
    esvd_raw |> filter(str_detect(ESVD2.0_Biome, "Anthropogenic")) |> pull(ESVD2.0_Ecozone_1) |> unique(),
    c("Anthropogenic freshwater systems", "Rivers and streams", "Freshwater lakes")
  )

### Computing ES values to be used in the analysis

values_raw <- # selecting values to be averaged later
  esvd_raw |> #starting point is raw values from the ESVD database
  filter(`Valuation Methods` != "VT") |> # avoiding value transfer as evaluation method 
  mutate(`Int$ Per Hectare Per Year` = if_else(
    ESVD2.0_Biome == "Anthropogenic systems" & ESVD2.0_Ecozone_1 %in% anthropogenic_biozones_to_exclude,
    NA_real_,
    `Int$ Per Hectare Per Year`),
    `Int$ Per Hectare Per Year` = if_else(
      ESVD2.0_Biome == "Marine" & ESVD2.0_Ecozone_1 != "Deep sea floor",
      NA_real_,
      `Int$ Per Hectare Per Year`
    )) |> 
  dplyr::select(iso3c = Country_Codes, esvd_biome = ESVD2.0_Biome_1, CICES, macro_cices, TEEB = ES_1, value = `Int$ Per Hectare Per Year`, continent = Continent) |> 
  mutate(esvd_biome = case_when(
    str_detect(esvd_biome, "forest") ~ "Forests", # renaming
    TRUE ~ esvd_biome
  )) |> 
  group_by(iso3c, esvd_biome, CICES, macro_cices, TEEB, value, continent) |> 
  summarise(value = mean(value)) |> # averaging values
  ungroup() |> 
  filter(macro_cices != "Provisioning (Abiotic)") |> 
  mutate(macro_cices = case_when( # renaming in Italian
    str_detect(macro_cices, "Cultural") ~ "Culturali",
    str_detect(macro_cices, "Regulation") ~ "Regolazione",
    TRUE ~ "Approvvigionamento"
  ))


#Computing average ES values

values_all_raw <- 
  values_raw |> 
  filter(str_length(CICES) == 7) |> # only values referring to biomes (code length == 7)
  drop_na() 

values_all_raw_no_outlier <- # removing outliers (interquantile range)
  values_all_raw |> 
  group_by(esvd_biome, continent) |>
  mutate(q25 = quantile(value, probs = 0.25, na.rm = T),
         q75 = quantile(value, probs = 0.75, na.rm = T),
         iqr = IQR(value, na.rm = T),
         upper = q75 + 1.5 * iqr,
         lower = q25 - 1.5 * iqr) |>
  ungroup() |>
  filter(value >= lower & value <= upper) |> 
  filter(!(str_detect(esvd_biome, "Anth") & value > 10^4)) # removing two values for anthropogenic systems that were still excessively high even after the outliers were taken out (the main one was in the millions of euros/ha)

### Creating a table with all biomes and ES groups

esvd_cices <- # a grid with all biomes-ES groups combinations
  expand_grid(
    esvd_biome = sort(unique(corr_table$esvd)),
    macro_cices = sort(values_all_raw$macro_cices |> unique())
  )


# Computing world and continent-wise mean values

mean_values_world <- # world mean values
  values_all_raw_no_outlier |> 
  group_by(esvd_biome, macro_cices) |> 
  summarise(value = mean(value, na.rm = T),
            n = n()) |> 
  ungroup() |> 
  mutate(aggregate = "world")

mean_values_continent <- # mean values by continent
  values_all_raw_no_outlier |> 
  group_by(esvd_biome, continent, macro_cices) |> 
  summarise(value = mean(value, na.rm = T),
            n = n()) |> 
  ungroup() |> 
  filter(continent != "Global") |> 
  rename(aggregate = continent)

wb_data <- # downloading GDP per capita and exchange rates to perfom the value transfer (source World Bank)
  rdbnomics::rdb(
  c(
    "WB/WDI/A-NY.GDP.MKTP.CD-EMU", # GDP Euro Area
    "WB/WDI/A-NY.GDP.MKTP.PP.CD-EMU", # Gdp Euro area PPP
    "WB/WDI/A-PA.NUS.FCRF-EMU" # US dollar / euro exchange rate
  )
)


ppp_factor_euro <- 
  wb_data |> 
  dplyr::select(original_period, series_code, value) |> 
  mutate(year = as.numeric(original_period), .keep = "unused") |> 
  pivot_wider(names_from = series_code, values_from = value) |> 
  drop_na() |> 
  mutate(
    ppp_factor = `A-NY.GDP.MKTP.CD-EMU` / `A-NY.GDP.MKTP.PP.CD-EMU`,
    ppp_factor_euro = ppp_factor * `A-PA.NUS.FCRF-EMU`
  ) |> 
  dplyr::select(year, ppp_factor_euro)

# Eurozone GDP deflator is from Eurostat

gdp_defl <- eurostat::get_eurostat("nama_10_gdp", filters = list(geo = "EA20", unit = "PD15_EUR", na_item = "B1GQ"))

gdp_defl <- 
  gdp_defl |> 
  mutate(year = year(time),
         gdp_defl = values,
         .keep = "unused") |> 
  drop_na() |> 
  dplyr::select(year, gdp_defl)

ppp_factor_euro <- #taking into account inflation with Euro Zone GDP deflator
  ppp_factor_euro |> 
  left_join(gdp_defl) |> 
  mutate(ppp_factor_euro_k = ppp_factor_euro / gdp_defl * gdp_defl[year == ref_yr]) 

ref_yr_fct <- # factor to apply to ES values to perform the value transfer
  ppp_factor_euro[ppp_factor_euro$year == ref_yr, ]$ppp_factor_euro_k

mean_values <- 
  mean_values_world |> 
  bind_rows(mean_values_continent) |> 
  mutate(!!paste0("value_", ref_yr, "_eur") := value * ref_yr_fct) #value transfer

eu_countries <- #ISO Codes fo EU countries
  eurostat::eu_countries |> 
  mutate(iso3c = countrycode::countrycode(name, origin = "country.name", destination = "iso3c")) |>   pull(iso3c)

#saveRDS(eu_countries, here("data", "eu_countries.rds"))

### Computing average values for the EU and China
# For each biome, average values are computed for both the EU and China. When less than 10 values are available, the continental mean is used.
# When even at continental level there are less than 10 studies for each biome, the world average is used.

# Number of studies referring to China and the EU

n_eu_chn <- values_all_raw[values_all_raw$iso3c %in% c("CHN", eu_countries), ]

n_eu_chn$aggregate <- ifelse(
  n_eu_chn$iso3c == "CHN",
  "CHN",
  "EEU"
)

n_eu_chn$iso3c <- NULL

# Getting the count of observations per group combination
group_counts <- table(n_eu_chn$esvd_biome, n_eu_chn$aggregate, n_eu_chn$macro_cices)

# Convert the table to a data frame for merging
n_eu_chn <- as.data.frame(group_counts)

names(n_eu_chn) <- c("esvd_biome", "aggregate", "macro_cices", "n")

n_eu_chn <- as_tibble(n_eu_chn)

# Computing average ES values for China and the EU

mean_values_eu_chn <- 
  values_all_raw_no_outlier[values_all_raw_no_outlier$iso3c %in% c("CHN", eu_countries), ]

mean_values_eu_chn$aggregate <- 
  ifelse(
    mean_values_eu_chn$iso3c == "CHN",
    "CHN",
    "EEU"
  )

mean_values_eu_chn$iso3c <- NULL

mean_values_eu_chn <- aggregate(value ~ esvd_biome + aggregate + macro_cices, 
                                data = mean_values_eu_chn, 
                                FUN = mean)

mean_values_eu_chn <- merge(mean_values_eu_chn, n_eu_chn)

mean_values_eu_chn[[paste0("value_", ref_yr, "_eur")]] <- 
  mean_values_eu_chn$value * ref_yr_fct

eu_chn_na_less10 <- expand_grid(
  esvd_cices,
  aggregate = c("CHN", "EEU")
) |> 
  left_join(n_eu_chn) |> 
  filter(is.na(n) | n < 10) # biomes with less than 10 studies/values in China and the EU

eu_chn_na_more10 <- expand_grid(
  esvd_cices,
  aggregate = c("CHN", "EEU")
) |> 
  left_join(n_eu_chn) |> 
  filter(n >= 10) # biomes with more than 10 studies/values in China and the EU

mean_values_eu_chn_proper <- # China and EU mean values for biomes with more than 10 observations
  eu_chn_na_more10 |> 
  left_join(dplyr::select(mean_values_eu_chn, !n)) |> 
  mutate(source = "country")

mean_values_eu_chn_continent <- # For combinations of country (EU/China) and biomes with less than 10 obs, mean continental values are considered (i.e. Asia's mean value for China)
  eu_chn_na_less10 |> 
  mutate(continent = if_else(
    aggregate == "CHN",
    "Asia",
    "Europe"
  ),
  n = NULL) |> 
  left_join(mean_values, by = c("esvd_biome", "macro_cices", "continent" = "aggregate")) |> 
  mutate(source = "continent", continent = NULL)

mean_values_eu_chn_world <- # In cases in which even at continental level there are less than 10 studies providing values for each biome, the world average is used instead
  mean_values_eu_chn_continent |> 
  mutate(value = if_else(is.na(value), 0, value)) |> 
  filter(n < 10 | value == 0) |> 
  dplyr::select(esvd_biome, macro_cices, aggregate) |> 
  left_join(dplyr::select(filter(mean_values, aggregate == "world"), !aggregate)) |> 
  mutate(source = "world")

mean_values_eu_chn_continent <- 
  mean_values_eu_chn_continent |> 
  filter(n >= 10) 

mean_values_world_world <- 
  mean_values |> 
  filter(aggregate == "world") |> 
  mutate(source = "world")

mean_values_def_eu_chn <- # A tibble with China, EU, continental and world average ES values
  bind_rows(
    mean_values_eu_chn_proper,
    mean_values_eu_chn_continent,
    mean_values_eu_chn_world,
    mean_values_world_world
  )

### RESULTS

t <- 44 # PV plant life (in years)

mean_values_def_eu_chn_nested <- # some data manpulation
  mean_values_def_eu_chn |> 
  group_by(esvd_biome, aggregate) |> 
  nest() |> 
  rename(values = data)

baseline_results <- # baseline results
  delta_area |> 
  rename(esvd_biome = esvd) |> 
  mutate(aggregate = case_when(
    Fase == "Modulo" ~ "CHN",
    Fase == "Trasporto modulo" ~ "world",
    TRUE ~ "EEU"
  )) |> 
  group_by(inventario, esvd_biome, aggregate) |> 
  nest() |> 
  left_join(mean_values_def_eu_chn_nested) |> 
  unnest(values) |> 
  drop_na() 

baseline_results_phase <- # baseline results for each life-cycle phase
  delta_area |> 
  rename(esvd_biome = esvd) |> 
  mutate(aggregate = case_when(
    Fase == "Modulo" ~ "CHN",
    Fase == "Trasporto modulo" ~ "world",
    TRUE ~ "EEU"
  )) |> 
  group_by(inventario, esvd_biome, Fase, aggregate) |> 
  nest() |> 
  left_join(mean_values_def_eu_chn_nested) |> 
  unnest(values) |> 
  unnest(data) |> 
  drop_na() |> 
  group_by(inventario, Fase, macro_cices) |> 
  summarise(delta_value = sum(.data[[paste0("value_", ref_yr, "_eur")]] * ha) * t) |> 
  ungroup() |> 
  mutate(delta_value = delta_value * 10^6) # results in euro/GWh



baseline_results_biome <- # baseline results per biome
  baseline_results |> 
  unnest(data) |> 
  group_by(inventario, esvd_biome, macro_cices) |> 
  summarise(delta_value = sum(.data[[paste0("value_", ref_yr, "_eur")]] * ha) * t) |> 
  ungroup() |> 
  mutate(delta_value = delta_value * 10^6) 

baseline_res_tot <- # beseline results per PV configuration
  baseline_results |> 
  unnest(data) |> 
  group_by(inventario) |> 
  summarise(delta_value = sum(.data[[paste0("value_", ref_yr, "_eur")]] * ha) * t) |> 
  ungroup() |> 
  mutate(delta_value = delta_value * 10^6) 



