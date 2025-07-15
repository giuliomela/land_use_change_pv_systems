### PLOTS ###

source(
  here::here( # replace with the path to the script_paper_es_impact_of_pv_plant_july25.R file
    "script",
    "paper_impact_PV_plant_july25",
    "script_paper_es_impact_of_pv_plant_july25.R"
  )
)

# loading names correspondance table 

corr_table_path <- here::here("data", "corr_table_nomi_it_en.xlsx")  # Modifica con il percorso corretto
sheet_names <- excel_sheets(corr_table_path)

corr_tables_l <- 
  map(
    sheet_names,
    \(x) read_excel(
      corr_table_path,
      sheet = x
    )
  ) |> setNames(sheet_names)



baseline_results_phase <- baseline_results_phase |> 
  left_join(corr_tables_l[["phases"]], by = c("Fase" = "phase_it")) |> 
  left_join(corr_tables_l[["macro_cices"]], by = c("macro_cices" = "macro_cices_it"))

baseline_results_tot <- 
  baseline_results_phase |> 
  filter(inventario == "base") |> 
  group_by(inventario) |> 
  summarise(delta_value = sum(delta_value), .groups = "drop") |> 
  mutate(macro_cices_en = "Total", phase_en = "Total")

# Palettes

category_pal <- 
  pal_cosmic()(length(unique(baseline_results_phase$phase_en)) + 1) |> 
  setNames(
    sort(c(unique(baseline_results_phase$phase_en), "Total"))
  )

baseline_results_phase_plot <- 
  baseline_results_phase |> 
  filter(inventario == "base") |> 
  bind_rows(baseline_results_tot) |> 
  # group_by(macro_cices, inventario) |> 
  mutate(Fase = fct_reorder(phase_en, delta_value, .fun = sum, .desc = F)) |> 
  # ungroup() |> 
  ggplot(aes(
    x = delta_value,
    y = str_wrap(macro_cices_en, 15),
    fill = phase_en
  )) +
  geom_bar(stat = "identity", position = "stack", color = "grey20", width = .45) +
  #facet_wrap( ~ inventario, scales = "free_x") +
  utilsgm::rse_theme() +
  scale_fill_manual(values = category_pal,
                    labels = ~ stringr::str_wrap(.x, width = 15)) +
  theme(text = element_text(size = 9),
        legend.text = element_text(size = 9),
        axis.text.y = element_text(size = 8)) +
  geom_vline(xintercept = 0) +
  labs(x = "Euro/GWh", y = "")


# Total results by scenario

baseline_res_tot_plot <- 
  baseline_res_tot |> 
  mutate(inventario = fct_reorder(inventario, delta_value)) |> 
  ggplot(aes(x = delta_value, y = inventario, 
             label = round(delta_value,1))) +
  geom_bar(stat = "identity", fill = "tomato", width = .4, color = "grey20") +
  utilsgm::rse_theme() +
  labs(x = "Euro/GWh", y = "") +
  geom_bar_text(outside = T, contrast = T) +
  geom_vline(xintercept = 0)

# Results by biome

base_res_lot_db <- baseline_results_biome |> 
  left_join(corr_tables_l[["macro_cices"]], by = c("macro_cices" = "macro_cices_it")) |> 
  mutate(esvd_biome = fct_reorder(esvd_biome, delta_value, .fun = sum, .desc = F),
         inventario = as.factor(inventario),
         inventario = fct_relevel(inventario, c("base", "a1", "a2")))

esvd_pal <- 
  ggsci::pal_locuszoom()(length(unique(baseline_results$esvd_biome))) |> 
  setNames(
    sort(unique(baseline_results$esvd_biome))
  )

res_biome_plot <- 
  base_res_lot_db |> 
  ggplot(aes(
    x = delta_value,
    y = str_wrap(macro_cices_en, 15),
    fill = esvd_biome
  )) +
  geom_bar(stat = "identity", position = "stack", width = .45, color = "grey20") +
  facet_wrap( ~ inventario, scales = "free_x") +
  utilsgm::rse_theme() +
  scale_fill_manual(values = esvd_pal,
                    labels = ~ stringr::str_wrap(.x, width = 20)
  ) +
  #utilsgm::scale_fill_rse(labels = ~ stringr::str_wrap(.x, width = 20)) +
  theme(text = element_text(size = 9),
        legend.text = element_text(size = 9),
        axis.text.y = element_text(size = 8)) +
  geom_vline(xintercept = 0) +
  labs(x = "Euro/GWh", y = "")

## Plot all phases by type of ES

res_uso_raw <- read_excel( # risultati già in GWh, vanno convertiti in euro 2022
  here("data", "risultati_uso.xlsx"),
  sheet = "tidy"
)


res_uso_tot <- 
  res_uso_raw |> 
  left_join(corr_tables_l[["macro_cices"]], by = c("macro_cices" = "macro_cices_it")) |> 
  select(!macro_cices) |> 
  group_by(macro_cices_en) |> 
  summarise(across(all_of(c("base", "a1", "a2")), sum)) |> 
  ungroup() |> 
  pivot_longer(!macro_cices_en, names_to = "inventario", values_to = "delta_value")


# Converting values to 2022 prices

defl_fct <- gdp_defl[gdp_defl$year == ref_yr, ]$gdp_defl / 
  gdp_defl[gdp_defl$year == 2019, ]$gdp_defl 

res_uso_tot$delta_value <- res_uso_tot$delta_value * defl_fct

res_uso_tot_all <- res_uso_tot |> 
  group_by(inventario) |> 
  summarise(delta_value = sum(delta_value)) |> 
  ungroup() |> 
  mutate(macro_cices_en = "Total")

res_total_all_se_plot <- 
  res_uso_tot |> 
  bind_rows(res_uso_tot_all) |> 
  mutate(inventario = fct_relevel(inventario, c("base", "a1", "a2")),
         fill_color = ifelse(macro_cices_en == "Total", "tot", "cices")) |> 
  ggplot(aes(x = delta_value, y = macro_cices_en, fill = fill_color,
             label = round(delta_value))) +
  geom_bar(stat = "identity", 
           width = .4,
           color = "grey20") +
  utilsgm::rse_theme() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(tot = "tomato", cices = "darkgreen")) +
  labs(x = "Euro/Gwh", y = "") +
  scale_x_continuous(limits = c(-500, 1200)) +
  geom_bar_text(outside = T, contrast = T, min.size = 12) +
  geom_vline(xintercept = 0) +
  facet_wrap(~ inventario)

### Plot results by life cycle phase


baseline_results_phase2 <- 
  baseline_results_phase |> 
  mutate(
    phase_en = case_when(
      str_detect(phase_en, "EoL") ~ "End of life",
      str_detect(phase_en, "transport") ~ "Transport",
      TRUE ~ "Production"
    )
  ) |> 
  group_by(inventario, phase_en, macro_cices_en) |> 
  summarise(delta_value = sum(delta_value)) |> 
  ungroup()

res_ciclo_vita <- res_uso_tot |> 
  mutate(phase_en  = "Use") |> 
  bind_rows(baseline_results_phase2)

res_ciclo_vita_tot <- 
  res_ciclo_vita |> 
  group_by(inventario, phase_en) |> 
  summarise(delta_value = sum(delta_value)) |> 
  ungroup() |> 
  mutate(macro_cices_en = "Total")


fase_pal <-
  pal_cosmic()(
    length(unique(res_ciclo_vita$phase_en))
  ) |> setNames(
    sort(unique(res_ciclo_vita$phase_en))
  )

plot_labels_ciclo_vita <- 
  res_ciclo_vita |> 
  group_by(macro_cices_en, inventario) |> 
  summarise(delta_value = sum(delta_value), .groups = "drop")


plot_res_ciclo_vita <-  
  res_ciclo_vita |> 
  bind_rows(res_ciclo_vita_tot) |> 
  group_by(macro_cices_en, inventario) |> 
  mutate(Fase = fct_reorder(phase_en, delta_value, .fun = sum, .desc = F)) |> 
  ungroup() |> 
  ggplot(aes(
    x = delta_value,
    y = str_wrap(macro_cices_en, 15),
    fill = Fase
  )) +
  geom_bar(stat = "identity", position = "stack", color = "grey20", width = 0.45) +
  facet_wrap(~ inventario) +
  utilsgm::rse_theme() +
  scale_fill_manual(values = fase_pal,
                    labels = ~ stringr::str_wrap(.x, width = 15)) +
  # theme(text = element_text(size = 10),
  #       legend.text = element_text(size = 10),
  #       axis.text.y = element_text(size = 9)) +
  geom_vline(xintercept = 0) +
  labs(x = "Euro/GWh", y = "")

plot_list <- # list of plots used in the manuscript
  list(
    baseline_results_phase_plot = baseline_results_phase_plot,
    baseline_res_tot_plot = baseline_res_tot_plot,
    res_biome_plot = res_biome_plot,
    res_total_all_se_plot = res_total_all_se_plot,
    plot_res_ciclo_vita = plot_res_ciclo_vita
  )

# map(
#   names(plot_list),
#   \(x) {
#     ggsave(
#       here("output", "plot_paper_mar25", 
#            paste0(x, ".jpg")),
#       plot = plot_list[[x]],
#       dpi = 400,
#       width = 4,
#       height = 3
#     )
#   }
# )

