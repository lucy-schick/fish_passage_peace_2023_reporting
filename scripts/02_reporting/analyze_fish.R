library(cowplot)


fpr_db_query(fpr_dbq_lscols(table_name = 'streams'))


query = "SELECT
  e.fish_observation_point_id,
  e.species_code,
  e.geom,
  s.gradient,
  s.stream_order,
  s.upstream_area_ha,
  s.mad_m3s,
  s.channel_width,
  s.watershed_group_code,
  s.linear_feature_id,
  round((ST_Z((ST_Dump(ST_LocateAlong(s.geom, e.downstream_route_measure))).geom))::numeric) as elevation
FROM bcfishobs.fiss_fish_obsrvtn_events_vw e
INNER JOIN bcfishpass.streams s
ON e.linear_feature_id = s.linear_feature_id
WHERE e.downstream_route_measure > s.downstream_route_measure;"
# WHERE e.species_code = 'WCT'

# AND e.watershed_group_code = 'ELKR';" ##looks like only one wct elsewhere anyway which is odd since they show
# get the correct segmented ID with the downstream route measure



f_raw <- fpr::fpr_db_query(query = query)


f_tidy <- f_raw |>
  # tidy the gradient data so we filter out anything < 0 and > 0.3
  dplyr::filter(gradient > 0 & gradient < 0.3) |>
  mutate(gradient_percent = round(gradient*100,0))

# lets have a look at the grayling data
f_tidy |>
  dplyr::filter(species_code == "GR") |>
  esquisse::esquisser()

# ---------- gradient
sp <- c('GR')
lab_x <- "Gradient (%)"
lab_y <- "Count (#)"
plot_fish_hist <- ggplot(f_tidy |>
                           dplyr::filter(species_code %in% sp)) +
  geom_bar(aes(gradient_percent),
           fill = "#56B4E9", alpha = 0.8) +
  labs(x = lab_x, y = lab_y) +
  scale_x_binned(
    limits = c(0,10),
    n.breaks = 12) +
  scale_y_continuous(
    # don't expand y scale at the lower end
    expand = expansion(mult = c(0, 0.05))
  ) +
  # facet_grid(~species_code) +
  cowplot::theme_minimal_hgrid() +
  cowplot::panel_border()

plot_fish_hist


# ---------- channel width
sp <- c('GR')
lab_x <- "Channel Width (m)"
lab_y <- "Count (#)"

plot_fish_hist <- ggplot(f_tidy |>
                           dplyr::filter(species_code %in% sp)) +
  geom_bar(aes(channel_width),
           fill = "#56B4E9", alpha = 0.8) +
  labs(x = lab_x, y = lab_y) +
  scale_x_binned(
    limits = c(0,50),
    n.breaks = 5) +
  scale_y_continuous(
    # don't expand y scale at the lower end
    expand = expansion(mult = c(0, 0.05))
  ) +
  # facet_grid(~species_code) +
  cowplot::theme_minimal_hgrid() +
  cowplot::panel_border()

plot_fish_hist






# ggplot(wct, aes(x=channel_width, y=mad_m3s)) +
#   geom_point()

# wct_elkr_grad <- wct %>%
#   mutate(Gradient = case_when(
#     gradient < .03 ~ '0 - 3 %',
#     gradient >= .03 &  gradient < .05 ~ '03 - 5 %',
#     gradient >= .05 &  gradient < .08 ~ '05 - 8 %',
#     gradient >= .08 &  gradient < .15 ~ '08 - 15 %',
#     gradient >= .15 &  gradient < .22 ~ '15 - 22 %',
#     gradient >= .22  ~ '22+ %')) %>%
#   group_by(Gradient)  %>%
#   summarise(Count = n()) %>%
#   mutate(total = nrow(wct),
#          Percent = round(Count/total * 100, 0))
#
# wct_elkr_grad$gradient_id <- c(3,5,8,15,22,99)

##save this for the report
##burn it all to a file we can use later
wct_elkr_grad %>% readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/02_prep_report/wct_mad_chanwidthgrade.csv'))



