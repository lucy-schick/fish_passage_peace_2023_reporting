#---------------------pscis export only--------------------------

# read in cleaned form from Q after review and finalization,
# create csv that can be used to paste special into the PSCIS submission spreadsheet


# first we back up the gpkg in the repo and update the coordinate columns in the gpkg in QGIS
pscis_raw <- fpr_sp_gpkg_backup(
  path_gpkg = "~/Projects/gis/sern_peace_fwcp_2023/data_field/2023/form_pscis_2023.gpkg",
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  write_to_rdata = TRUE,
  return_object = TRUE)


# prep for csvs for cut and paste by subsetting columns to those in spreadsheet
pscis_spdsht_cols <- pscis_raw %>%
  # make a column to identify phase 1 sites
  dplyr::mutate(
    phase = case_when(
      !is.na(my_crossing_reference) ~ 1,
      TRUE ~ 2
    )
  ) %>%
  # arrange by phase then date (helps when following through raw photos)
  dplyr::arrange(phase, date_time_start) %>%
  # only select columns from template object
  dplyr::select(any_of(names(fpr_xref_template_pscis())), site_id) %>%
  # remove scoring columns, as these can't be copied and pasted anyways because of macros
  dplyr::select(-stream_width_ratio:-barrier_result)
  # then arrange it by pscis id to separate phase 1s from phase 2 and reassessments if using just one csv
  # dplyr::arrange(pscis_crossing_id, date)


# get phase1 sites ids
sites_p1 <- pscis_spdsht_cols %>%
  dplyr::filter(!is.na(my_crossing_reference)) %>%
  dplyr::pull(my_crossing_reference)

# seperate out phase 1 sites and sort for cut and paste
dat_p1 <- pscis_spdsht_cols %>%
  dplyr::filter(!is.na(my_crossing_reference)) %>%
  dplyr::arrange(date)

# see if the rassess sites are in the template
sites_reassess <- fpr::fpr_import_pscis(workbook_name = 'pscis_reassessments.xlsm') %>%
  dplyr::pull(pscis_crossing_id)

dat_r <- pscis_spdsht_cols %>%
  dplyr::filter(pscis_crossing_id %in% sites_reassess)

# put a switch to pull out the phase 2 sites by looking at the fisheries data submission template
# this doesn't actually work because some of these sites might just be monitoring sites!!
sites_p2 <- fpr_import_hab_con(col_filter_na = TRUE, row_empty_remove = TRUE, backup = FALSE) %>%
  purrr::pluck("step_1_ref_and_loc_info") %>%
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date))) %>%
  tidyr::separate(alias_local_name, into = c('site', 'location'), remove = FALSE) %>%
  mutate(site = as.numeric(site)) %>%
  dplyr::filter(!stringr::str_detect(alias_local_name, "_ef")) %>%
  dplyr::distinct(site, .keep_all = FALSE) %>%
  dplyr::pull(site)

dat_p2 <- pscis_spdsht_cols %>%
  dplyr::filter(pscis_crossing_id %in% sites_phase2)

# do a sanity check to make sure we have all the sites only in one place


