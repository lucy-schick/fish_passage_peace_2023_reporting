#---------------------pscis export only--------------------------

# read in cleaned form from Q after review and finalization,
# create csv that can be used to paste special into the PSCIS submission spreadsheet




gpkg_path = '../../gis/sern_peace_fwcp_2023/data_field/2023/form_pscis_2023.gpkg'
# gpkg_path = '../../gis/sern_skeena_2023/background_layers.gpkg'
# my_form <- sf::st_read(dsn = gpkg_path, layer = 'crossings', use_stream = T)

# read it in
dat <- sf::st_read(dsn = gpkg_path)

# update utms for input to spreadsheet and update lat and long so we can recreate with just a version controlled csv in the repo
dat2 <-  dat %>%
  fpr::fpr_sp_assign_latlong() %>%
  fpr::fpr_sp_assign_utm()

# burn back to GIS project with the updated coordinates
# dat2 %>%
#   sf::st_write(gpkg_path, append=F, delete_dsn=T)

# burn out the most recent version of everything to csv
dat2 %>%
  readr::write_csv(paste0(
    'data/dff/form_pscis_2023.csv'), na = '')

# prep for csvs for cut and paste
dat3 <- dat2 %>%
  # only select columns from template object
  dplyr::select(any_of(names(fpr_xref_template_pscis()))) %>%
  # remove scoring columns, as these can't be copied and pasted anyways because of macros
  dplyr::select(-stream_width_ratio:-barrier_result) %>%
  # then arrange it by pscis id to separate phase 1s from phase 2 and reassessments
  dplyr::arrange(pscis_crossing_id, date)


# phase1
dat_p1 <- dat3 %>%
  dplyr::filter(!is.na(my_crossing_reference))

# put a switch to see if the rassess sites are in the template?
dat_r <- dat3 %>%
  dplyr::filter(
    pscis_crossing_id %in%
      (fpr::fpr_import_pscis(workbook_name = 'pscis_reassessments.xlsm') %>%
         dplyr::pull(pscis_crossing_id)
      )
  )

# phase 2 sites should be the ones left over






# get a list of unique sites from hab con
hc <- fpr_import_hab_con()
