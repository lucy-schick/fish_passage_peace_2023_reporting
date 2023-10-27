source('scripts/packages.R')

# import form_pscis.gpkg direct from mergin and rearrange then burn to csv

# relative path to the q project form

#--------------------import---------------------------

# name the project directory we are pulling from
dir_project <- 'sern_peace_fwcp_2023'

# read in the forms and join together
form_pscis1 <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/form_pscis.gpkg'))

form_pscis2 <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/form_pscis_20230825_resaved.gpkg'))

# check to see that column names are equiv (must be if number is the same but still)
identical(names(form_pscis1), names(form_pscis2))

# bind rows and burn as csv to back up folder with coords added
utm <- 10

form_prep <- bind_rows(
  form_pscis1,
  form_pscis2
) %>%
  st_transform(crs = 26910) %>%
  poisspatial::ps_sfc_to_coords(X = 'easting', Y = 'northing') %>%
  # add in utm zone of study area
  mutate(utm_zone = utm) %>%
  readr::write_csv(paste0('data/inputs_extracted/mergin_backups/form_pscis_raw_',
                   format(lubridate::now(), "%Y%m%d"),
                   '.csv'))

# burn amalgamated form to backup folder as gpckg, use latest synced version number on mergin
form_amalg <- bind_rows(
  form_pscis1,
  form_pscis2
  ) %>%
  sf::st_write('data/inputs_extracted/mergin_backups/form_pscis_v70.gpkg', append=FALSE)

# read in cleaned amalgamated pscis form
form_pscis <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/form_pscis_v70.gpkg')) %>%
  st_transform(crs = 26910) %>%
  poisspatial::ps_sfc_to_coords(X = 'easting', Y = 'northing') %>%
  # add in utm zone of study area
  mutate(utm_zone = utm)

# check for duplicates
form_pscis %>%
  filter(!is.na(site_id)) %>%
  group_by(site_id) %>%
  filter(n()>1)

# this is a table that cross references column names for pscis table and has the columns in the same order as the spreadsheet
xref_names_pscis <- fpr::fpr_xref_pscis

# get order of columns as per the excel template spreadsheet
# this can be used as a select(all_of(name_pscis_sprd_ordered)) later
# to order columns for the field form and/or put the field entered table in order
name_pscis_sprd_ordered <- fpr::fpr_xref_pscis %>%
  filter(!is.na(spdsht)) %>%
  select(spdsht) %>%
  pull(spdsht)

# see names that coincide between the xref table and what we have
intersect(name_pscis_sprd_ordered, names(form_pscis))

# see which are different
setdiff(name_pscis_sprd_ordered, names(form_pscis))
# order matters
setdiff(names(form_pscis), name_pscis_sprd_ordered)

##---------------------pscis clean only--------------------------
form_prep1 <- form_pscis %>%
  #split date time column into date and time
  dplyr::mutate(date_time_start = lubridate::ymd_hms(date_time_start),
                date = lubridate::date(date_time_start),
                time = hms::as_hms(date_time_start)) %>%
  # filter out to get only the records newly created
  filter(!is.na(date_time_start)) %>%
  # create a site_id
  mutate(
    site_id = case_when(is.na(pscis_crossing_id) ~ my_crossing_reference,
                        T ~ pscis_crossing_id)
  ) %>%
  # remove the bute events as they are duplicates and just for training in 2023, and sometimes the camera is not defined
  filter(camera_id != 'newgraph_bute'| is.na(camera_id)) %>%
  # remove the form making site
  filter(site_id != '12345')

# clean up data fields to make copy and paste to prov template easier
form_prep2 <- form_prep1 %>%
  # some columns that have yes/no answers have NA values in mergin, need to change to No
  # need to add "No" as default values to mergin
  mutate(across(contains('yes_no'), ~replace_na(.,'No'))) %>%
  # some numeric fields for CBS have NA values when a user input 0
  mutate(across(c(outlet_drop_meters, outlet_pool_depth_0_01m, culvert_slope_percent, stream_slope),
                ~case_when(crossing_type == 'Closed Bottom Structure' ~replace_na(.,0),
                TRUE ~ .
                ))) %>%
  # change "trib" to long version "Tributary"
  mutate(stream_name = str_replace_all(stream_name, 'Trib ', 'Tributary ')) %>%
  # change "Hwy" to "Highway"
  mutate(road_name = str_replace_all(road_name, 'Hwy ', 'Highway '))

# to use all the columns from the template first we make an empty dataframe from a template
template <- fpr::fpr_import_pscis() %>%
  slice(0)

# then we join it to our populated spreadsheet
# we may as well keep all the columns that are not in the spreadsheet and append to the end
form <- bind_rows(
  template,
  form_prep2
) %>%
  # only select columns from template object
  select(any_of(names(template))) %>%
  # then arrange it by pscis id to separate phase 1s from reassessments
  arrange(pscis_crossing_id, date)

# burn to a csv
form %>% readr::write_csv(paste0(
    'data/dff/form_pscis_',
    format(lubridate::now(), '%Y%m%d'),
    '.csv'), na='')

# --------------------moti climate change ---------------------------
#
# moti_names <- setdiff(names(form_pscis), name_pscis_sprd_ordered) %>%
#   enframe(name = NULL, value = 'spdsht') %>%
#   mutate(report = str_to_title(spdsht),
#          report = stringr::str_replace_all(report, '_', ' '),
#          report = stringr::str_replace_all(report, 'event affecting culvert', ''),
#          report = stringr::str_replace_all(report, 'id', 'ID'),
#          report = stringr::str_replace_all(report, 'Gps', 'GPS'),
#          report_include = case_when(
#            str_detect(spdsht,
#                       'photo|long|lat|mergin|surveyor|gps|width|utm|time|source|camera|aggregated|rowid|geometry') ~ F,
#            T ~ T
#          ),
#          id_join = NA_integer_,
#          id_side = NA_integer_) %>%
#   filter(spdsht != 'stream_width_ratio_score')
# # filter(report_include == T)
#
# # burn out to csv so we can manually do the descriptions
# moti_names %>%
#   write_csv('data/inputs_raw/moti_climate.csv')
#
# # we want to get our climate change risk information summarized for each site.
# # if we were to add a tag to the names or a xref tie that tells us if each column is part of this or not we would get ahead...
#
# ##-------------------- moti correct chris_culvert_id-----------------------
# # one thing we definitely need to do it get the chris_culvert_id for each site as we used the wrong one on our forms. We should be able to cross ref the ids from bcdata
# # so let's try that first
#
#
# # get_this <- bcdata::bcdc_tidy_resources('ministry-of-transportation-mot-culverts') %>%
# #   filter(bcdata_available == T)  %>%
# #   pull(package_id)
# #
# # dat <- bcdata::bcdc_get_data(get_this)
# #
# # moti_raw <- dat %>%
# #   purrr::set_names(nm = janitor::make_clean_names(names(dat)))
# #
# # # match our sites to ids
# # moti <- left_join(
# #   form_prep,
# #
# #   moti_raw %>% select(culvert_id, chris_culvert_id) %>% sf::st_drop_geometry(),
# #
# #   by = c('mot_culvert_id' = 'culvert_id')
# # )
#
# # the names must have changed so lets use the file in the mergin project as we know that one is the same
# moti_raw <- sf::st_read('../../gis/mergin/bcfishpass_skeena_20220823/clipped_moti_culverts_sp.gpkg') %>%
#   sf::st_drop_geometry()
#
# moti <- left_join(
#   form_prep,
#
#   moti_raw %>% select(culvert_id, chris_culvert_id),
#
#   by = c('mot_culvert_id' = 'culvert_id')
# )
#
# # burn to a csv
# moti %>%
#
#
#   #sort data by date
#   arrange(date) %>%
#
#   readr::write_csv(paste0(
#     'data/dff/form_pscis_moti_',
#     format(lubridate::now(), "%Y%m%d"),
#     '.csv'))

