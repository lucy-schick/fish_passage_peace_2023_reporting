#---------------------pscis export only--------------------------

# read in cleaned form from Q after review and finalization,
# create csv that can be used to paste special into the PSCIS submission spreadsheet


# tfpr_xref_template_pscis <- function(...){
#   template_prep<- fpr::fpr_xref_pscis %>%
#     dplyr::filter(!is.na(spdsht))
#
#   d_num = template_prep %>%
#     dplyr::filter(type_readxl == 'numeric') %>%
#     dplyr::pull(spdsht)
#
#   template <- template_prep %>%
#     dplyr::select(spdsht) %>%
#     tidyr::pivot_wider(names_from = spdsht, values_from = spdsht) %>%
#     dplyr::slice(0) %>%
#     # https://stackoverflow.com/questions/65921972/convert-a-vector-of-character-strings-as-symbols
#     dplyr::mutate(dplyr::across(c(!!! rlang::syms(d_num)), ~ as.numeric(.x)))
# }
#
# t <- tfpr_xref_template_pscis()




# gpkg_path = '../../gis/sern_peace_fwcp_2023/data_field/2023/form_pscis_2023.gpkg'
gpkg_path = '../../gis/sern_skeena_2023/background_layers.gpkg'
my_form <- sf::st_read(dsn = gpkg_path, layer = 'crossings')


test_mixed <- my_form %>%
  dplyr::group_split(utm_zone) %>%
  purrr::map(dplyr::slice, 1:2) %>%
  dplyr::bind_rows()

t_split <- split(test_mixed, seq(1:nrow(test_mixed))) %>%
  bind_rows() %>%
  select(contains(c('aggregated_crossings_id','utm', 'easting', 'northing')))


t_gsplit <- test_mixed %>%
  group_by(utm_zone, aggregated_crossings_id) %>%
  group_split() %>%
  bind_rows() %>%
  select(contains(c('aggregated_crossings_id','utm', 'easting', 'northing')))



# here is a test
my_form2 <- fpr::fpr_sp_utm_assign(form = my_form %>%  slice(1:10),
                               id = aggregated_crossings_id,
                               col_zone = 'utm_zone2',
                               col_easting = 'utm_easting2',
                               col_northing = 'utm_northing2') %>%
  dplyr::select(contains(c('aggregated_crossings_id','zone', 'easting', 'northing'))) %>%
  dplyr::mutate(chk_east = abs(utm_easting - utm_easting2),
         chk_nor = abs(utm_northing - utm_northing2),
         chk_zone = abs(utm_zone - utm_zone2)) %>%
  # filter(chk_east < 1)
  dplyr::filter(if_any(contains('chk'), ~ .x <= 1))

t_mixed <- my_form2 %>%
  bind_rows() %>%
  select(contains(c('aggregated_crossings_id','utm', 'easting', 'northing')))

my_form2 <- tdff_sp_utm_assign(form = test9, crs_output = 4326, id = aggregated_crossings_id)

t09 <- my_form2 %>%
  bind_rows() %>%
  select(contains(c('aggregated_crossings_id','utm', 'easting', 'northing')))

my_form2 <- tdff_sp_utm_assign(form = test9, crs_output = 4326, id = aggregated_crossings_id)


#######################
# OK THIS WORKS WHILE map2 did not due to sort order issue in the inputs created by group_split
df <- test_mixed %>%
  mutate(epsg = 26900 + utm_zone)

# https://stackoverflow.com/questions/49181715/how-to-make-a-data-frame-into-a-simple-features-data-frame
l <- lapply(unique(df$aggregated_crossings_id), function(x){
  df <- df[df$aggregated_crossings_id == x,]
  epsg <- df$epsg[1]
  df <- st_transform(df, crs = epsg)
  df <- df %>%
    mutate(
      easting2 = sf::st_coordinates(.)[,1],
      northing2 = sf::st_coordinates(.)[,2])
})

try_this <- do.call(rbind, lapply(l, function(x) x <- st_transform(x, 4326))) %>%
  select(contains(c('aggregated_crossings_id','utm', 'easting', 'northing')))

####--------------------------------

# # this is a table that cross references column names for pscis table and has the columns in the same order as the spreadsheet
# xref_names_pscis <- fpr::fpr_xref_pscis
#
# # get order of columns as per the excel template spreadsheet
# # this can be used as a select(all_of(name_pscis_sprd_ordered)) later
# # # to order columns for the field form and/or put the field entered table in order
# name_pscis_sprd_ordered <- fpr::fpr_xref_pscis %>%
#   filter(!is.na(spdsht)) %>%
#   select(spdsht) %>%
#   pull(spdsht)



#
# # see names that coincide between the xref table and what we have
# intersect(name_pscis_sprd_ordered, names(form_pscis))
#
# # see which are different
# setdiff(name_pscis_sprd_ordered, names(form_pscis))
# # order matters
# setdiff(names(form_pscis), name_pscis_sprd_ordered)

# to use all the columns from the template first we make an empty dataframe from a template
template <- fpr::fpr_import_pscis() %>%
  slice(0)

# then we join it to our populated spreadsheet
# we may as well keep all the columns that are not in the spreadsheet and append to the end
form <- bind_rows(
  template,
  form_pscis
) %>%
  # only select columns from template object
  select(any_of(names(template))) %>%
  # remove scoring columns, as these can't be copied and pasted anyways because of macros
  select(-stream_width_ratio:-barrier_result) %>%
  # then arrange it by pscis id to separate phase 1s from reassessments
  arrange(pscis_crossing_id, date)


# burn to a csv ready for copy and paste to template
form %>% readr::write_csv(paste0(
  'data/dff/pscis_export.csv'), na='')

