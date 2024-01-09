
df_utm <- fpr::fpr_db_query(query = fpr::fpr_db_q_crossings_utm())


# here is a test
my_df_utm_test <- fpr::fpr_sp_utm_assign(form = df_utm_test,
                                   id = aggregated_crossings_id,
                                   col_zone = 'utm_zone2',
                                   col_easting = 'utm_easting2',
                                   col_northing = 'utm_northing2') %>%
  dplyr::select(contains(c('aggregated_crossings_id','zone', 'easting', 'northing'))) %>%
  dplyr::mutate(chk_east = abs(utm_easting - utm_easting2),
                chk_nor = abs(utm_northing - utm_northing2),
                chk_zone = abs(utm_zone - utm_zone2)) %>%
  # filter(chk_east < 1)
  dplyr::filter(if_any(contains('chk'), ~ .x >= 1))
