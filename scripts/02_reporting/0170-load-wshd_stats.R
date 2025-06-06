source('scripts/packages.R')
# source('scripts/tables.R')

# get phase 2 pscis site data from sqlite, only possible after provincial submission and bcfishpass db update
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_list_tables(conn)
bcfishpass_phase2_raw <- readwritesqlite::rws_read_table("bcfishpass", conn = conn)

pscis2 <- fpr::fpr_import_pscis(workbook_name = 'pscis_phase2.xlsm')
###HAAAAAAAAACCCCCCCCCCCCCKKKKKKKKKKKK - we wnat to present results for 124231 so we need to add that to our list of sites
sites_extra <- c(125231, 125261)

bcfishpass_phase2 <- bcfishpass_phase2_raw |>
  dplyr::filter(stream_crossing_id %in% c(pscis2 |> dplyr::pull(pscis_crossing_id), sites_extra)) |>
  dplyr::filter(!is.na(stream_crossing_id)) |>
  # get the elevations
  fpr::fpr_sp_assign_sf_from_utm(col_easting = "utm_easting",
                            col_northing = "utm_northing") |>
  poisspatial::ps_elevation_google(Z = 'elev_site') |>
  dplyr::mutate(elev_site = round(elev_site, 0)) |>
  sf::st_drop_geometry()

##we needed to remove crossings that are first order - this used to run but doesn't want to anymore
##i wonder if it is because the 1st order watershed is the first one on the list so the api kicks us off...
bcfishpass_phase2_clean <- bcfishpass_phase2 |>
   filter(stream_order != 1)

# bcfishpass_phase2 |>
#   filter(stream_order == 1)
# # see console - so far we don't have any first order watersheds so we don't need to do all the next moves!! If we do, we need to walk through it though
# conn <- DBI::dbConnect(
#   RPostgres::Postgres(),
#   dbname = Sys.getenv('PG_DB_SHARE'),
#   host = Sys.getenv('PG_HOST_SHARE'),
#   port = Sys.getenv('PG_PORT_SHARE'),
#   user = Sys.getenv('PG_USER_SHARE'),
#   password = Sys.getenv('PG_PASS_SHARE')
# )
#
# dat <- bcfishpass_phase2 |>
#   filter(stream_order == 1)
#   # distinct(localcode_ltree, .keep_all = T)
# #
# # # add a unique id - we could just use the reference number
# dat$misc_point_id <- seq.int(nrow(dat))
#
# # ##pull out the localcode_ltrees we want
# ids <-  dat |>
#   pull(localcode_ltree) |>
#   # unique() |>
#   as_vector() |>
#   na.omit()
#
# ids2 <- dat |>
#   pull(wscode_ltree) |>
#   # unique() |>
#   as_vector() |>
#   na.omit()
# #
# # # note that we needed to specifiy the order here.  Not sure that is always going to save us....
# sql <- glue::glue_sql(
#
#                                 "SELECT localcode_ltree, wscode_ltree, area_ha, geom as geometry from whse_basemapping.fwa_watersheds_poly
#                                 WHERE localcode_ltree IN ({ids*})
#                                 AND wscode_ltree IN ({ids2*})
#                                 AND watershed_order = 1
#                                 ",
#   .con = conn
# )
#
# wshds_1ord_prep <- sf::st_read(conn,
#                         query = sql) |>
#   st_transform(crs = 4326)
#
# # test <- wshds_1ord_prep |>
# #   filter(localcode_ltree == wscode_ltree)
#
# # # i think we need to be joining in the stream_crossing_id and joining on that....
#
# wshds_1ord <- left_join(
#   dat |>
#     distinct(stream_crossing_id, .keep_all = T) |>
#     select(
#     stream_crossing_id,
#     localcode_ltree,
#     wscode_ltree,
#     stream_order
#     ) |>
#     mutate(stream_crossing_id = as.character(stream_crossing_id)),
#     # filter(stream_order == 1),
#   wshds_1ord_prep |>
#     mutate(localcode_ltree = as.character(localcode_ltree),
#            wscode_ltree = as.character(wscode_ltree)),
#   by = c('localcode_ltree','wscode_ltree')
# )


## call fwapgr to get the watersheds as spatial objects
wshds_fwapgr <- fpr::fpr_sp_watershed(bcfishpass_phase2_clean)

# join first order df to original df
# wshds_prep <- bind_rows(
#   wshds_fwapgr,
#   wshds_1ord
# )

#in this particular script we don't have any first order watersheds so
wshds_prep <- wshds_fwapgr


# calculate stats for each watershed
hab_wshds_raw <- fpr::fpr_sp_wshd_stats(dat = wshds_prep)

# add our elev_min back in and put it
hab_wshds <- left_join(
  hab_wshds_raw |> mutate(stream_crossing_id = as.numeric(stream_crossing_id)),

  bcfishpass_phase2 |>
    st_drop_geometry() %>%
    select(stream_crossing_id, elev_site),

  by = c('stream_crossing_id')) |>
  # put elev_site before elev_min
  dplyr::relocate(elev_site, .before = elev_min)

# make sure we have the correct crs
unique(bcfishpass_phase2$utm_zone)

#add to the geopackage
fpr::fpr_make_geopackage(dat = hab_wshds, utm_zone = 10)


#burn to kml as well so we can see elevations
st_write(hab_wshds |>
           rename(name = stream_crossing_id),
         append = F,
         delete_layer = T,
         driver = 'kml',
         dsn = "data/inputs_extracted/wshds.kml")


conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_list_tables(conn)
readwritesqlite::rws_drop_table("wshds", conn = conn) ##now drop the table so you can replace it
readwritesqlite::rws_write(hab_wshds, exists = F, delete = TRUE,
          conn = conn, x_name = "wshds")
readwritesqlite::rws_list_tables(conn)
readwritesqlite::rws_disconnect(conn)
