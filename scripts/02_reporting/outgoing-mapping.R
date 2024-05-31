source('scripts/packages.R')
source('scripts/tables.R')

fpr_make_geopackage(dat = hab_fish_collect)
#still need to do - see https://github.com/NewGraphEnvironment/fish_passage_peace_2023_reporting/issues/53
# fpr_make_geopackage(dat = hab_features)
fpr_make_geopackage(dat = hab_site_priorities)
fpr_make_geopackage(dat = phase1_priorities)

##we do this manually since the
# phase1_priorities %>%
#   st_transform(crs = 3005) %>%
#   sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'phase1_priorities', delete_layer = TRUE)

# create folder in project for mapping
# dir.create('data/fishpass_mapping')

##add the tracks
sf::read_sf("./data/habitat_confirmation_tracks.gpx", layer = "tracks") %>%
  sf::st_write(paste0("./data/fishpass_mapping/", 'fishpass_mapping', ".gpkg"), 'hab_tracks', append = TRUE)


##here is the study area watersheds
wshd_study_areas <- fpr_db_query(
  query = "SELECT * FROM whse_basemapping.fwa_watershed_groups_poly a
                               WHERE a.watershed_group_code IN ('PARS','CARP','CRKD', 'NATR')"
)

wshd_study_areas %>%
  # select(-wscode_ltree) %>%
  st_cast("POLYGON") %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_write(paste0("./data/fishpass_mapping/", 'fishpass_mapping', ".gpkg"), 'wshd_study_areas', append = F)

####------------add the watersheds-------------------------
# this is done in 0170-load-wshd_stats.R


####--------------------burn geojsons from geopackage-----------------------------------------------------
##we need geojsons to make the mapping convenient so lets pull everything out of the geopackage and write to geojson files
read_gpkg <- function(layers = layer_name){
  sf::st_read(dsn = "./data/fishpass_mapping/fishpass_mapping.gpkg", layer = layers) %>%
    mutate(name = layers)
    # sf::st_transform(crs = 4326)
}

##get the names of the layers you want
layer_names <- sf::st_layers(paste0("./data/fishpass_mapping/", 'fishpass_mapping', ".gpkg")) %>%
  pluck('name')

##grab the layers and give them a name
layers_to_burn <- layer_names %>%
  purrr::map(read_gpkg) %>%
  purrr::set_names(nm = layer_names)

##now burn them to fishpass_mapping folder
write_geojson <- function(layers){
  layers %>%
  geojsonio::geojson_write(file = paste0("./data/fishpass_mapping/", unique(layers$name), ".geojson"))
}

layers_to_burn %>%
  map(write_geojson)
