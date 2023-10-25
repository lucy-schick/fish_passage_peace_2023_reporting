# read in the gps
# see the
sf::st_layers('/Volumes/GARMIN/Garmin/GPX/Current/Current.gpx')

list.files('/Volumes/GARMIN/Garmin/GPX/Current/')

t <- sf::st_read('/Volumes/GARMIN/Garmin/GPX/Waypoints.gpx')

sf::st_layers('/Volumes/GARMIN/Garmin/GPX/Waypoints_17-AUG-23.gpx')

t <- sf::st_read('/Volumes/GARMIN/Garmin/GPX/Waypoints_17-AUG-23.gpx',
                 layer = 'waypoints')
t <- sf::st_read('/Volumes/GARMIN/Garmin/GPX/Current/Current.gpx',
                 layer = 'waypoints')


# ------------------------waypoints --------------------------------------------
# read all the gpx on the unit at the same time - NOTE - there are no waypoints in the Current directory but there are tracks
filestoread <- list.files('/Volumes/GARMIN/Garmin/GPX/',
                          pattern = '.gpx',
                          full.names = T)


wp_all <- filestoread %>%
  purrr::map(sf::st_read, layer = 'waypoints') %>%
  dplyr::bind_rows() %>%
  # make a unique ID for the waypoint name
  dplyr::mutate(name = paste0('ai_', name)) %>%
  arrange(time)

# burn all to backups
wp_all %>%
  sf::st_write('/Volumes/backup_2022/backups/new_graph/archive/archive/GIS/gps_backups/backup_wp_ai_20231024.gpx',
               dataset_options="GPX_USE_EXTENSIONS=yes",
               delete_dsn = TRUE)

# burn 2023 to onedrive
wp_all %>%
  # check to see that the first waypoint matches field cards - yes
  dplyr::filter(time > '2023-08-25') %>%
  sf::st_write('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/waypoints_field_2023_al.gpx',
               dataset_options="GPX_USE_EXTENSIONS=yes",
               delete_dsn = TRUE)

# see the dates
wp_all %>%
  dplyr::pull(time)


## ----------- peace -----------------
wp_peace <- wp_all %>%
  dplyr::filter(time > '2023-08-25') %>%
  # check to see if this date match the field cards - yes
  dplyr::filter(time < '2023-09-12')

# burn peace to onedrive
wp_peace %>%
  sf::st_write('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_peace_2023_reporting/data/gps/peace_2023_field_waypoints_al.gpx',
               dataset_options="GPX_USE_EXTENSIONS=yes",
               delete_dsn = TRUE)

# burn peace to mergin
wp_peace %>%
  sf::st_write('/Users/airvine/Projects/gis/sern_peace_fwcp_2023/data_field/2023/peace_2023_field_waypoints_al.gpx',
               dataset_options="GPX_USE_EXTENSIONS=yes",
               delete_dsn = TRUE)


## -------------- skeena --------------------------
wp_sk <- wp_all %>%
  dplyr::filter(time > '2023-09-13 00:00:00') %>%
  # check to see if this date match the field cards - yes
  dplyr::filter(time < '2023-09-28')

# burn to onedrive
wp_sk %>%
  sf::st_write('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_skeena_2023_reporting/data/gps/skeena_2023_field_waypoints_al.gpx',
               dataset_options="GPX_USE_EXTENSIONS=yes",
               delete_dsn = TRUE)

# burn to mergin
wp_sk %>%
  sf::st_write('/Users/airvine/Projects/gis/sern_skeena_2023/data_field/2023/skeena_2023_field_waypoints_al.gpx',
               dataset_options="GPX_USE_EXTENSIONS=yes",
               delete_dsn = TRUE)


# --------------------------------- tracks ---------------------------------------
# read in all the tracks and backup
filestoread <- list.files('/Volumes/GARMIN/Garmin/GPX/Archive/',
                          pattern = '.gpx',
                          full.names = T)

tracks_archive <- filestoread %>%
  purrr::map(sf::st_read, layer = 'tracks') %>%
  dplyr::bind_rows()

# get the current track too
filestoread <- list.files('/Volumes/GARMIN/Garmin/GPX/Current/',
                          pattern = '.gpx',
                          full.names = T)

tracks_current <- filestoread %>%
  purrr::map(sf::st_read, layer = 'tracks') %>%
  dplyr::bind_rows()

# combine and tag so we know whos tracks are whos
tracks <- bind_rows(
  tracks_archive,
  tracks_current
) %>%
  # add surveyor tag tag to track
  dplyr::mutate(name = paste0('ai_', name))

# burn to backup
tracks %>%
  sf::st_write('/Volumes/backup_2022/backups/new_graph/archive/archive/GIS/gps_backups/backup_tracks_ai_20231024.gpx',
             dataset_options="GPX_USE_EXTENSIONS=yes",
             delete_dsn = TRUE)






