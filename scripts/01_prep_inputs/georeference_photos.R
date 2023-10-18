stub_from <- 'C:/Users/matwi/OneDrive'
name_repo <- 'fish_passage_peace_2022_reporting'


# read in Mateo gpx file
gpx <- paste0(stub_from, '/Projects/repo/', name_repo,
       '/data/gps/', 'parsnip_2022_field_mw.GPX')

track_points_prep = read_sf(gpx, layer = "track_points")

# add lon, lat to our track points
track_points_mw <- track_points_prep %>%
    st_coordinates() %>%
    as_tibble() %>%
    setNames(c("gps_longitude","gps_latitude")) %>%
    rowid_to_column() #%>%
    # mutate(time = lubridate::as_datetime(time, tz="America/Vancouver")) ##put them in the same tz

# import Mateo photos

# path to photos on OneDrive
path <- paste0(stub_from, '/Projects/repo/', name_repo,
               '/data/photos/', 'MW')

photo_metadata <- exifr::read_exif(path,recursive=T)  %>%
  janitor::clean_names() %>%
  #filter timestamp photos
  filter(file_name %like% 'TimePhoto') %>%
  select(file_name, source_file, create_date) %>%
  mutate(create_date = lubridate::as_datetime(create_date, tz="America/Vancouver"))

# import Al photos
path2 <- paste0(stub_from, '/Projects/repo/', name_repo,
                '/data/photos/', 'AI')

photo_metadata2 <- exifr::read_exif(path2,recursive=T)  %>%
  janitor::clean_names() %>%
  select(file_name, source_file, create_date, gps_latitude, gps_longitude) %>%
  mutate(create_date = lubridate::as_datetime(create_date, tz="America/Vancouver"))


#find the track point that is closest in time to the CreateDate stamp of the photo..
#https://stackoverflow.com/questions/21607247/searching-for-nearest-date-in-data-frame
get_closest_line_in_history <- function(x, history){
  time_diffs <- difftime(x, history)
  time_diffs[time_diffs<0] <- NA

  res <- which.min(time_diffs)
  if (length(res) != 1){
    return(NA)  ##return a NA as in the post
  }else{
    return(res)
  }
}

indx_closest_point <- sapply(photo_metadata$create_date,
                             get_closest_line_in_history,
                             track_points_prep$time) %>%
                             as_tibble()

# closest point corresponds to row id in track points so join dataframes
joined_tracks <- left_join(indx_closest_point, track_points_mw, by = c('value' = 'rowid'))

# tracks are now matched up to photos indexes so bind columns and drop value column
photo_metadata_processed <- bind_cols(photo_metadata, joined_tracks) %>%
                            select(-value) %>%
                            bind_rows(photo_metadata2)

##write to a csv
write.csv(photo_metadata_processed, file = 'data/inputs_extracted/photo_metadata.csv', row.names = F)







