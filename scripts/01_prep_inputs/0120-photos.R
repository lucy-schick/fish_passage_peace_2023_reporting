# grab the files from mergin and move to project using linux cmd
# mv -v ~/Projects/gis/mergin/bcfishpass_elkr_20220904/photos/* ~/Projects/current/2022-056-nupqu-elk-cwf/data/photos/mergin/originals
# mv -v ~/Projects/gis/mergin/bcfishpass_skeena_20220823-v225/photos/* ~/Projects/current/2022-049-sern-skeena-fish-passage/data/photos/mergin/


# make folders ------------------------------------------------------------

# steps
# import form_pscis.gpkg direct from mergin
# filter !is.na(modelled_crossing_id) and pull(modelled_crossing_id)
# filter !is.na(stream_crossing_id) and pull(stream_crossing_id)
# feed list (or lists) of modelled and pscis ids to folderstocreate %>% purrr::map(fpr::fpr_photo_folders)


# 2023 - rename the photos
# read in the forms and join
dir_project <- 'sern_peace_fwcp_2023'

form_pscis1 <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/form_pscis.gpkg'))

form_pscis2 <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/form_pscis_20230825_resaved.gpkg'))


# check to see that column names are equiv (must be if number is the same but still)

identical(names(form_pscis1), names(form_pscis2))

# join and give a site_id
form_pscis <- bind_rows(
  form_pscis1,
  form_pscis2
) %>%
  mutate(
    site_id = case_when(is.na(pscis_crossing_id) ~ my_crossing_reference,
                        T ~ pscis_crossing_id)
  ) %>%
  # remove the bute events as they are duplicates and just for training in 2023
  # sometimes the camera is not defined (noticed this in the field) - think that was why redid form (can't remember though)
  filter(camera_id != 'newgraph_bute' | is.na(camera_id)) %>%
  # remove the form making site
  filter(site_id != '12345') %>%
  arrange(site_id)

# check for duplicates
form_pscis %>%
  filter(!is.na(site_id)) %>%
  group_by(site_id) %>%
  filter(n()>1)

# resize the photos and change the extension to JPG for consistency and unforseen issues
# sync to mergin after copying to new dir (resized) and removing originals
# record version number of mergin project in issue for now to track
fpr_photo_resize_batch(
  dir_source = '/Users/airvine/Projects/gis/sern_peace_fwcp_2023/ignore_mobile/photos/',
  dir_target = '/Users/airvine/Projects/gis/sern_peace_fwcp_2023/ignore_mobile/photos_resized/')

# move to onedrive so we don't pay for space on mergin.  should just go direct to onedrive in the future
fpr::fpr_photo_resize_batch(
  dir_source = '/Users/airvine/Projects/gis/sern_peace_fwcp_2023/ignore_mobile/photos_resized/',
  dir_target = '/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_peace_2023_reporting/data/photos/mergin/')


# rename the photos from the FISS cards------------------
# dataframe comes from fiss_site_tidy
fpr::fpr_photo_rename(
  dat = form_fiss_site_raw,
  dir_from_stub = '/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_peace_2023_reporting/data/photos/mergin/',
  dir_to_stub = '/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_peace_2023_reporting/data/photos/sorted/',
  col_string_add = TRUE,
  col_string_append = location)

# remove all the duplicated photos on onedrive that were renamed.
photos_dry_run_before <- fpr::fpr_photo_remove_dupes('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_peace_2023_reporting/data/photos/sorted/')
photos_dry_run3_before <- fpr::fpr_photo_remove_dupes('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_peace_2023_reporting/data/photos/sorted/',
                                               min_replicates = 3)


# actually run the removal of the first un-renamed photo
# fpr::fpr_photo_remove_dupes('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_peace_2023_reporting/data/photos/sorted/',
#                             dry_run = F)

photos_dry_run_after <- fpr::fpr_photo_remove_dupes('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_peace_2023_reporting/data/photos/sorted/')
photos_dry_run3_after <- fpr::fpr_photo_remove_dupes('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_peace_2023_reporting/data/photos/sorted/',
                                                      min_replicates = 3)

save(photos_dry_run_before,
     photos_dry_run3_before,
     photos_dry_run_after,
     photos_dry_run3_after,
     file = "data/inputs_extracted/photos_dry_run.RData")


# rotate photos -------------------------------------------------------------
fpr_photo_rotate(site_id = 6543, str_to_pull = '20230830_144102_barrel')
file.remove('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/peace/photos/6543/20230830_144102_barrel.jpg')



fpr::fpr_photo_flip(site_id = 14000988, rotate = 270, str_to_pull = '5587')
fpr::fpr_photo_flip(site_id = 14000994, rotate = 270, str_to_pull = '00938')
fpr::fpr_photo_flip(site_id = 14000997, rotate = 270, str_to_pull = '5578')
fpr::fpr_photo_flip(site_id = 2021090399, rotate = 270, str_to_pull = '5756')

# QA photo files ----------------------------------------------------------
fpr::fpr_photo_qa_df(dir_photos = '/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/peace/photos/')



# build photo amalgamation for each site ------------------------------------------------
# get a list of sites to burn
sites_l <- fpr::fpr_import_pscis_all() %>%
  bind_rows() %>%
  distinct(site_id) %>%
  arrange(site_id) %>%
  pull(site_id)

# burn the amal photos to onedrive
sites_l %>%
  purrr::map(fpr::fpr_photo_amalg_cv, dir_photos = '/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/peace/photos/')
fpr::fpr_photo_amalg_cv(site_id = 3700015, dir_photos = '/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/peace/photos/')

# Find sites that have directories but do not have an entry in the PSCIS spreadsheets
dir_photos <- '/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/peace/photos/'
setdiff(
  list.dirs(dir_photos, full.names = F, recursive = F),

  pscis_all %>%
    distinct(site_id) %>%
    arrange(site_id) %>%
    # head() %>% #test
    pull(site_id)
)


# make phase2 photo files and copy in photos ------------------------------

# because we did not have the pscis ids our photos for phase 2 sites that had a my_crossing_reference
# need to be copied into new folders

##path to the photos
path_photos <- '/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/peace/photos/'


##use the pscis spreadsheet to make the folders to copy the photos to
# d <- import_pscis(workbook_name = 'pscis_phase1.xlsm')
d <- fpr::fpr_import_pscis(workbook_name = 'pscis_phase2.xlsm')

conn <- rws_connect("data/bcfishpass.sqlite")
xref_pscis_my_crossing_modelled <- readwritesqlite::rws_read_table("xref_pscis_my_crossing_modelled", conn = conn)
rws_disconnect(conn)

pscis_new_sites <- left_join(
  d,
  xref_pscis_my_crossing_modelled,
  by = c('pscis_crossing_id' = 'stream_crossing_id')
) %>%
  filter(!is.na(external_crossing_reference))

folderstocopy<- pscis_new_sites$external_crossing_reference %>% as.character()

folders_new_names <- pscis_new_sites$pscis_crossing_id %>% as.character()

path_to_photos <- paste0(path_photos, folderstocopy)

folderstocreate <- paste0(path_photos, folders_new_names)

##create the folders
lapply(folderstocreate, dir.create)


paths_to_copy <- function(target){
  list.files(path = target,
             pattern = ".JPG$",
             recursive = TRUE,
             full.names = T,
             include.dirs = T)
  # stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet')
}

photo_names_to_copy <- function(target){
  list.files(path = target,
             pattern = ".JPG$",
             recursive = TRUE,
             full.names = F,
             include.dirs = T)
  # stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet')
}


filestocopy_list <- path_to_photos %>%
  purrr::map(paths_to_copy)

change_file_names <- function(filestocopy, filename_before, filename_after){
  gsub(filestocopy, pattern = filename_before, replacement = filename_after)
}


filestopaste_list <- mapply(change_file_names, filestocopy_list, folderstocopy, folders_new_names)

copy_over_photos <- function(filescopy, filespaste){
  file.copy(from=filescopy, to=filespaste,
            overwrite = T,
            copy.mode = TRUE)
}

mapply(copy_over_photos, filescopy =  filestocopy_list,
       filespaste = filestopaste_list)



