

tdff_photo_names_find <-  function(path_form = "../../gis/sern_peace_fwcp_2023/data_field/2023/form_fiss_site_2023.gpkg",
                                   col_string_add = FALSE,
                                   col_string_append = location){ # make a dataframe ready to rename photos with

  dat  <- sf::st_read(dsn=  path_form)

  dat2 <- dat %>%
    tidyr::pivot_longer(
      # don't pivot the photo tag names though
      cols = starts_with('photo_') & !contains('tag'),
      values_to = 'photo_og',
      names_to = 'photo_renamed',
      cols_vary = 'slowest') %>%
    dplyr::filter(!is.na(photo_og)) %>%

    # ------below is for testing if no tag---------------
  # select(site_id, crew_members, mergin_user, contains('photo')) %>%
  #   add_row(site_id = 12345, photo_renamed = 'photo_extra1', photo_extra1_tag = NA_character_,  photo_og = '12345.jpg') %>%
  # ------above is for testing if no tag---------------

  dplyr::mutate(photo_renamed = dplyr::case_when(stringr::str_detect(photo_renamed,'photo_extra') &
                                                   if_all(contains('photo_extra'), is.na) ~
                                                   'untagged', T ~
                                                   photo_renamed),

                # below needs to be generalized so we can have any number of "photo_extra#" columns and they are tagged accordingly.
                photo_renamed = dplyr::case_when(stringr::str_detect(photo_renamed, 'photo_extra1') ~
                                                   janitor::make_clean_names(photo_extra1_tag, allow_dupes = T, sep_out = ''),
                                                 stringr::str_detect(photo_renamed, 'photo_extra2') ~
                                                   janitor::make_clean_names(photo_extra2_tag, allow_dupes = T, sep_out = ''),
                                                 T ~ photo_renamed),
                photo_renamed = stringr::str_replace_all(photo_renamed, 'photo_', ''))
  # dplyr::distinct(photo_renamed) %>%
  # dplyr::pull(photo_renamed)

  if(col_string_add){
    dat3 <- dat2 %>%
      dplyr::mutate(photo_renamed = paste0(
        {{ col_string_append }},
        '_',
        photo_renamed))
  }else dat3 <-  dat2
}

t <- tdff_photo_names_find(path_form = '../../gis/sern_peace_fwcp_2023/data_field/2023/form_fiss_site_2023.gpkg')

t <- tdff_photo_names_find(col_string_add = T) %>%
  select(site_id, location, contains('photo')) %>%
  distinct(photo_renamed) %>%
  mutate(order = NA_integer_) %>%
  select(photo_renamed, order)
#



