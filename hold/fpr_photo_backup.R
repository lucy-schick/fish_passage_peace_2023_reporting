# temporary dump to here while developing fpr_photo

#' Helper function to QA Photos
#'
#' Prep for fpr_photo_qa which will QA photos. Ensure that there are 6 named photos and no duplicates named.
#' Duplicates do not just contain the keyword but also a preceding underscore and proceeding period (ie. `_keyword.`).
#' Used inside of
#'
#' @param site_id Numeric value of site corresponding to folder name
#'
#' @return
#' @export
#'
#' @examples
fpr_photo_qa_prep <- function(site_id){
  list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = F) %>%
    stringr::str_subset(., '_barrel.|_outlet.|_upstream.|_downstream.|_road.|_inlet.') %>%
    as_tibble() %>%
    mutate(x = case_when(
      value %ilike% 'road' ~ 'road',
      value %ilike% 'inlet' ~ 'inlet',
      value %ilike% 'upstream' ~ 'upstream',
      value %ilike% 'barrel' ~ 'barrel',
      value %ilike% 'outlet' ~ 'outlet',
      value %ilike% 'downstream' ~ 'downstream'
    )) %>%
    tidyr::pivot_wider(names_from = x) %>%
    dplyr::mutate(site = site_id)
}

#' Helper function to QA Photos
#'
#' Ensure that there are 6 named photos and no duplicates named.  Uses \link{fpr_photo_qa_prep}.
#' Used inside of \link{fpr_photo_qa_df}.
#'
#'
#' @param dat Dataframe to pull site IDs from that coincide with photo folders. Defaults to pscis_all
#' @param col Column to pull to get site IDs. Defaults to `site_id`
#'
#' @return List of tibbles for each sites.
#' @export
#'
#' @examples
fpr_photo_qa <- function(dat = pscis_all,
                         col = site_id){
  dat %>%
    dplyr::arrange({{col}}) %>%
    dplyr::pull({{col}}) %>%
    purrr::map(fpr_photo_qa_prep) %>%
    purrr::set_names(
      nm = dat %>%
        dplyr::arrange({{col}}) %>%
        dplyr::pull({{col}})
    )
}


#' QA photos to see if all required PSCIS photos are named and look for duplicates of those names.
#'
#' Produce data frame showing missing photos and sites with duplicates. Old way of doing it documented here.
#' Uses helper functions \link{fpr_photo_qa_prep} and \link{fpr_photo_qa}
#'
#'
#'
#'#' find sites with 0 Rows.
#' fpr::fpr_photo_qa()[fpr::fpr_photo_qa() %>%
#' map(fpr::fpr_dat_w_rows) %>%
#' grep(pattern = F) ] %>%
#' names(.) %>%
#' unique(.)
#'
#'
#' See and fix duplicates with
#' fpr::fpr_photo_qa() %>%
#' data.table::rbindlist(fill = T)
#'
#' After dups are fixed then Query for missing values with
#' dplyr::bind_rows() %>%
#' dplyr::filter(dplyr::if_any(dplr::everything(), is.na))
#'
#' @return
#' @export
#'
#' @examples
fpr_photo_qa_df <- function(){

  dat_photo <- fpr::fpr_import_pscis_all() %>%
    dplyr::bind_rows() %>%
    fpr::fpr_photo_qa()

  dplyr::bind_rows(

    dat_photo %>%
      purrr::map(dplyr::group_by, site) %>%
      purrr::map(summarise_all, class) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(if_any(everything(), is.na)),

    dat_photo %>%
      purrr::map(dplyr::group_by, site) %>%
      purrr::map(summarise_all, class) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(if_any(everything(), ~ . == 'list'))
  )
}
