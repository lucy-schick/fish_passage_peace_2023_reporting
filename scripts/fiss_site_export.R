fiss_raw <- fpr_sp_gpkg_backup(
  path_gpkg = "~/Projects/gis/sern_peace_fwcp_2023/data_field/2023/form_fiss_site_2023.gpkg",
  update_utm = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  write_to_rdata = TRUE,
  return_object = TRUE,
  # these are passed to fpr_sp_assign_utm within the function
  col_easting = "utm_easting",
  col_northing = "utm_northing",
  sig_dig = 0
)
