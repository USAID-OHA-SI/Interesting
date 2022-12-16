# PEPFAR Countries & Levels

pepfar_countries <- grabr::get_outable(
  username = glamr::datim_user(),
  password = glamr::datim_pwd()
)

pepfar_countries <- pepfar_countries |>
  dplyr::select(-ends_with(c("_uid", "_iso"))) |>
  dplyr::mutate(ou_country = dplyr::case_when(
    operatingunit != country ~ paste0(operatingunit, "/", country),
    TRUE ~ operatingunit
  )) |>
  dplyr::relocate(ou_country, .before = 1)

usethis::use_data(pepfar_countries, overwrite = TRUE)

