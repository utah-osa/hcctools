remove_fac_in_surg <- function(uq_surg, uq_fac) {
    surg_comp <- uq_surg %>%
        stringr::str_split("__") %>%
        unlist() %>%
        purrr::keep(!(stringr::str_detect(., fac_str))) %>%
        paste(collapse = "__")

    fac_comp <- uq_surg %>%
        stringr::str_split("__") %>%
        unlist() %>%
        purrr::keep(stringr::str_detect(., fac_str)) %>%
        paste(collapse = "__")

    result <-
        paste0("0surg_comp:", surg_comp, " fac_comp:", fac_comp)
}
