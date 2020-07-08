remove_surg_from_fac <- function(new_surg, uq_fac){

    new_surg_str <- new_surg %>% stringr::str_split("__") %>% paste0(collapse="|")

    uq_fac %>% stringr::str_split("__") %>%
        unlist() %>%
        purrr::keep(
            !(stringr::str_detect(.,new_surg_str))
        ) %>%
        paste(collapse="__")
}
