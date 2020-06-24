get_tag_cor <- function(df) {
    cor_data <-
        df %>%
        dplyr::select_if( ~ is.numeric(.) == TRUE | is.logical(.) == TRUE)

    temp <- stats::cor(cor_data)

    temp_row1 <- temp[1, ] %>%
        tibble::enframe() %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::mutate(value = abs(value)) %>%
        dplyr::arrange(desc(value)) %>%
        dplyr::filter(stringr::str_detect(name, "^vt_") == FALSE) %>%
        dplyr::filter((!stringr::str_detect(name, "bun_med")) &
                   (!stringr::str_detect(name, "bun_min")) &
                   (!stringr::str_detect(name, "bun_avg")) &
                   (!stringr::str_detect(name, "bun_max")) &
                   (!stringr::str_detect(name, "bun_sum"))) %>%
        dplyr::rename("correlation" = "value") %>%
        dplyr::mutate(correlation = round(correlation, 4))
}
