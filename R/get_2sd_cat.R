get_2sd_cat <- function(df) {
    med_sd <- df %>%
        dplyr::pull(vt_med) %>%
        sd()

    med_avg <- df %>%
        dplyr::pull(vt_med) %>%
        mean()


    df <- df %>%
        dplyr::mutate(
            bundle_group =
                dplyr::case_when(
                    vt_med < med_avg + 2 * med_sd & vt_med > med_avg - 2 * med_sd ~ "avg",
                    vt_med > med_avg + 2 * med_sd  ~ "above avg",
                    vt_med < med_avg - 2 * med_sd  ~ "below avg",
                    vt_med == 0 ~ "below avg"
                )
        )
}
