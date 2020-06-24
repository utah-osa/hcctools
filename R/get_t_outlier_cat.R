get_t_outlier_cat <- function(df) {

    vt_summary <- df$vt_med %>% summary()
    Q1 <- vt_summary[2]
    Q3 <- vt_summary[5]
    IQR <- Q3 - Q1

    df %>%
        dplyr::mutate(
            bundle_group = dplyr::case_when(
                vt_med > Q3 + 3 * IQR ~ "above avg",
                vt_med < Q1 - 3 * IQR ~ "below avg",
                TRUE ~ "avg",
            )
        )

}
