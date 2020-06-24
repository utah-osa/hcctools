
get_t_statistic_cat <- function(df) {
    med_sd <- df %>% dplyr::pull(vt_med) %>% sd()

    med_avg <- df %>%  dplyr::pull(vt_med) %>% mean()

    med_n <- df %>%  dplyr::pull(vt_med) %>% length()

    df <-
        df %>%
        dplyr::mutate(t_stat = (med_avg - vt_med) / (med_sd / sqrt(med_n))) %>%
        dplyr::mutate(abs_t_stat = abs(t_stat)) %>%
        dplyr::mutate(pvalue = 2 * pt(-abs_t_stat, df = med_n - 1)) %>%
        dplyr::mutate(
            bundle_group =  dplyr::case_when(
                pvalue < .000001 & t_stat > 0 ~ "above avg" ,
                pvalue < .000001 & t_stat < 0 ~ "below avg" ,
                TRUE ~ "avg"
            )
        )
}
