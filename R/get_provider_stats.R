get_provider_stats <- function(df, provider_str = "") {
    # Get the unique providers in a given bundle procedure.
    temp <- df %>%
        #slice(1:5)%>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            uq_pr = pr_list %>%
                stringr::str_split("_") %>%
                unlist() %>%
                sort() %>%
                unique() %>%
                paste0(collapse = "|||")
        ) %>%
        splitstackshape::cSplit('uq_pr', '|||')

    # for a given bundled procedure's  'providers bundle' calculate relevant statistics.
    #
    temp2 <- temp %>%
        dplyr::group_by_(.dots = names(temp)[dplyr::matches("uq_pr_", vars = names(temp))]) %>%
        dplyr::summarize(
            n = dplyr::n(),
            vt_med_med = median(vt_med),
            vt_med_min = min(vt_med),
            vt_med_max = max(vt_med),
            vt_med_mean = mean(vt_med),
            vt_sum_sum  = sum(vt_sum),
            num_surg = sum(vt_cnt)
        ) %>% dplyr::arrange(dplyr::desc(num_surg), dplyr::desc(n)) %>%
        tidyr::unite("pr_combo",
              names(temp)[dplyr::matches("uq_pr_", vars = names(temp))],
              na.rm = TRUE,
              sep = " ||| ")
    if (provider_str != "") {
        result <- temp2 %>% filter(pr_combo %>% stringr::str_detect(provider_str))
    } else{
        result <- temp2
    }
    result

}
