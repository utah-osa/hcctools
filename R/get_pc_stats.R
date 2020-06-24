get_pc_stats <- function(df){
    df %>% dplyr::pull(pc_list) %>%
        stringr::str_split("_") %>%
        unlist() %>%
        tibble::enframe() %>%
        dplyr::select(cpt_code = value) %>%
        dplyr::group_by(cpt_code) %>%
        dplyr::summarize(n=dplyr::n()) %>%
        dplyr::arrange( dplyr::desc(n))
}
