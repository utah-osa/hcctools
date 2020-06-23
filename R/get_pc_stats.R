get_pc_stats <- function(df){
    df %>% pull(pc_list) %>%
        str_split("_") %>%
        unlist() %>%
        enframe() %>%
        select(cpt_code = value) %>%
        group_by(cpt_code) %>%
        summarize(n=n()) %>%
        arrange( desc(n))
}
