combine_two_uq_fac_strings <- function(string1, string2){
    list_1 <- string1 %>%
        stringr::str_split("__") %>%
        unlist()

    list_2 <- string2 %>%
        stringr::str_split("__") %>%
        unlist()

    list_3 <- c(list_1, list_2) %>%
        stringr::str_trim() %>%
        stringr::str_sort() %>%
        unique() %>%
        paste0(collapse="__") %>%
        stringr::str_replace("__$|^__", "")
}
