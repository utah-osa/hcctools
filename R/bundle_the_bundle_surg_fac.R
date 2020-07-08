bundle_the_bundle_surg_fac <- function(df){
    df <- df  %>%
        rowwise() %>%
        dplyr::mutate(
            uq_surg = surg_bundle_prs %>%
                stringr::str_split(",") %>%
                unlist() %>%
                sort() %>%
                stringr::str_trim() %>%
                unique() %>%
                paste0(collapse = "__"),
            uq_fac = fac_bundle_prs %>%
                stringr::str_split(",") %>%
                unlist() %>%
                sort() %>%
                stringr::str_trim() %>%
                unique() %>%
                paste0(collapse = "__")
        ) %>%
        dplyr::mutate(fac_from_surg = remove_fac_in_surg(uq_surg,uq_fac)) %>%
        dplyr::mutate(new_surg = stringr::str_extract(fac_from_surg,
                                                      "(?<=0surg_comp:)[[:graph:]|[:space:]]+ (?=fac_comp:)"),
                      new_fac = stringr::str_extract(fac_from_surg,
                                                     "(?<=fac_comp:)[[:graph:]|[:space:]]+")) %>%

        dplyr::mutate(uq_fac_2 = remove_surg_from_fac(new_surg, uq_fac)) %>%
        dplyr::mutate(uq_fac_final = dplyr::case_when(
            is.na(new_fac) & !is.na(uq_fac_2) ~ uq_fac_2,
            !is.na(new_fac) & is.na(uq_fac_2) ~ new_fac,
            is.na(new_fac) & is.na(uq_fac_2) ~ "",
            new_fac == uq_fac_2 ~ new_fac,
            new_fac != uq_fac_2 ~ combine_two_uq_fac_strings(new_fac,uq_fac_2)



        )
        ) %>%
        dplyr::mutate(uq_fac_final = dplyr::if_else(is.na(uq_fac_final) | uq_fac_final=="", "Not Specified", uq_fac_final),
                      new_surg = dplyr::if_else(is.na(new_surg) | new_surg=="", "Not Specified", new_surg)) %>%
        dplyr::mutate(uq_fac_final = stringr::str_trim(uq_fac_final),
                      new_surg  = stringr::str_trim(new_surg)) %>%
        dplyr::mutate(same_surg_fac = dplyr::if_else(new_surg == uq_fac_final, TRUE,FALSE)) %>%

        dplyr::mutate(uq_fac_final = dplyr::if_else(same_surg_fac==TRUE, "",uq_fac_final)) %>%

        #select(same_surg_fac,new_surg, uq_fac_final,new_fac, uq_fac_2, uq_surg,uq_fac)
        dplyr::group_by(new_surg,uq_fac_final) %>%
        dplyr::summarize(num_bun_type = n(),
                         sum_bun_type = sum(vt_cnt),
                         bb_vt_med_mean= mean(vt_med)

        ) %>%
        dplyr::mutate(pr_pc_pair = paste0("Surgeon(s):",new_surg,
                                          "\n Facility:",uq_fac_final#,
                                          #"\n Possible CPT(s):", possible_buns
        )
        ) %>%
        as.data.frame() %>%
        dplyr::group_by(new_surg, uq_fac_final) %>%
        dplyr::summarize(estimated_price = mean(bb_vt_med_mean)) %>%
        mutate(estimated_price = round(estimated_price,2)) %>%
        dplyr::arrange(new_surg, uq_fac_final) %>%

        # dplyr::mutate(estimated_price = paste0("$",
        #                                        format(
        #                                            round(estimated_price,2),
        #                                            big.interval = ","
        #                                        )
        # )
        #) %>%
        dplyr::select(`Doctor` = new_surg,
                      `Facility` = uq_fac_final,
                      `Estimated Price`=estimated_price)
}

