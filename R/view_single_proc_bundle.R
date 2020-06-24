view_single_proc_bundle <- function(df){
    df %>% dplyr::select(
        `Bundled Procedure Code List` = pc_list,
        `Provider List` = pr_list,
        `Procedure Frequency` = vt_cnt,
        `Median Price`=vt_med,
        `Minimum Price`=vt_min,
        `Maximum Price`=vt_max,

        `Anesthesia - Procedure Code List` = anes_bundle,
        `Anesthesia - Provider List` = anes_bundle_prs,
        `Anesthesia - Procedure Code Descriptions` = anes_bun_descr,
        `Anesthesia - Median Price` = anes_bun_med,

        `Facility - Procedure Code List` = fac_bundle,
        `Facility - Provider List` = fac_bundle_prs,
        `Facility - Procedure Code Descriptions` = fac_bun_descr,
        `Facility - Median Price` = fac_bun_med,

        `Medicine - Procedure Code List` = medi_bundle,
        `Medicine - Provider List` = medi_bundle_prs,
        `Medicine - Procedure Code Descriptions` = medi_bun_descr,
        `Medicine - Median Price` = medi_bun_med,

        `Pathology - Procedure Code List` = path_bundle,
        `Pathology - Provider List` = path_bundle_prs,
        `Pathology - Procedure Code Descriptions` = path_bun_descr,
        `Pathology - Median Price` = path_bun_med,

        `Radiology - Procedure Code List` = radi_bundle,
        `Radiology - Provider List` = radi_bundle_prs,
        `Radiology - Procedure Code Descriptions` = radi_bun_descr,
        `Radiology - Median Price` = radi_bun_med,

        `Surgery - Procedure Code List` = surg_bundle,
        `Surgery - Provider List` = surg_bundle_prs,
        `Surgery - Procedure Code Descriptions` = surg_bun_descr,
        `Surgery - Median Price` = surg_bun_med,



    ) %>%
        t() %>%
        knitr::kable(align = 'l', booktabs = TRUE, format = "html") %>%
        kableExtra::kable_styling(latex_options = "striped", font_size = 9) %>%
        #row_spec(1, bold = TRUE, italic = TRUE) #%>%
        kableExtra::column_spec(1, bold = T, border_right = T, extra_css = "text-align: right;") %>%
        kableExtra::column_spec(2, bold = F, border_right = F, extra_css = "text-align: left;") %>%
        kableExtra::row_spec(1:6, color = "white", background = "#666") %>%
        kableExtra::row_spec(7:10, color = "white", background = "#f3476f") %>%
        kableExtra::row_spec(11:14, color = "white", background = "#e86a33") %>%
        kableExtra::row_spec(15:18, color = "white", background = "#e0a426") %>%
        kableExtra::row_spec(19:22, color = "white", background = "#77bf45") %>%
        kableExtra::row_spec(23:26, color = "white", background = "#617ff7") %>%
        kableExtra::row_spec(27:30, color = "white", background = "#a974ee") %>%
        kableExtra::pack_rows("Full Bundled Procedure", 1, 6,
                  label_row_css = "background-color: #666; color: #fff; font-size: 1.6em;") %>%
        kableExtra::pack_rows("Anesthesia", 7, 10,
                  label_row_css = "background-color: #f3476f; color: #fff; font-size: 1.6em;") %>%
        kableExtra::pack_rows("Facility and Evaluation", 11, 14,
                  label_row_css = "background-color: #e86a33; color: #fff; font-size: 1.6em;") %>%
        kableExtra::pack_rows("Medicine Procedures", 15, 18,
                  label_row_css = "background-color: #e0a426; color: #fff; font-size: 1.6em;") %>%
        kableExtra::pack_rows("Pathology Services", 19, 22,
                  label_row_css = "background-color: #77bf45; color: #fff; font-size: 1.6em;") %>%
        kableExtra::pack_rows("Radiology Services", 23, 26,
                  label_row_css = "background-color: #617ff7; color: #fff; font-size: 1.6em;") %>%
        kableExtra::pack_rows("Surgery Prcedures", 27, 30,
                  label_row_css = "background-color: #a974ee; color: #fff; font-size: 1.6em;")
    # row_spec(4, underline = TRUE, monospace = TRUE) %>%
    # row_spec(5, angle = 45) %>%
    # column_spec(5, strikeout = TRUE)
}
