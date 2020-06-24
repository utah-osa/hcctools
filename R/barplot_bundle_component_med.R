
#' barplot_bundle_component_med
#'
#' @param df
#'
#' @return ggplot
#' @export
#'
#' @examples
#'
barplot_bundle_component_med <- function(df) {
    surg_med <- df$surg_bun_med
    medi_med <- df$medi_bun_med
    radi_med <- df$radi_bun_med
    path_med <- df$path_bun_med
    anes_med <- df$anes_bun_med
    fac_med <- df$fac_bun_med

    component_meds <-
        rbind(surg_med, medi_med, path_med, radi_med, anes_med, fac_med) %>%
        tidyr::replace_na(0) %>%
        as.data.frame()

    component_meds$ID <- as.factor(1:nrow(component_meds))

    # fill_cols <- c(
    #     "Anesthesia" = "#003f5c",
    #     "Facility" = "#444e86",
    #     "Medicine Proc" = "#955196",
    #     "Pathology" = "#dd5182",
    #     "Radiology" = "#ff6e54",
    #     "Surgery" = "#ffa600"
    # )


    melted <- reshape2::melt(component_meds, id = "ID") %>%
        dplyr::mutate(
            ID = dplyr::case_when(
                ID == 1 ~ "Surgery",
                ID == 2 ~ "Medicine Proc",
                ID == 3 ~ "Pathology",
                ID == 4 ~ "Radiology",
                ID == 5 ~ "Anesthesia",
                ID == 6 ~ "Facility"
            )
        )

    ggplot2::ggplot(melted, ggplot2::aes(variable, value, fill = ID, group = ID)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::scale_fill_manual(values = cust_color_pal1) +
        ggplot2::ggtitle("Bundle Price by Component") +
        ggplot2::xlab("Component Bundle") +
        ggplot2::ylab("Price ($)") +
        ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
        ggplot2::theme(#axis.title.x=element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank())
}
