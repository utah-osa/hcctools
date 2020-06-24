#' Title
#'
#' @param df
#' @param tag
#'
#' @return
#' @export
#'
#' @examples
get_tag_density_information <- function(df, tag) {
    gg1 <- hcctools::boxplot_by_tag(df = df, tag = tag)
    gg2 <- hcctools::plot_ridge_tag(df = df, tag = tag)



    stats_table <- df %>% tibble::as_tibble() %>%
        dplyr::mutate(!!sym(tag) := dplyr::if_else(!!sym(tag) == 1, "Yes", "No")) %>%
        #select(!!sym(tag)) %>%
        dplyr::group_by(!!sym(tag)) %>%
        dplyr::summarize(
            `Mean` = mean(vt_med) %>% round(0),
            `Min` = min(vt_med) %>% round(0),
            `1Q` = vt_med %>% quantile(.25) %>% round(0),
            `Median` = median(vt_med) %>% round(0),
            `3Q` = vt_med %>% quantile(.75) %>% round(0),
            `Max` = max(vt_med) %>% round(0),
            `sd` = sd(vt_med) %>% round(0),
            `Frequency` = n(),
        ) %>%
        dplyr::arrange(dplyr::desc(!!sym(tag)))

    stats_table <- ggtexttable(stats_table,
                               rows = NULL,
                               theme = ttheme("classic"))

    f_str <- paste("vt_med", "~", tag)
    print(f_str)

    levene <- df %>% rstatix::levene_test(as.formula(f_str))

    levene_table <- ggpubr::ggtexttable(levene, rows = NULL,
                                theme = ggpubr::ttheme("classic"))

    welch <- df %>% rstatix::t_test(as.formula(f_str), var.equal = FALSE)

    welch_table <- ggpubr::ggtexttable(welch, rows = NULL,
                               theme = ttheme("classic"))

    ggpubr::ggarrange(
        gg1,
        gg2,
        ggpubr::ggarrange(
            stats_table,
            levene_table,
            welch_table,
            nrow = 3,
            ncol = 1,
            labels = c(NA, "levene", "welch")
        ),
        #labels = c("A", "B"),
        nrow = 3,
        ncol = 1,
        align = "v",
        common.legend = TRUE,
        legend = "top"
    )
}
