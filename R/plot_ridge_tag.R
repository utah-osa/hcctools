#' Title
#'
#' @param df
#' @param tag
#' @param resp
#'
#' @return
#' @export
#'
#' @examples
plot_ridge_tag <- function(df, tag, resp = "vt_med") {
    max_lim <- df %>% dplyr::pull(vt_med) %>% max()

    tag_type <-
        stringr::str_extract(tag, "_[[:alpha:]]+$") %>%
        stringr::str_replace("_", "")

    ylabel <- paste0("Bundle has '", tag_type, "' tag?")

    xlabel <- "Median Bundle Price"

    temp_title <-
        paste0("Ridge Plot: Bundle with '", tag_type, "' tag")

    # scale_fill_values <- c("TRUE" = "#003f5c",
    #                        "FALSE" = "#ffa600")


    df %>%
        #filter(vt_med < 35000) %>%
        ggplot2::ggplot(data = .,
                        ggplot2::aes_(x = as.name(resp), y = as.name(tag))) +
        ggridges::stat_density_ridges(
            ggplot2::aes_(fill = as.name(tag), alpha = .8),
            # stat = "binline",
            # bins = 20,
            # scale = 0.95,,
            # draw_baseline = FALSE
            quantile_lines = TRUE,
            quantiles = 2
        ) +
        ggplot2::geom_label(
            data = df %>%
                dplyr::group_by(!!sym(tag)) %>%
                dplyr::summarise(!!sym(resp) := median(!!sym(resp))),
            ggplot2::aes_(label = as.name(resp), #label = dollar(as.name(resp)),
                 color = as.name(tag)),
            size = 3.5,
            show.legend = FALSE
        ) +
        ggplot2::scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
        ggplot2::labs(title = temp_title,
             fill = paste0("Bundle has '", tag_type, "'")) +
        ggplot2:: xlab(xlabel) +
        ggplot2:: ylab(ylabel) +
        #theme(legend.title = element_blank())+
        ggplot2::scale_y_discrete(labels = c("TRUE" = "Yes",
                                    "FALSE" = "No")) +
        ggplot2::scale_fill_manual(
            values = cust_color_pal2,
            labels = c("TRUE" = "Yes",
                       "FALSE" = "No"),
            guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::scale_color_manual(values = cust_color_pal2) +
        ggplot2::xlim(0, max_lim)

}
