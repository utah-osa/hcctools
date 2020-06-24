#' Title
#'
#' @param df
#' @param stack_group
#' @param temp_title
#' @param temp_ylab
#' @param temp_xlab
#'
#' @return
#' @export
#'
#' @examples
plot_med_density <- function(df,
                                stack_group="none",
                                temp_title = "Density Plot",
                                temp_ylab = "Count",
                                temp_xlab = "Median Bundled Procedure Price"){


    if(stack_group=="none"){
        med_price_dist <- ggplot2::ggplot(data=df,
                                          ggplot2::aes(x=vt_med))+
            ggplot2::geom_histogram(fill="#617ff7")+
            #geom_density(aes(y=..count..))+
            ggplot2::geom_vline(xintercept = median(df$vt_med))+
            ggplot2::annotate("label",
                     x=median(df$vt_med),
                     y=15,
                     color="#617ff7",
                     label=paste0(round(mean(df$vt_med),2) ))+
            ggplot2::labs(title=temp_title)+
            ggplot2::xlab(temp_xlab)+
            ggplot2::ylab(temp_ylab)

        # print(med_price_dist)
    }else if (stack_group == "bundle_group") {
        print("printing bundle_group")

        # caculate bundle group medians
        bg_above_avg_median <- df %>%
            dplyr::filter(bundle_group=="above avg") %>%
            dplyr::pull(vt_med) %>%
            mean()%>%
            round(2)

        bg_below_avg_median <- df %>%
            dplyr::filter(bundle_group=="below avg") %>%
            dplyr::pull(vt_med) %>%
            mean()%>%
            round(2)

        bg_avg_median <- df %>%
            dplyr::filter(bundle_group=="avg") %>%
            dplyr::pull(vt_med) %>%
            mean() %>%
            round(2)

        mean_of_median <- df$vt_med %>% mean() %>% round(2)

        # generate plot
        med_price_dist <- ggplot2::ggplot(data = df,
                                    ggplot2::aes_(x = as.name("vt_med"),
                                      fill = as.name(stack_group)
                                 )
        ) +
            ggplot2::geom_histogram(#fill = as.name(stack_group),
                position = "stack") +
            #geom_density(aes(y=..count..))+
            ggplot2::geom_vline(xintercept = mean_of_median) +
            ggplot2::annotate(
                "label",
                x = mean_of_median,
                y = 30,
                color = "black",
                label = paste0(mean_of_median)
            )+
            ggplot2::geom_vline(xintercept = bg_above_avg_median) +
            ggplot2::annotate(
                "label",
                x = bg_above_avg_median,
                y = 15,
                color = "red",
                label = paste0(bg_above_avg_median)
            )+
            ggplot2::geom_vline(xintercept = bg_below_avg_median) +
            ggplot2::annotate(
                "label",
                x = bg_below_avg_median,
                y = 15,
                color = "blue",
                label = paste0(bg_below_avg_median)
            )+
            ggplot2::geom_vline(xintercept = bg_avg_median) +
            ggplot2::annotate(
                "label",
                x = bg_avg_median,
                y = 15,
                color = "green",
                label = paste0(bg_avg_median)
            ) +
            ggplot2::labs(title=temp_title)+
            ggplot2::xlab(temp_xlab)+
            ggplot2::ylab(temp_ylab)

    }


}
