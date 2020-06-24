#select_component_stats
# This function selects the component stats of interest, thereby removing all the tags.
select_component_stats <- function(df) {
    df <- df %>% dplyr::select(
        pc_list,
        pr_list,
        vt_cnt,
        vt_med,
        vt_min,
        vt_max,
        vt_sum,

        surg_bundle,
        surg_bundle_prs,
        surg_bun_descr,
        surg_bun_med,
        surg_bun_max,
        surg_bun_min,
        surg_bun_avg,
        surg_bun_sum,

        medi_bundle,
        medi_bundle_prs,
        medi_bun_descr,
        medi_bun_med,
        medi_bun_max,
        medi_bun_min,
        medi_bun_avg,
        medi_bun_sum,

        radi_bundle,
        radi_bundle_prs,
        radi_bun_descr,
        radi_bun_med,
        radi_bun_max,
        radi_bun_min,
        radi_bun_avg,
        radi_bun_sum,

        path_bundle,
        path_bundle_prs,
        path_bun_descr,
        path_bun_med,
        path_bun_max,
        path_bun_min,
        path_bun_avg,
        path_bun_sum,

        anes_bundle,
        anes_bundle_prs,
        anes_bun_descr,
        anes_bun_med,
        anes_bun_max,
        anes_bun_min,
        anes_bun_avg,
        anes_bun_sum,

        fac_bundle,
        fac_bundle_prs,
        fac_bun_descr,
        fac_bun_med,
        fac_bun_max,
        fac_bun_min,
        fac_bun_avg,
        fac_bun_sum
    )
}
