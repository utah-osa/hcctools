select_med_stats <- function(df) {
    df <- df %>% select(
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

        medi_bundle,
        medi_bundle_prs,
        medi_bun_descr,
        medi_bun_med,

        radi_bundle,
        radi_bundle_prs,
        radi_bun_descr,
        radi_bun_med,

        path_bundle,
        path_bundle_prs,
        path_bun_descr,
        path_bun_med,

        anes_bundle,
        anes_bundle_prs,
        anes_bun_descr,
        anes_bun_med,

        fac_bundle,
        fac_bundle_prs,
        fac_bun_descr,
        fac_bun_med,

    )
}
