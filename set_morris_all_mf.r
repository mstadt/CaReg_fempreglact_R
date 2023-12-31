# parameter settings for Morris method
testpars <- c("Vp",
            "GFR",
            "k_prod_PTHg",
            "K_Ca_CASR",
            "beta_exo_PTHg",
            "gamma_exo_PTHg",
            "K_abs_D3",
            "Gamma_abs0",
            "delta_abs_D3",
            "ICa",
            "delta_res_max",
            "K_PTHp_res",
            "K_D3p_res",
            "Gamma_res_min",
            "PTHp_ref",
            "Cap_ref",
            "K_TAL_PTHp",
            "K_DCT_PTHp",
            "K_DCT_D3p",
            "k_pf_Ca",
            "k_fp_Ca",
            "K_conv_PTH",
            "gamma_conv_Ca",
            "gamma_conv_D3",
            "k_conv_min",
            "delta_conv_max",
            "D3_inact_p",
            "gamma_deg_PTHp",
            "Gamma_ac",
            "gamma_prod_D3",
            "k_PTHg_deg",
            "k_PTHp_deg",
            "k_deg_D3",
            "delta_PT_max",
            "Lambda_PT0",
            "delta_TAL_max",
            "Lambda_TAL0",
            "delta_DCT_max",
            "Lambda_DCT0"
            )

parsbinf <- c(0.9 * p$Vp,
            0.9 * p$GFR,
            0.9 * p$k_prod_PTHg,
            1.1, # K_Ca_CASR, normal range [Ca]_p
            0.9 * p$beta_exo_PTHg,
            0.9 * p$gamma_exo_PTHg,
            0.9 * p$K_abs_D3,
            0.9 * p$Gamma_abs0,
            0.9 * p$delta_abs_D3,
            0.9 * p$ICa,
            0.9 * p$delta_res_max,
            0.9 * p$K_PTHp_res,
            0.9 * p$K_D3p_res,
            0.9 * p$Gamma_res_min,
            0.9 * p$PTHp_ref,
            0.9 * p$Cap_ref,
            0.9 * p$K_TAL_PTHp,
            0.9 * p$K_DCT_PTHp,
            0.9 * p$K_DCT_D3p,
            0.9 * p$k_pf_Ca,
            0.9 * p$k_fp_Ca,
            0.9 * p$K_conv_PTH,
            0.9 * p$gamma_conv_Ca,
            0.9 * p$gamma_conv_D3,
            0.9 * p$k_conv_min,
            0.9 * p$delta_conv_max,
            0.9 * p$D3_inact_p,
            0.9 * p$gamma_deg_PTHp,
            0.9 * p$Gamma_ac,
            0.9 * p$gamma_prod_D3,
            0.9 * p$k_PTHg_deg,
            0.9 * p$k_PTHp_deg,
            0.9 * p$k_deg_D3,
            0.9 * p$delta_PT_max,
            0.9 * p$Lambda_PT0,
            0.9 * p$delta_TAL_max,
            0.9 * p$Lambda_TAL0,
            0.9 * p$delta_DCT_max,
            0.9 * p$Lambda_DCT0
            )

parsbsup <- c(1.10 * p$Vp,
            1.10 * p$GFR,
            1.10 * p$k_prod_PTHg,
            1.3, # K_Ca_CASR, normal range [Ca]_p
            1.10 * p$beta_exo_PTHg,
            1.10 * p$gamma_exo_PTHg,
            1.10 * p$K_abs_D3,
            1.10 * p$Gamma_abs0,
            1.10 * p$delta_abs_D3,
            1.10 * p$ICa,
            1.10 * p$delta_res_max,
            1.10 * p$K_PTHp_res,
            1.10 * p$K_D3p_res,
            1.10 * p$Gamma_res_min,
            1.10 * p$PTHp_ref,
            1.10 * p$Cap_ref,
            1.10 * p$K_TAL_PTHp,
            1.10 * p$K_DCT_PTHp,
            1.10 * p$K_DCT_D3p,
            1.10 * p$k_pf_Ca,
            1.10 * p$k_fp_Ca,
            1.10 * p$K_conv_PTH,
            1.10 * p$gamma_conv_Ca,
            1.10 * p$gamma_conv_D3,
            1.10 * p$k_conv_min,
            1.10 * p$delta_conv_max,
            1.10 * p$D3_inact_p,
            1.10 * p$gamma_deg_PTHp,
            1.10 * p$Gamma_ac,
            1.10 * p$gamma_prod_D3,
            1.10 * p$k_PTHg_deg,
            1.10 * p$k_PTHp_deg,
            1.10 * p$k_deg_D3,
            1.10 * p$delta_PT_max,
            1.10 * p$Lambda_PT0,
            1.10 * p$delta_TAL_max,
            1.10 * p$Lambda_TAL0,
            1.10 * p$delta_DCT_max,
            1.10 * p$Lambda_DCT0
            )