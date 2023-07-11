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
            "Gamma_res_min"
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
            0.9 * p$Gamma_res_min
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
            1.10 * p$Gamma_res_min
            )