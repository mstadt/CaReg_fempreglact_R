# parameter settings for Morris method
# (21 parameters from local sensitivity analysis)
# NOTE: FETUSORMILK NOT INCLUDED (add later in preg/lact)

testpars <- c("Vp",
            "k_PTHp_deg",
            "delta_res_max",
            "k_deg_D3",
            "k_pf_Ca",
            "k_fp_Ca",
            "k_conv_min",
            "D3_inact_p",
            "ICa",
            "Gamma_abs0",
            "delta_abs_D3",
            "K_abs_D3",
            "K_D3p_res",
            "Lambda_PT0",
            "Lambda_TAL0",
            "Lambda_DCT0",
            "K_Ca_CASR",
            "K_conv_PTH",
            "k_prod_PTHg",
            "gamma_deg_PTHp",
            "Gamma_ac"
            )

rde <- 0.9 # ratio to decrease
parsbinf <- c(rde * p$Vp,
            rde * p$k_PTHp_deg,
            rde * p$delta_res_max,
            rde * p$k_deg_D3,
            rde * p$k_pf_Ca,
            rde * p$k_fp_Ca,
            rde * p$k_conv_min,
            rde * p$D3_inact_p,
            rde * p$ICa,
            rde * p$Gamma_abs0,
            rde * p$delta_abs_D3,
            rde * p$K_abs_D3,
            rde * p$K_D3p_res,
            0.95 * p$Lambda_PT0,
            0.95 * p$Lambda_TAL0,
            0.95 * p$Lambda_DCT0,
            1.1, # K_Ca_CASR
            rde * p$K_conv_PTH,
            rde * p$k_prod_PTHg,
            rde * p$gamma_deg_PTHp,
            rde * p$Gamma_ac
            )

rinc <- 1.1 # ratio to increase
parsbsup <- c(rinc * p$Vp,
            rinc * p$k_PTHp_deg,
            rinc * p$delta_res_max,
            rinc * p$k_deg_D3,
            rinc * p$k_pf_Ca,
            rinc * p$k_fp_Ca,
            rinc * p$k_conv_min,
            rinc * p$D3_inact_p,
            rinc * p$ICa,
            rinc * p$Gamma_abs0,
            rinc * p$delta_abs_D3,
            rinc * p$K_abs_D3,
            rinc * p$K_D3p_res,
            1.05 * p$Lambda_PT0,
            1.05 * p$Lambda_TAL0,
            1.05 * p$Lambda_DCT0,
            1.3,
            rinc * p$K_conv_PTH,
            rinc * p$k_prod_PTHg,
            rinc * p$gamma_deg_PTHp,
            rinc * p$Gamma_ac
            )