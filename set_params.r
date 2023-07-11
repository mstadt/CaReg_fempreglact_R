set_params <- function(sexORrep) {
    # set parameters for model
    # sexORrep -- male, female, preg, lact options

    # start with only male parameter values
    if (sexORrep == 'male') {
    list(
        Vp = 10e-3,
        gamma_prod_D3 = 3.0e-3,
        k_prod_PTHg = 3.0,
        k_PTHg_deg = 0.035,
        n1_exo = 100,
        n2_exo = 30,
        rho_exo = 10,
        R = 1.1,
        K_Ca_CASR = 1.17,
        beta_exo_PTHg = 0.059,
        gamma_exo_PTHg = 0.057,
        k_PTHp_deg = 0.1320,
        K_abs_D3 = 160,
        Gamma_abs0 = 0.25,
        delta_abs_D3 = 0.45,
        ICa = 2.3e-3,
        delta_res_max = 0.7e-3,
        K_PTHp_res = 2.45,
        K_D3p_res = 160,
        Gamma_res_min = 0.142e-3,
        GFR = 2.0e-3,
        delta_PT_max = 0.03,
        PTHp_ref = 12,
        nPT = 2,
        Lambda_PT0 = 0.66,
        delta_TAL_max = 0.015,
        Cap_ref = 1.7,
        nTAL = 2,
        K_TAL_PTHp = 4,
        Lambda_TAL0 = 0.185,
        delta_DCT_max = 0.015,
        K_DCT_PTHp = 7.25,
        K_DCT_D3p = 160,
        Lambda_DCT0 = 0.095,
        k_pf_Ca = 0.0017,
        k_fp_Ca = 2.75e-4,
        kappa_b = 0.4,
        FetusORMilk = 0,
        nconv = 6,
        K_conv_PTH = 7.25,
        gamma_conv_Ca = 0.3,
        gamma_conv_D3 = 1.8e-2,
        k_conv_min = 4.4e-6,
        delta_conv_max = 6.02e-5,
        D3_inact_p = 25e3,
        gamma_deg_PTHp = 0.2,
        k_deg_D3 = 0.0029,
        Gamma_ac = 0.958e-3
    )
    } else {
        temp = sprintf('%s params not available', sexORrep)
        print(temp)
    }
}