ca_mod_eqnsMorris_1<- function(Time, State, Pars) {
    # simulation settings
    
    # state varables
    # amt_PTHg  -- amount of PTH in the parathyroid gland pool
    # PTHp_con -- amount of PTH in plasma
    # Cap_con  -- amount of Ca in plasma
    # D3p_con  -- amount of 1,25(OH)2D3 in plasma
    # amt_NCaf -- amount of calcium in rapidly exchangeable pool

    with(as.list(c(State, Pars)), {
        # fixed parameters (not changed in Morris)
        # male parameters
        #Vp = 10e-3
        gamma_prod_D3 = 3.0e-3
        #k_prod_PTHg = 3.0,
        k_PTHg_deg = 0.035
        n1_exo = 100
        n2_exo = 30
        rho_exo = 10
        R = 1.1
        #K_Ca_CASR = 1.17,
        beta_exo_PTHg = 0.059
        gamma_exo_PTHg = 0.057
        #k_PTHp_deg = 0.1320
        #K_abs_D3 = 160,
        #Gamma_abs0 = 0.25,
        #delta_abs_D3 = 0.45,
        #ICa = 2.3e-3,
        #delta_res_max = 0.7e-3,
        K_PTHp_res = 2.45
        #K_D3p_res = 160,
        Gamma_res_min = 0.142e-3
        GFR = 2.0e-3
        delta_PT_max = 0.03
        PTHp_ref = 12
        nPT = 2
        #Lambda_PT0 = 0.66,
        delta_TAL_max = 0.015
        Cap_ref = 1.7
        nTAL = 2
        K_TAL_PTHp = 4
        #Lambda_TAL0 = 0.185,
        delta_DCT_max = 0.015
        K_DCT_PTHp = 7.25
        K_DCT_D3p = 160
        # Lambda_DCT0 = 0.095
        #k_pf_Ca = 0.0017,
        #k_fp_Ca = 2.75e-4,
        kappa_b = 0.4
        FetusORMilk = 0
        nconv = 6
        #K_conv_PTH = 7.25,
        gamma_conv_Ca = 0.3
        gamma_conv_D3 = 1.8e-2
        #k_conv_min = 4.4e-6,
        delta_conv_max = 6.02e-5
        #D3_inact_p = 25e3,
        #gamma_deg_PTHp = 0.2,
        #k_deg_D3 = 0.0029
        #Gamma_ac = 0.958e-3


        # PTHg
        PTHg_prod_effect_D3 = 1/(1 + gamma_prod_D3 * D3p_con)
        PTHg_basal_synthesis = k_prod_PTHg
        PTHg_synthesis = PTHg_basal_synthesis * PTHg_prod_effect_D3

        PTHg_degradation = k_PTHg_deg * amt_PTHg
        n_Ca_norm = n1_exo/(1 + exp(-rho_exo * (R - Cap_con))) + n2_exo
        CaSR_PTHg_regulation =  Cap_con^n_Ca_norm / (Cap_con^n_Ca_norm 
            + (K_Ca_CASR)^n_Ca_norm)
        F_Ca = beta_exo_PTHg - gamma_exo_PTHg * CaSR_PTHg_regulation
        PTHg_exocytosis = F_Ca * amt_PTHg

        dPTHg = PTHg_synthesis - PTHg_exocytosis - PTHg_degradation

        # PTHp
        PTHp_influx = PTHg_exocytosis
        PTHp_degradation = k_PTHp_deg * PTHp_con

        dPTHp_con = (PTHp_influx - PTHp_degradation) * (1 / Vp)
        
        # Cap
        # intestinal calcium absorption
        Gut_impact_D3 = D3p_con^2 / (K_abs_D3^2 + D3p_con^2)
        Gut_frac_absorption = min(0.9999, Gamma_abs0 + (delta_abs_D3) * Gut_impact_D3)
        Gut_absorption = ICa * Gut_frac_absorption

        #bone resorption
        PTHp_res_effect = delta_res_max * 0.2 * PTHp_con^2 / (PTHp_con^2 + K_PTHp_res^2)
        D3_res_effect   = delta_res_max * 0.8 * D3p_con^2 / (D3p_con^2 + K_D3p_res^2)
        Bone_resorption = Gamma_res_min + (PTHp_res_effect + D3_res_effect)

        # renal calcium handling
        Renal_filtration = GFR * Cap_con
        delta_PT_PTH = delta_PT_max / (1 + (PTHp_con / PTHp_ref)^nPT)
        Lambda_PT = Lambda_PT0 + delta_PT_PTH
        delta_TAL_Ca = 0.7 * delta_TAL_max / (1 + (Cap_con / Cap_ref)^nTAL)
        delta_TAL_PTH = 0.3 * delta_TAL_max * PTHp_con / (PTHp_con + K_TAL_PTHp)
        Lambda_TAL = Lambda_TAL0 + delta_TAL_Ca + delta_TAL_PTH
        delta_DCT_PTH = 0.8 * delta_DCT_max * PTHp_con / (PTHp_con + K_DCT_PTHp)
        delta_DCT_D3  = 0.2 * delta_DCT_max * D3p_con / (D3p_con + K_DCT_D3p)
        Lambda_DCT = Lambda_DCT0 + delta_DCT_PTH + delta_DCT_D3
        Renal_frac_reab = min(0.9999999, Lambda_PT + Lambda_TAL + Lambda_DCT)
        Urine_excretion = (1-Renal_frac_reab)*Renal_filtration
    
        # bone fast pool
        Plasma_to_FastPool = k_pf_Ca * Cap_con
        FastPool_to_Plasma = k_fp_Ca * amt_NCaf

        dCap_con = (1 - kappa_b)*(Gut_absorption + Bone_resorption + FastPool_to_Plasma -
            Plasma_to_FastPool - Urine_excretion - FetusORMilk) * (1 / Vp)

        # D3p (calcitriol)
        PTH_impact_D3 = PTHp_con^nconv/(PTHp_con^nconv + K_conv_PTH^nconv)
        Ca_impact_D3  = 1 / (1 + gamma_conv_Ca * Cap_con)
        D3_impact_D3  = 1 / (1 + gamma_conv_D3 * D3p_con)
        Rconv = k_conv_min + delta_conv_max * PTH_impact_D3 *
            Ca_impact_D3 * D3_impact_D3
        D3_synthesis = Rconv * D3_inact_p

        PTH_impact_D3_degradation = 1 / (1 + gamma_deg_PTHp * PTHp_con)
        D3_degradation = k_deg_D3 * D3p_con * PTH_impact_D3_degradation
        dD3p_con = (D3_synthesis - D3_degradation) * (1 / Vp)

        # NCaf
        Bone_accretion = Gamma_ac * amt_NCaf
        dNCaf = Plasma_to_FastPool - FastPool_to_Plasma - Bone_accretion

        return(list(c(dPTHg, dPTHp_con, dCap_con, dD3p_con, dNCaf)))
    })
}