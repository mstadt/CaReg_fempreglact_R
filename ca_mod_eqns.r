ca_mod_eqns <- function(Time, State, Pars) {
    # simulation settings
    
    # state varables
    # amt_PTHg  -- amount of PTH in the parathyroid gland pool
    # amt_PTHp -- amount of PTH in plasma
    # amt_Cap  -- amount of Ca in plasma
    # amt_D3p  -- amount of 1,25(OH)2D3 in plasma
    # amt_NCaf -- amount of calcium in rapidly exchangeable pool

    with(as.list(c(State, Pars)), {
        # concentrations
        PTHp_con <- amt_PTHp/Vp
        Cap_con  <- amt_Cap/Vp
        D3p_con  <- amt_D3p/Vp

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

        dPTHp = PTHp_influx - PTHp_degradation
        
        # Cap
        # intestinal calcium absorption
        Gut_impact_D3 = D3p_con^2 / (K_abs_D3^2 + D3p_con^2)
        Gut_frac_absorption = Gamma_abs0 + (delta_abs_D3) * Gut_impact_D3
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

        dCap = (1 - kappa_b)*(Gut_absorption + Bone_resorption + FastPool_to_Plasma -
            Plasma_to_FastPool - Urine_excretion - FetusORMilk)

        # D3p (calcitriol)
        PTH_impact_D3 = PTHp_con^nconv/(PTHp_con^nconv + K_conv_PTH^nconv)
        Ca_impact_D3  = 1 / (1 + gamma_conv_Ca * Cap_con)
        D3_impact_D3  = 1 / (1 + gamma_conv_D3 * D3p_con)
        Rconv = k_conv_min + delta_conv_max * PTH_impact_D3 *
            Ca_impact_D3 * D3_impact_D3
        D3_synthesis = Rconv * D3_inact_p

        PTH_impact_D3_degradation = 1 / (1 + gamma_deg_PTHp * PTHp_con)
        D3_degradation = k_deg_D3 * D3p_con * PTH_impact_D3_degradation
        dD3p = D3_synthesis - D3_degradation

        # NCaf
        Bone_accretion = Gamma_ac * amt_NCaf
        dNCaf = Plasma_to_FastPool - FastPool_to_Plasma - Bone_accretion

        return(list(c(dPTHg, dPTHp, dCap, dD3p, dNCaf)))
    })
}