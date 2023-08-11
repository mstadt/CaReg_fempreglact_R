compute_ss <- function(X, func_opts) {
    sexORrep <- func_opts$sexORrep
    var_out <- func_opts$var_out
    # initial guess values
    if (sexORrep == "male"){
        init_guess <- c(
            amt_PTHg = 34.986,
            PTHp_con = 6.279,
            Cap_con = 1.179,
            D3p_con = 153.688,
            amt_NCaf = 1.626
            )
    } else if (sexORrep == "female") {
        init_guess <- c(
            amt_PTHg = 35.167,
            PTHp_con = 6.288,
            Cap_con = 1.179,
            D3p_con = 84.38,
            amt_NCaf = 1.626
            )
    } else if (sexORrep == "preg") {
        init_guess <- c(
            amt_PTHg = 24.269,
            PTHp_con = 13.469,
            Cap_con = 1.179,
            D3p_con = 269.108,
            amt_NCaf = 1.625
            )
    } else if (sexORrep == "lact") {
        init_guess <- c(
            amt_PTHg = 23.445,
            PTHp_con = 13.247,
            Cap_con = 1.178,
            D3p_con = 329.178,
            amt_NCaf = 1.62
        )
    } else {
        temp = sprintf('%s init cond not available', sexORrep)
        print(temp)
    }

    one_par <- function(i){
        if (sexORrep == 'male') {
            fun <- ca_mod_eqnsMorris_male_all
        } else if (sexORrep == 'female') {
            fun <- ca_mod_eqnsMorris_male_all
        } else if (sexORrep == 'preg') {
            fun <- ca_mod_eqnsMorris_preglact_all
        } else if (sexORrep == 'lact') {
            fun <- ca_mod_eqnsMorris_preglact_all
        }
        ST <- stode(init_guess, time = 0, func = fun,
                        parms = X[i, ])
        return(ST$y)
        }

    res_per_par <- sapply(1:nrow(X), one_par, simplify = TRUE)
    res_per_state <- aperm(res_per_par)
    if (var_out == 'amt_PTHg') {
        vals <- res_per_state[,1]
    } else if (var_out == 'PTHp_con') {
        vals <- res_per_state[,2]
    } else if (var_out == 'Cap_con') {
        vals <- res_per_state[,3]
    } else if (var_out == 'D3p_con') {
        vals <- res_per_state[,4]
    } else if (var_out == 'amt_NCaf') {
        vals <- res_per_state[,5]
    } else {
        temp = sprintf('%s variable is not available', var_out)
        print(temp)
    }
    return(vals)
}