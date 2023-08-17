compute_ss <- function(X, optvals) {
    sexORrep <- optvals$sexORrep
    varname <- optvals$var_out

    if (sexORrep == 'male') {
        modeqns <- ca_mod_eqnsMorris_male_all
        amt_PTHg0 <- 34.986
        PTHp_con0 <- 6.279
        Cap_con0 <- 1.179
        D3p_con0 <- 153.688
        NCaf0 <- 1.626
    } else if (sexORrep == 'female') {
        modeqns <- ca_mod_eqnsMorris_male_all
        amt_PTHg0 <- 35.167
        PTHp_con0 <- 6.288
        Cap_con0 <- 1.179
        D3p_con0 <- 84.38
        NCaf0 <- 1.626
    } else if (sexORrep == 'preg') {
        modeqns <- ca_mod_eqnsMorris_preglact_all
        amt_PTHg0 <- 24.269
        PTHp_con0 <- 13.469
        Cap_con0 <- 1.179
        D3p_con0 <- 269.108
        NCaf0 <- 1.625
    } else if (sexORrep == 'lact') {
        #modeqns <- ca_mod_eqnsMorris_lact
        modeqns <- ca_mod_eqnsMorris_preglact_all
        amt_PTHg0 <- 23.445
        PTHp_con0 <- 13.247
        Cap_con0 <- 1.178
        D3p_con0 <- 329.178
        NCaf0 <- 1.624
    }


    init_guess <- c(amt_PTHg = amt_PTHg0,
                    PTHp_con = PTHp_con0,
                    Cap_con = Cap_con0,
                    D3p_con = D3p_con0,
                    amt_NCaf = NCaf0)

    one_par <- function(i) {
        ST <- runsteady(init_guess, time = c(0, 1e5), func = modeqns,
                    parms = X[i, ], stol = 1e-06, rtol = 1e-6, atol = 1e-6)
        checkSS <- attributes(ST)$steady
        if (checkSS) {
            return(ST$y)
        } else if (!checkSS) {
            print(i)
            print('WARNING: SS not reached')
            print(ST$y)
            ST2 <- stodes(init_guess, times = 0, func = modeqns,
                            parms = X[i, ], rtol = 1e-4, atol = 1e-5)
            checkSS2 <- attributes(ST2)$steady
            if (checkSS2) {
                return(ST2$y)
            } else {
                print('WARNING: SS2 not reached')
                print(ST2)
                return(init_guess)
            }
        }
    }

        # #ST1 <- runsteady(init_guess, time = c(0,5000), func = modeqns,
        #                     #parms = X[i, ])
        # # ST1 <- runsteady(init_guess, time = c(0, 500), func = modeqns,
        # #                     parms = X[i, ])
        # # ST <- stodes(ST1$y, times = 0, func = modeqns,
        # #                     parms = X[i, ], rtol = 1e-5, atol = 1e-6)
        # # # attributes(ST) # could use this to debug and see what attributes are!
        # # check_ss1 <- attributes(ST)$steady # this can check for steady state
        # # if (!check_ss1) {
        # #     print('trying larger tol')
        # #     print(i)
        # #     # print(X[i, ])
        # #     print(ST1$y)
        # #     print(ST$y)
        # #     ST2 <- stodes(ST1$y, times = 0, func = modeqns,
        # #                     parms = X[i, ], rtol = 1e-3, atol = 1e-4)
        # #     print(ST2$y)
        # #     check_ss2 <- attributes(ST2)$steady
        # #     if (!check_ss2) {
        # #         print('higher tol not reached')
        # #         print(i)
        # #         print(ST2$y)
        # #         return(init_guess)
        # #     }
        # #     return(ST2$y) # set to run steady if has issues converging
        # # } else {
        # #     return(ST$y)
        # # }
        # }

    res_per_par <- sapply(1:nrow(X), one_par, simplify = TRUE)
    res_per_state <- aperm(res_per_par)

    if (varname == 'amt_PTHg') {
        varid <- 1
    } else if (varname == 'PTHp_con') {
       varid <- 2
    } else if (varname == 'Cap_con') {
       varid <- 3
    } else if (varname == 'D3p_con') {
       varid <- 4
    } else if (varname == 'amt_NCaf') {
       varid <- 5
    } else {
       varid <- 0
       temp <- sprintf('varname: %s ', varname)
       print(temp)
       print('varname not found!')
    }
    vals <- res_per_state[, varid]

    return(vals)
}