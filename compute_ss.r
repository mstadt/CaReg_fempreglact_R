compute_ss <- function(X) {
    # initial guess values
    # NOTE: currently female
    amt_PTHg0 <- 35.167
    PTHp_con0 <- 6.288
    Cap_con0 <- 1.179
    D3p_con0 <- 84.38
    NCaf0 <- 1.626

    init_guess <- c(amt_PTHg = amt_PTHg0,
                    PTHp_con = PTHp_con0,
                    Cap_con = Cap_con0,
                    D3p_con = D3p_con0,
                    amt_NCaf = NCaf0)

    one_par <- function(i) {
        ST1 <- runsteady(init_guess, time = c(0,5000), func = ca_mod_eqnsMorris_male_all,
                            parms = X[i, ])
        ST <- stodes(ST1$y, times = 0, func = ca_mod_eqnsMorris_male_all,
                            parms = X[i, ], rtol = 1e-3, atol = 1e-3)
        # attributes(ST) # could use this to debug and see what attributes are!
        check_ss <- attributes(ST)$steady # this can check for steady state
        if (!check_ss) {
            print(i)
            return(ST1$y) # set to run steady if has issues converging
        } else {
            return(ST$y)
        }
        }

    res_per_par <- sapply(1:nrow(X), one_par, simplify = TRUE)
    res_per_state <- aperm(res_per_par)

    #var_out = "PTHp_con"
    vals <- res_per_state[,2]

    return(vals)
}