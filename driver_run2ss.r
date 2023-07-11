# run ca_mod_eqns system to steady state

library(deSolve)

source("set_params.r")
source("ca_mod_eqns.r")
source("varnames.r")

init_cond = c(amt_PTHg = 34.986,
                amt_PTHp = 0.06279,
                amt_Cap = 0.01179,
                amt_D3p = 1.53688,
                amt_1.626)

camod <- list(init = init_conds(),


            )