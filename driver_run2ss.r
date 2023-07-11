# run ca_mod_eqns system to steady state

library(deSolve)

source("set_params.r")
source("ca_mod_eqns.r")
source("varnames.r")
source("init_conds.r")

init_cond = c(amt_PTHg = 34.986,
                amt_PTHp = 0.06279,
                amt_Cap = 0.01179,
                amt_D3p = 1.53688,
                amt_NCaf =1.626)

camod <- list(init = init_conds(),
            params = set_params('male'),
            cmt = get_varnames(),
            model = ca_mod_eqns
            )

times = seq(0,2000,0.1)

# Run the model
out <- as.data.frame(lsoda(
                            unlist(camod$init[camod$cmt]),
                            times,
                            camod$model,
                            camod$params,
                            rtol = 1e-10,
                            atol = 1e-10
                            )
                    )

eql <- as.list(tail(out,n=1))