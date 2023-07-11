# run ca_mod_eqns system to steady state

library(deSolve)

source("set_params.r")
source("ca_mod_eqns.r")
source("varnames.r")
source("init_conds.r")

sexORrep <- 'female'

camod <- list(init = init_conds(sexORrep),
            params = set_params(sexORrep),
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