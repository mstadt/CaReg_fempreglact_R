library(ODEsensitivity)

source("set_params.r")
source("ca_mod_eqnsMorris_male.r")
source("varnames.r")
source("init_conds.r")

sexORrep <- 'male'

vnames <- get_varnames()
init_cond = unlist(init_conds(sexORrep)[vnames])
p <- set_params(sexORrep)

if (sexORrep == 'male') {
    modeqns <- ca_mod_eqnsMorris_male
} else {
    temp <- sprintf('%s eqns not done', sexORrep)
    print(temp)
}

mtimes = c(1000, 4000)

# to get testpars, parsbinf, parsbsup
source("set_morris.r")

# run Morris Method
set.seed(151)
start <- Sys.time()
print(start)
print('computing Morris method')

camod_res_morris <- ODEmorris(mod = modeqns,
                                pars = testpars,
                                state_init = init_cond,
                                times = mtimes,
                                binf = parsbinf,
                                bsup = parsbsup,
                                r = 500
                                )

end <- Sys.time()
print(end)
print(difftime(end, start, units = "mins"))