library(ODEsensitivity)

source("set_params.r")
source("ca_mod_eqnsMorris_male.r")
soucre("ca_mod_eqnsMorris_female.r")
source("varnames.r")
source("init_conds.r")

sexORrep <- 'male'

vnames <- get_varnames()
init_cond <- unlist(init_conds(sexORrep)[vnames])
p <- set_params(sexORrep)

if (sexORrep == 'male') {
    modeqns <- ca_mod_eqnsMorris_male
} else if (sexORrep == 'female') {
   modeqns <- ca_mod_eqnsMorris_female
}else {
    print(sexORrep + " not found")
}

mtimes <- c(1000, 4000)

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
                                r = 1000
                                )

end <- Sys.time()
print(end)
print(difftime(end, start, units = "mins"))

save_info = 1
if (save_info) {
    today <- Sys.Date()
    fname <- paste(today, 
                    "_MorrisAnalysis_SS",
                    "_sexORrep-",
                    sexORrep,
                    ".RData",
                    sep = "")
    save.image(fname)
    print("results saved to:")
    print(sprintf("%s", fname))
}