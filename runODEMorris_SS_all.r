library(ODEsensitivity)

source("set_params.r")
source("varnames.r")
source("init_conds.r")

sexORrep <- 'female'
temp <- sprintf("%s Morris analysis", sexORrep)
print(temp)

vnames <- get_varnames()
init_cond = unlist(init_conds(sexORrep)[vnames])
p <- set_params(sexORrep)

if (sexORrep == 'male') {
    # to get testpars, parsbinf, parsbsup
    source("set_morris_all_mf.r")
    source("ca_mod_eqnsMorris_male_all.r")
    modeqns <- ca_mod_eqnsMorris_male_all
} else if (sexORrep == 'female') {
    source("set_morris_all_mf.r")
    source("ca_mod_eqnsMorris_male_all.r")
    modeqns <- ca_mod_eqnsMorris_male_all # same for male and female
} else if (sexORrep == 'preg') {
    source("set_morris_all_preglact.r")
    source("ca_mod_eqnsMorris_preglact_all.r")
    modeqns <- ca_mod_eqnsMorris_preglact_all
} else if (sexORrep == 'lact') {
    source("set_morris_all_preglact.r")
    source("ca_mod_eqnsMorris_preglact_all.r")
    modeqns <- ca_mod_eqnsMorris_preglact_all
} else {
    print(sexORrep + " not found")
}

mtimes <- c(1000, 4000)

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
                                r = 1000,
                                sexORrep = sexORrep
                                )

end <- Sys.time()
print(end)
print(difftime(end, start, units = "mins"))

save_info = 1
if (save_info) {
    today <- Sys.Date()
    fname <- paste(today, 
                    "_MorrisAnalysis_SS_all",
                    "_sexORrep-", sexORrep,
                    ".RData",
                    sep = "")
    save.image(fname)
    print("results saved to:")
    print(sprintf("%s", fname))
}