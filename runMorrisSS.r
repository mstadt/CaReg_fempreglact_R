library(rootSolve)
library(sensitivity)
source("set_params.r")
source("computeSS_1var.r")

sexORrep <- "female"
temp <- sprintf("%s Morris analysis", sexORrep)
print(temp)

p <- set_params(sexORrep)

if (sexORrep == 'male') {
    # to get testpars, parsbinf, parsbsup
    source("set_morris_all_mf.r")
    source("ca_mod_eqnsMorris_male_all.r")
    #modeqns <- ca_mod_eqnsMorris_male_all
} else if (sexORrep == 'female') {
    source("set_morris_all_mf.r")
    source("ca_mod_eqnsMorris_male_all.r")
    #modeqns <- ca_mod_eqnsMorris_male_all # same for male and female
} else if (sexORrep == 'preg') {
    source("set_morris_all_preglact.r")
    source("ca_mod_eqnsMorris_preglact_all.r")
    #modeqns <- ca_mod_eqnsMorris_preglact_all
} else if (sexORrep == 'lact') {
    source("set_morris_all_preglact.r")
    source("ca_mod_eqnsMorris_preglact_all.r")
    #modeqns <- ca_mod_eqnsMorris_preglact_all
} else {
    print(sexORrep + " not found")
}

# run Morris Method
rval = 100
startall <- Sys.time()
# PTHp_con
print('start PTHp morris')
startPTHp <- Sys.time()
optsvals <- list(
        sexORrep = sexORrep,
        var_out = "PTHp_con"
        )
x_PTHp <- morris(model = computeSS,
                    factors = testpars,
                    r = rval,
                    design = list(type = "oat",
                                    levels = 10,
                                    grid.jump = 1),
                    binf = parsbinf,
                    bsup = parsbsup,
                    scale = TRUE,
                    func_opts = optsvals
                    )

endPTHp <- Sys.time()
print(difftime(endPTHp, startPTHp, unit = "mins"))

# Cap_con

# D3p_con