library(rootSolve)
library(sensitivity)
source("set_params.r")


sexORrep <- "female"
temp <- sprintf("%s Morris analysis", sexORrep)
print(temp)

p <- set_params(sexORrep)

if (sexORrep == 'male') {
    # to get testpars, parsbinf, parsbsup
    source("set_morris_all_mf.r")
    source("ca_mod_eqnsMorris_male_all.r")
} else if (sexORrep == 'female') {
    source("set_morris_all_mf.r")
    source("ca_mod_eqnsMorris_male_all.r")
} else if (sexORrep == 'preg') {
    source("set_morris_all_preglact.r")
    source("ca_mod_eqnsMorris_preglact_all.r")
} else if (sexORrep == 'lact') {
    source("set_morris_all_preglact.r")
    source("ca_mod_eqnsMorris_preglact_all.r")
    #modeqns <- ca_mod_eqnsMorris_preglact_all
} else {
    print(sexORrep + " not found")
}

# run Morris Method
set.seed(151)
source("compute_ss.r")
rval = 100
startall <- Sys.time()
# PTHp_con
print('start PTHp morris')
starttemp <- Sys.time()
optsvals_PTHp <- list(
        sexORrep = sexORrep,
        var_out = "PTHp_con"
        )
x_PTHp <- morris(model = compute_ss,
                    factors = testpars,
                    r = rval,
                    design = list(type = "oat",
                                    levels = 10,
                                    grid.jump = 1),
                    binf = parsbinf,
                    bsup = parsbsup,
                    scale = TRUE,
                    optvals = optsvals_PTHp
                    )

endtemp <- Sys.time()
print(difftime(endtemp, starttemp, unit = "mins"))

# Cap_con
print('start Cap morris')
starttemp <- Sys.time()
optsvals_Cap <- list(
        sexORrep = sexORrep,
        var_out = "Cap_con"
        )
x_Cap <- morris(model = compute_ss,
            factors = testpars,
            r = rval,
            design = list(type = "oat",
                            levels = 10,
                            grid.jump = 1),
            binf = parsbinf,
            bsup = parsbsup,
            scale = TRUE,
            optvals = optsvals_Cap
            )
endtemp <- Sys.time()
print(difftime(endtemp, starttemp, unit = "mins"))

# D3p_con
print('start D3p morris')
starttemp <- Sys.time()
optsvals_D3p <- list(
        sexORrep = sexORrep,
        var_out = "D3p_con"
        )
x_D3p <- morris(model = compute_ss,
            factors = testpars,
            r = rval,
            design = list(type = "oat",
                            levels = 10,
                            grid.jump = 1),
            binf = parsbinf,
            bsup = parsbsup,
            scale = TRUE,
            optvals = optsvals_D3p
            )
endtemp <- Sys.time()
print(difftime(endtemp, starttemp, unit = "mins"))

end_all <- Sys.time()
print(difftime(end_all, startall, units = "mins"))

save_info = 1
if (save_info) {
    today <- Sys.Date()
    fname <- paste0(today,
                    "_CaMorrisAnalysisSS_",
                    "_sexORrep-", sexORrep,
                    ".RData"
                    )
    save.image(fname)
    print("results saved to:")
    print(sprintf("%s",fname))
}