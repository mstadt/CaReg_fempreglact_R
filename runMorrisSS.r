library(rootSolve)
library(sensitivity)
source("set_params.r")


sexORrep <- "lact"
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
    #source("set_morris_all_preglact.r")
    #source("ca_mod_eqnsMorris_preglact_all.r")
    #source("set_morris_lact.r")
    #source("set_morris_preglact.r") # old script I did... maybe will help?
    #source("ca_mod_eqnsMorris_lact.r")
    #modeqns <- ca_mod_eqnsMorris_preglact_all
    source("set_morris_all_preglact.r")
    source("ca_mod_eqnsMorris_preglact_all.r")
} else {
    print(sexORrep + " not found")
}

# run Morris Method
set.seed(151)
source("compute_ss.r")
rval = 100
startall <- Sys.time()
print(startall)
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

# compute mu, mu*, sigma
x <- x_PTHp
x_PTHp$mu <- apply(x$ee, 2, mean)
x_PTHp$mu.star <- apply(x$ee, 2, function(x) mean(abs(x)))
x_PTHp$sigma <- apply(x$ee, 2, sd)

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
# compute mu, mu*, sigma
x <- x_Cap
x_Cap$mu <- apply(x$ee, 2, mean)
x_Cap$mu.star <- apply(x$ee, 2, function(x) mean(abs(x)))
x_Cap$sigma <- apply(x$ee, 2, sd)

# # D3p_con
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
# compute mu, mu*, sigma
x <- x_D3p
x_D3p$mu <- apply(x$ee, 2, mean)
x_D3p$mu.star <- apply(x$ee, 2, function(x) mean(abs(x)))
x_D3p$sigma <- apply(x$ee, 2, sd)

print('Morris Analysis complete')
end_all <- Sys.time()
print(difftime(end_all, startall, units = "mins"))

save_info = 0
if (save_info) {
    today <- Sys.Date()
    fname <- paste0(today,
                    "_CaMorrisAnalysisSS",
                    "_sexORrep-", sexORrep,
                    ".RData"
                    )
    save.image(fname)
    print("results saved to:")
    print(sprintf("%s",fname))
}