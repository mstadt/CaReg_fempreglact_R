init_conds <- function() {
    Vp <- 0.01
    IC_PTHp <- 6.279 * Vp
    IC_Cap <- 1.179 * Vp
    IC_D3p <- 153.688 * Vp
    list(
        amt_PTHg = 34.986,
        amt_PTHp = IC_PTHp,
        amt_Cap = IC_Cap,
        amt_D3p = IC_D3p,
        
        amt_NCaf = 1.626
    )
}