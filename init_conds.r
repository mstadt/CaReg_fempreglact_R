init_conds <- function(sexORrep) {
    if (sexORrep == 'male') {
    list(
        amt_PTHg = 34.986,
        PTHp_con = 6.279,
        Cap_con = 1.179,
        D3p_con = 153.688,
        amt_NCaf = 1.626
        )
    } else if ( sexORrep == 'female') {
    list(
        amt_PTHg = 35.167,
        PTHp_con = 6.288,
        Cap_con = 1.179,
        D3p_con = 84.38,
        amt_NCaf = 1.626
        )
    } else {
        temp = sprintf('%s init cond not available', sexORrep)
        print(temp)
    }
}