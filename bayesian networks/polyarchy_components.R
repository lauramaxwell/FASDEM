#getting data in shape for BN

#use only the ordinal measures of the components of polyarchy.
library(dplyr)
library(tidyr)
library(bnlearn)
library(bnstruct)
library(Rgraphviz)
library(zoo)
library(seqR)
setwd("~/Documents/Fall 2017/FASDEM/")

vdem <- readRDS("~/Dropbox/V-Dem Research and DS for Research/V-Dem data for Analysis/V-Dem Datasets/v7.1/Team DS/V-Dem-DS-CY-v7.1.rds")
ord <- as.data.frame(vdem %>%
  select_("country_name", "historical_date","e_v2x_polyarchy_5C","v2mecenefi_ord", "v2mecenefi_ord", 
                       "v2meharjrn_ord", "v2meslfcen_ord",
                       "v2mebias_ord", "v2mecrit_ord", "v2merange_ord", "v2cldiscm_ord",
                       "v2cldiscw_ord", "v2clacfree_ord", "v2psparban_ord", "v2psbars_ord",
                       "v2psoppaut_ord", "v2elmulpar_ord", "v2cseeorgs_ord", "v2csreprss_ord",
                       "e_v2x_suffr_5C", "v2elembaut_ord", "v2elembcap_ord", "v2elrgstry_ord",
                       "v2elvotbuy_ord", "v2elirreg_ord", "v2elintim_ord", "v2elpeace_ord",
                       "v2elfrfair_ord")) #%>%
    # distinct_("country_name", "e_v2x_polyarchy_5C","v2mecenefi_ord", "v2mecenefi_ord", 
    #           "v2meharjrn_ord", "v2meslfcen_ord",
    #           "v2mebias_ord", "v2mecrit_ord", "v2merange_ord", "v2cldiscm_ord",
    #           "v2cldiscw_ord", "v2clacfree_ord", "v2psparban_ord", "v2psbars_ord",
    #           "v2psoppaut_ord", "v2elmulpar_ord", "v2cseeorgs_ord", "v2csreprss_ord",
    #           "e_v2x_suffr_5C", "v2elembaut_ord", "v2elembcap_ord", "v2elrgstry_ord",
    #           "v2elvotbuy_ord", "v2elirreg_ord", "v2elintim_ord", "v2elpeace_ord",
    #           "v2elfrfair_ord", .keep_all = T) %>%
    # group_by(country_name) %>%
    # mutate(duration = (lead(historical_date)) - (historical_date), e_v2x_polyarchy_5C = e_v2x_polyarchy_5C * 4, e_v2x_suffr_5C = e_v2x_suffr_5C * 4))

ord_diff <- as.data.frame(ord %>%
                            mutate(e_v2x_polyarchy_5C = e_v2x_polyarchy_5C * 4, e_v2x_suffr_5C = e_v2x_suffr_5C * 4) %>%
                            group_by(country_name) %>%
                            mutate_if(is.numeric, funs(diff = . - lag(., 1))))
ord_diff$total_diff <- apply(ord_diff[,grep("diff", colnames(ord_diff))], 1, function(x) sum(x, na.rm = T))

#ord_lags <- as.data.frame(ord %>%
#                            group_by(country_name) %>%
#                            mutate_if(is.numeric, funs(l1 = lag(. , 1), l2 = lag( . , 2))))
                          
#append indicator for successful sequence

ord_diff_seq <- ord_diff %>%
  dplyr::group_by(country_name) %>%
  dplyr::mutate(suc_seq = findMovement(e_v2x_polyarchy_5C, direction = "up", upper_lim = 3, lbuffer = 3)) %>%
  ungroup() %>%
  filter(e_v2x_polyarchy_5C <= 3, suc_seq >=1) 

ql_dat <- ord_diff_seq[,colnames(ord_diff_seq)[grep("_diff", colnames(ord_diff_seq))]][,1:25]

test <- ql_matrix(ql_dat, colnames(ql_dat), na.rm = T) #this function isn't built for this...need modifications.







#polyarchy changes
poly_up <- filter(ord_diff, e_v2x_polyarchy_5C_diff > 0)
poly_down <- filter(ord_diff, e_v2x_polyarchy_5C_diff < 0)   
#group_by(country_name) %>%
#mutate_if(is.numeric, funs(l1 = lag(. , 1), l2 = lag( . , 2), l3 = lag(. , 3))))

#have it try to learn a BN

ord[is.na(ord)] <- 0
dag <- iamb(ord[,-c(1,2)])

#create a network structure

arc.set = matrix(c("v2mecenefm_ord_l1"  , "v2mecenefm_ord", 
                   "v2mecenefi_ord_l1" , "v2mecenefm_ord", 
                   "v2meharjrn_ord_l1" , "v2mecenefm_ord", 
                   "v2meslfcen_ord_l1" , "v2mecenefm_ord", 
                   "v2mebias_ord_l1" , "v2mecenefm_ord", 
                   "v2mecrit_ord_l1", "v2mecenefm_ord", 
                   "v2merange_ord_l1" , "v2mecenefm_ord",
                   "v2cldiscm_ord_l1" , "v2mecenefm_ord",
                   "v2cldiscw_ord_l1" , "v2mecenefm_ord",
                   "v2clacfree_ord_l1" , "v2mecenefm_ord",
                   "v2psparban_ord_l1"  , "v2mecenefm_ord",
                   "v2psbars_ord_l1"  , "v2mecenefm_ord",
                   "v2psoppaut_ord_l1" , "v2mecenefm_ord",
                   "v2elmulpar_ord_l1"  , "v2mecenefm_ord",
                   "v2cseeorgs_ord_l1" , "v2mecenefm_ord",
                   "v2elfrfair_ord_l1" , "v2mecenefm_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2mecenefi_ord", 
                   "v2mecenefi_ord_l1" , "v2mecenefi_ord", 
                   "v2meharjrn_ord_l1" , "v2mecenefi_ord", 
                   "v2meslfcen_ord_l1" , "v2mecenefi_ord", 
                   "v2mebias_ord_l1" , "v2mecenefi_ord", 
                   "v2mecrit_ord_l1", "v2mecenefi_ord", 
                   "v2merange_ord_l1" , "v2mecenefi_ord",
                   "v2cldiscm_ord_l1" , "v2mecenefi_ord",
                   "v2cldiscw_ord_l1" , "v2mecenefi_ord",
                   "v2clacfree_ord_l1" , "v2mecenefi_ord",
                   "v2psparban_ord_l1"  , "v2mecenefi_ord",
                   "v2psbars_ord_l1"  , "v2mecenefi_ord",
                   "v2psoppaut_ord_l1" , "v2mecenefi_ord",
                   "v2elmulpar_ord_l1"  , "v2mecenefi_ord",
                   "v2cseeorgs_ord_l1" , "v2mecenefi_ord",
                   "v2elfrfair_ord_l1" , "v2mecenefi_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2meharjrn_ord", 
                   "v2mecenefi_ord_l1" , "v2meharjrn_ord", 
                   "v2meharjrn_ord_l1" , "v2meharjrn_ord", 
                   "v2meslfcen_ord_l1" , "v2meharjrn_ord", 
                   "v2mebias_ord_l1" , "v2meharjrn_ord", 
                   "v2mecrit_ord_l1", "v2meharjrn_ord", 
                   "v2merange_ord_l1" , "v2meharjrn_ord",
                   "v2cldiscm_ord_l1" , "v2meharjrn_ord",
                   "v2cldiscw_ord_l1" , "v2meharjrn_ord",
                   "v2clacfree_ord_l1" , "v2meharjrn_ord",
                   "v2psparban_ord_l1"  , "v2meharjrn_ord",
                   "v2psbars_ord_l1"  , "v2meharjrn_ord",
                   "v2psoppaut_ord_l1" , "v2meharjrn_ord",
                   "v2elmulpar_ord_l1"  , "v2meharjrn_ord",
                   "v2cseeorgs_ord_l1" , "v2meharjrn_ord",
                   "v2elfrfair_ord_l1" , "v2meharjrn_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2meslfcen_ord", 
                   "v2mecenefi_ord_l1" , "v2meslfcen_ord", 
                   "v2meharjrn_ord_l1" , "v2meslfcen_ord", 
                   "v2meslfcen_ord_l1" , "v2meslfcen_ord", 
                   "v2mebias_ord_l1" , "v2meslfcen_ord", 
                   "v2mecrit_ord_l1", "v2meslfcen_ord", 
                   "v2merange_ord_l1" , "v2meslfcen_ord",
                   "v2cldiscm_ord_l1" , "v2meslfcen_ord",
                   "v2cldiscw_ord_l1" , "v2meslfcen_ord",
                   "v2clacfree_ord_l1" , "v2meslfcen_ord",
                   "v2psparban_ord_l1"  , "v2meslfcen_ord",
                   "v2psbars_ord_l1"  , "v2meslfcen_ord",
                   "v2psoppaut_ord_l1" , "v2meslfcen_ord",
                   "v2elmulpar_ord_l1"  , "v2meslfcen_ord",
                   "v2cseeorgs_ord_l1" , "v2meslfcen_ord",
                   "v2elfrfair_ord_l1" , "v2meslfcen_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2mebias_ord", 
                   "v2mecenefi_ord_l1" , "v2mebias_ord", 
                   "v2meharjrn_ord_l1" , "v2mebias_ord", 
                   "v2meslfcen_ord_l1" , "v2mebias_ord", 
                   "v2mebias_ord_l1" , "v2mebias_ord", 
                   "v2mecrit_ord_l1", "v2mebias_ord", 
                   "v2merange_ord_l1" , "v2mebias_ord",
                   "v2cldiscm_ord_l1" , "v2mebias_ord",
                   "v2cldiscw_ord_l1" , "v2mebias_ord",
                   "v2clacfree_ord_l1" , "v2mebias_ord",
                   "v2psparban_ord_l1"  , "v2mebias_ord",
                   "v2psbars_ord_l1"  , "v2mebias_ord",
                   "v2psoppaut_ord_l1" , "v2mebias_ord",
                   "v2elmulpar_ord_l1"  , "v2mebias_ord",
                   "v2cseeorgs_ord_l1" , "v2mebias_ord",
                   "v2elfrfair_ord_l1" , "v2mebias_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2mecrit_ord", 
                   "v2mecenefi_ord_l1" , "v2mecrit_ord", 
                   "v2meharjrn_ord_l1" , "v2mecrit_ord", 
                   "v2meslfcen_ord_l1" , "v2mecrit_ord", 
                   "v2mebias_ord_l1" , "v2mecrit_ord", 
                   "v2mecrit_ord_l1", "v2mecrit_ord", 
                   "v2merange_ord_l1" , "v2mecrit_ord",
                   "v2cldiscm_ord_l1" , "v2mecrit_ord",
                   "v2cldiscw_ord_l1" , "v2mecrit_ord",
                   "v2clacfree_ord_l1" , "v2mecrit_ord",
                   "v2psparban_ord_l1"  , "v2mecrit_ord",
                   "v2psbars_ord_l1"  , "v2mecrit_ord",
                   "v2psoppaut_ord_l1" , "v2mecrit_ord",
                   "v2elmulpar_ord_l1"  , "v2mecrit_ord",
                   "v2cseeorgs_ord_l1" , "v2mecrit_ord",
                   "v2elfrfair_ord_l1" , "v2mecrit_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2merange_ord", 
                   "v2mecenefi_ord_l1" , "v2merange_ord", 
                   "v2meharjrn_ord_l1" , "v2merange_ord", 
                   "v2meslfcen_ord_l1" , "v2merange_ord", 
                   "v2mebias_ord_l1" , "v2merange_ord", 
                   "v2mecrit_ord_l1", "v2merange_ord", 
                   "v2merange_ord_l1" , "v2merange_ord",
                   "v2cldiscm_ord_l1" , "v2merange_ord",
                   "v2cldiscw_ord_l1" , "v2merange_ord",
                   "v2clacfree_ord_l1" , "v2merange_ord",
                   "v2psparban_ord_l1"  , "v2merange_ord",
                   "v2psbars_ord_l1"  , "v2merange_ord",
                   "v2psoppaut_ord_l1" , "v2merange_ord",
                   "v2elmulpar_ord_l1"  , "v2merange_ord",
                   "v2cseeorgs_ord_l1" , "v2merange_ord",
                   "v2elfrfair_ord_l1" , "v2merange_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2cldiscm_ord", 
                   "v2mecenefi_ord_l1" , "v2cldiscm_ord", 
                   "v2meharjrn_ord_l1" , "v2cldiscm_ord", 
                   "v2meslfcen_ord_l1" , "v2cldiscm_ord", 
                   "v2mebias_ord_l1" , "v2cldiscm_ord", 
                   "v2mecrit_ord_l1", "v2cldiscm_ord", 
                   "v2merange_ord_l1" , "v2cldiscm_ord",
                   "v2cldiscm_ord_l1" , "v2cldiscm_ord",
                   "v2cldiscw_ord_l1" , "v2cldiscm_ord",
                   "v2clacfree_ord_l1" , "v2cldiscm_ord",
                   "v2psparban_ord_l1"  , "v2cldiscm_ord",
                   "v2psbars_ord_l1"  , "v2cldiscm_ord",
                   "v2psoppaut_ord_l1" , "v2cldiscm_ord",
                   "v2elmulpar_ord_l1"  , "v2cldiscm_ord",
                   "v2cseeorgs_ord_l1" , "v2cldiscm_ord",
                   "v2elfrfair_ord_l1" , "v2cldiscm_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2cldiscw_ord", 
                   "v2mecenefi_ord_l1" , "v2cldiscw_ord", 
                   "v2meharjrn_ord_l1" , "v2cldiscw_ord", 
                   "v2meslfcen_ord_l1" , "v2cldiscw_ord", 
                   "v2mebias_ord_l1" , "v2cldiscw_ord", 
                   "v2mecrit_ord_l1", "v2cldiscw_ord", 
                   "v2merange_ord_l1" , "v2cldiscw_ord",
                   "v2cldiscm_ord_l1" , "v2cldiscw_ord",
                   "v2cldiscw_ord_l1" , "v2cldiscw_ord",
                   "v2clacfree_ord_l1" , "v2cldiscw_ord",
                   "v2psparban_ord_l1"  , "v2cldiscw_ord",
                   "v2psbars_ord_l1"  , "v2cldiscw_ord",
                   "v2psoppaut_ord_l1" , "v2cldiscw_ord",
                   "v2elmulpar_ord_l1"  , "v2cldiscw_ord",
                   "v2cseeorgs_ord_l1" , "v2cldiscw_ord",
                   "v2elfrfair_ord_l1" , "v2cldiscw_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2clacfree_ord", 
                   "v2mecenefi_ord_l1" , "v2clacfree_ord", 
                   "v2meharjrn_ord_l1" , "v2clacfree_ord", 
                   "v2meslfcen_ord_l1" , "v2clacfree_ord", 
                   "v2mebias_ord_l1" , "v2clacfree_ord", 
                   "v2mecrit_ord_l1", "v2clacfree_ord", 
                   "v2merange_ord_l1" , "v2clacfree_ord",
                   "v2cldiscm_ord_l1" , "v2clacfree_ord",
                   "v2cldiscw_ord_l1" , "v2clacfree_ord",
                   "v2clacfree_ord_l1" , "v2clacfree_ord",
                   "v2psparban_ord_l1"  , "v2clacfree_ord",
                   "v2psbars_ord_l1"  , "v2clacfree_ord",
                   "v2psoppaut_ord_l1" , "v2clacfree_ord",
                   "v2elmulpar_ord_l1"  , "v2clacfree_ord",
                   "v2cseeorgs_ord_l1" , "v2clacfree_ord",
                   "v2elfrfair_ord_l1" , "v2clacfree_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2psparban_ord", 
                   "v2mecenefi_ord_l1" , "v2psparban_ord", 
                   "v2meharjrn_ord_l1" , "v2psparban_ord", 
                   "v2meslfcen_ord_l1" , "v2psparban_ord", 
                   "v2mebias_ord_l1" , "v2psparban_ord", 
                   "v2mecrit_ord_l1", "v2psparban_ord", 
                   "v2merange_ord_l1" , "v2psparban_ord",
                   "v2cldiscm_ord_l1" , "v2psparban_ord",
                   "v2cldiscw_ord_l1" , "v2psparban_ord",
                   "v2clacfree_ord_l1" , "v2psparban_ord",
                   "v2psparban_ord_l1"  , "v2psparban_ord",
                   "v2psbars_ord_l1"  , "v2psparban_ord",
                   "v2psoppaut_ord_l1" , "v2psparban_ord",
                   "v2elmulpar_ord_l1"  , "v2psparban_ord",
                   "v2cseeorgs_ord_l1" , "v2psparban_ord",
                   "v2elfrfair_ord_l1" , "v2psparban_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2psbars_ord", 
                   "v2mecenefi_ord_l1" , "v2psbars_ord", 
                   "v2meharjrn_ord_l1" , "v2psbars_ord", 
                   "v2meslfcen_ord_l1" , "v2psbars_ord", 
                   "v2mebias_ord_l1" , "v2psbars_ord", 
                   "v2mecrit_ord_l1", "v2psbars_ord", 
                   "v2merange_ord_l1" , "v2psbars_ord",
                   "v2cldiscm_ord_l1" , "v2psbars_ord",
                   "v2cldiscw_ord_l1" , "v2psbars_ord",
                   "v2clacfree_ord_l1" , "v2psbars_ord",
                   "v2psparban_ord_l1"  , "v2psbars_ord",
                   "v2psbars_ord_l1"  , "v2psbars_ord",
                   "v2psoppaut_ord_l1" , "v2psbars_ord",
                   "v2elmulpar_ord_l1"  , "v2psbars_ord",
                   "v2cseeorgs_ord_l1" , "v2psbars_ord",
                   "v2elfrfair_ord_l1" , "v2psbars_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2psoppaut_ord", 
                   "v2mecenefi_ord_l1" , "v2psoppaut_ord", 
                   "v2meharjrn_ord_l1" , "v2psoppaut_ord", 
                   "v2meslfcen_ord_l1" , "v2psoppaut_ord", 
                   "v2mebias_ord_l1" , "v2psoppaut_ord", 
                   "v2mecrit_ord_l1", "v2psoppaut_ord", 
                   "v2merange_ord_l1" , "v2psoppaut_ord",
                   "v2cldiscm_ord_l1" , "v2psoppaut_ord",
                   "v2cldiscw_ord_l1" , "v2psoppaut_ord",
                   "v2clacfree_ord_l1" , "v2psoppaut_ord",
                   "v2psparban_ord_l1"  , "v2psoppaut_ord",
                   "v2psbars_ord_l1"  , "v2psoppaut_ord",
                   "v2psoppaut_ord_l1" , "v2psoppaut_ord",
                   "v2elmulpar_ord_l1"  , "v2psoppaut_ord",
                   "v2cseeorgs_ord_l1" , "v2psoppaut_ord",
                   "v2elfrfair_ord_l1" , "v2psoppaut_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2elmulpar_ord", 
                   "v2mecenefi_ord_l1" , "v2elmulpar_ord", 
                   "v2meharjrn_ord_l1" , "v2elmulpar_ord", 
                   "v2meslfcen_ord_l1" , "v2elmulpar_ord", 
                   "v2mebias_ord_l1" , "v2elmulpar_ord", 
                   "v2mecrit_ord_l1", "v2elmulpar_ord", 
                   "v2merange_ord_l1" , "v2elmulpar_ord",
                   "v2cldiscm_ord_l1" , "v2elmulpar_ord",
                   "v2cldiscw_ord_l1" , "v2elmulpar_ord",
                   "v2clacfree_ord_l1" , "v2elmulpar_ord",
                   "v2psparban_ord_l1"  , "v2elmulpar_ord",
                   "v2psbars_ord_l1"  , "v2elmulpar_ord",
                   "v2psoppaut_ord_l1" , "v2elmulpar_ord",
                   "v2elmulpar_ord_l1"  , "v2elmulpar_ord",
                   "v2cseeorgs_ord_l1" , "v2elmulpar_ord",
                   "v2elfrfair_ord_l1" , "v2elmulpar_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2cseeorgs_ord", 
                   "v2mecenefi_ord_l1" , "v2cseeorgs_ord", 
                   "v2meharjrn_ord_l1" , "v2cseeorgs_ord", 
                   "v2meslfcen_ord_l1" , "v2cseeorgs_ord", 
                   "v2mebias_ord_l1" , "v2cseeorgs_ord", 
                   "v2mecrit_ord_l1", "v2cseeorgs_ord", 
                   "v2merange_ord_l1" , "v2cseeorgs_ord",
                   "v2cldiscm_ord_l1" , "v2cseeorgs_ord",
                   "v2cldiscw_ord_l1" , "v2cseeorgs_ord",
                   "v2clacfree_ord_l1" , "v2cseeorgs_ord",
                   "v2psparban_ord_l1"  , "v2cseeorgs_ord",
                   "v2psbars_ord_l1"  , "v2cseeorgs_ord",
                   "v2psoppaut_ord_l1" , "v2cseeorgs_ord",
                   "v2elmulpar_ord_l1"  , "v2cseeorgs_ord",
                   "v2cseeorgs_ord_l1" , "v2cseeorgs_ord",
                   "v2elfrfair_ord_l1" , "v2cseeorgs_ord",
                   
                   "v2mecenefm_ord_l1"  , "v2elfrfair_ord", 
                   "v2mecenefi_ord_l1" , "v2elfrfair_ord", 
                   "v2meharjrn_ord_l1" , "v2elfrfair_ord", 
                   "v2meslfcen_ord_l1" , "v2elfrfair_ord", 
                   "v2mebias_ord_l1" , "v2elfrfair_ord", 
                   "v2mecrit_ord_l1", "v2elfrfair_ord", 
                   "v2merange_ord_l1" , "v2elfrfair_ord",
                   "v2cldiscm_ord_l1" , "v2elfrfair_ord",
                   "v2cldiscw_ord_l1" , "v2elfrfair_ord",
                   "v2clacfree_ord_l1" , "v2elfrfair_ord",
                   "v2psparban_ord_l1"  , "v2elfrfair_ord",
                   "v2psbars_ord_l1"  , "v2elfrfair_ord",
                   "v2psoppaut_ord_l1" , "v2elfrfair_ord",
                   "v2elmulpar_ord_l1"  , "v2elfrfair_ord",
                   "v2cseeorgs_ord_l1" , "v2elfrfair_ord",
                   "v2elfrfair_ord_l1" , "v2elfrfair_ord"
                   ),
                 ncol = 2, 
                 byrow = TRUE,
                 dimnames = list(NULL, c("from", "to")))
#could also make it with the model2network("[A][C][B|A][D|C][F|A:B:C][E|F]") command
bn <- empty.graph(c(unique(arc.set[,1]), unique(arc.set[,2])))
arcs(bn) <- arc.set

ord_lag_bn <- ord_lags[,c(unique(arc.set[,1]), unique(arc.set[,2]))]
ord_lag_bn[is.na(ord_lag_bn)] <- 0
for (i in 1:ncol(ord_lag_bn)){
  ord_lag_bn[,i] <- as.factor(ord_lag_bn[,i])
}

fit <- bn.fit(bn, ord_lag_bn)
