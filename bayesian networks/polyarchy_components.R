#getting data in shape for BN

#use only the ordinal measures of the components of polyarchy.
library(dplyr)
library(tidyr)
library(bnlearn)
library(bnstruct)
library(Rgraphviz)
setwd("~/Documents/Fall 2017/FASDEM/bayesian networks/")

vdem <- readRDS("~/Dropbox/V-Dem Research and DS for Research/V-Dem data for Analysis/V-Dem Datasets/v7.1/Team DS/V-Dem-DS-CY-v7.1.rds")
ord <- as.data.frame(vdem %>%
  select_("country_name", "historical_date","e_v2x_polyarchy_5C","v2mecenefm_ord", "v2mecenefi_ord", 
                       "v2meharjrn_ord", "v2meslfcen_ord",
                       "v2mebias_ord", "v2mecrit_ord", "v2merange_ord", "v2cldiscm_ord",
                       "v2cldiscw_ord", "v2clacfree_ord", "v2psparban_ord", "v2psbars_ord",
                       "v2psoppaut_ord", "v2elmulpar_ord", "v2cseeorgs_ord", "v2csreprss_ord",
                       "e_v2x_suffr_5C", "v2elembaut_ord", "v2elembcap_ord", "v2elrgstry_ord",
                       "v2elvotbuy_ord", "v2elirreg_ord", "v2elintim_ord", "v2elpeace_ord",
                       "v2elfrfair_ord") %>%
    distinct_("country_name", "e_v2x_polyarchy_5C","v2mecenefm_ord", "v2mecenefi_ord", 
              "v2meharjrn_ord", "v2meslfcen_ord",
              "v2mebias_ord", "v2mecrit_ord", "v2merange_ord", "v2cldiscm_ord",
              "v2cldiscw_ord", "v2clacfree_ord", "v2psparban_ord", "v2psbars_ord",
              "v2psoppaut_ord", "v2elmulpar_ord", "v2cseeorgs_ord", "v2csreprss_ord",
              "e_v2x_suffr_5C", "v2elembaut_ord", "v2elembcap_ord", "v2elrgstry_ord",
              "v2elvotbuy_ord", "v2elirreg_ord", "v2elintim_ord", "v2elpeace_ord",
              "v2elfrfair_ord", .keep_all = T) %>%
    group_by(country_name) %>%
    mutate(duration = year(lead(historical_date)) - year(historical_date)))

ord_diff <- as.data.frame(ord %>%
  group_by(country_name) %>%
  mutate_if(is.numeric, funs(diff = . - lag(., 1))) %>%
  select(-duration_diff))

ord_lags <- as.data.frame(ord %>%
                            group_by(country_name) %>%
                            mutate_if(is.numeric, funs(l1 = lag(. , 1), l2 = lag( . , 2))))
                          

#polyarchy changes
poly_up <- filter(ord_diff, e_v2x_polyarchy_5C_diff > 0)
poly_down <- filter(ord_diff, e_v2x_polyarchy_5C_diff < 0)   
#group_by(country_name) %>%
#mutate_if(is.numeric, funs(l1 = lag(. , 1), l2 = lag( . , 2), l3 = lag(. , 3))))

#have it try to learn a BN

ord[is.na(ord)] <- 0
dag <- iamb(ord[,-c(1,2)])
