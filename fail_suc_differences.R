##descriptive differences between failing and successful sequences

library(dplyr)
library(tidyr)
library(seqR)
library(ggplot2)

setwd("~/Documents/Fall 2017/FASDEM/")

vdem <- readRDS("~/Dropbox/V-Dem Research and DS for Research/V-Dem data for Analysis/V-Dem Datasets/v7.1/Team DS/V-Dem-DS-CY-v7.1.rds")
vdem$e_v2x_polyarchy_5C <- vdem$e_v2x_polyarchy_5C *4

vdem_seq <- vdem %>%
  fill(starts_with("v2el")) %>%
  dplyr::select_("e_v2x_polyarchy_5C", "year", "country_name", "v2mecenefi_ord", "v2mecenefi_ord", "v2meharjrn_ord", "v2meslfcen_ord","v2mebias_ord", "v2mecrit_ord", "v2merange_ord", 
                 "v2cldiscm_ord","v2cldiscw_ord", "v2clacfree_ord", "v2psparban_ord", "v2psbars_ord","v2psoppaut_ord", "v2elmulpar_ord", 
                 "v2cseeorgs_ord", "v2csreprss_ord","e_v2x_suffr_5C", "v2elembaut_ord", "v2elembcap_ord", "v2elrgstry_ord","v2elvotbuy_ord", 
                 "v2elirreg_ord", "v2elintim_ord", "v2elpeace_ord","v2elfrfair_ord") %>%
  dplyr::group_by(country_name) %>%
  dplyr::mutate(seq = findMovement(e_v2x_polyarchy_5C, direction = "up", upper_lim = 3, lower_lim = 2, lbuffer = 3), e_v2x_suffr_ord = e_v2x_suffr_5C * 4) %>%
  filter(seq >= 1) %>%
  ungroup() %>%
  filter(e_v2x_polyarchy_5C <= 3)

vdem_seq2 <- vdem %>%
  fill(starts_with("v2el")) %>%
  dplyr::select_("e_v2x_polyarchy_5C", "year", "country_name", "v2mecenefi_ord", "v2mecenefi_ord", "v2meharjrn_ord", "v2meslfcen_ord","v2mebias_ord", "v2mecrit_ord", "v2merange_ord", 
                 "v2cldiscm_ord","v2cldiscw_ord", "v2clacfree_ord", "v2psparban_ord", "v2psbars_ord","v2psoppaut_ord", "v2elmulpar_ord", 
                 "v2cseeorgs_ord", "v2csreprss_ord","e_v2x_suffr_5C", "v2elembaut_ord", "v2elembcap_ord", "v2elrgstry_ord","v2elvotbuy_ord", 
                 "v2elirreg_ord", "v2elintim_ord", "v2elpeace_ord","v2elfrfair_ord") %>%
  group_by(country_name) %>%
  mutate(seq = findMovement(e_v2x_polyarchy_5C, direction = "up", lower_lim = 2, lbuffer = 3), e_v2x_suffr_ord = e_v2x_suffr_5C * 4) %>%
  filter(seq >= 1) %>%
  group_by(country_name, seq) %>%
  filter(max(e_v2x_polyarchy_5C) < 3) %>%
  ungroup()

vdem_seq_all <- rbind(vdem_seq,vdem_seq2)
vdem_seq_all$suc_seq <- c(rep(1,nrow(vdem_seq)), rep(0,(nrow(vdem_seq_all)-nrow(vdem_seq))))

seq <- dplyr::select(vdem_seq, -country_name, -year, -suc_seq)
seq_low <- as.data.frame(apply(seq, 2, function(x) ifelse(as.numeric(x) >= 2, NA, x)))
#seq_low2 <- seq %>% filter(e_v2x_polyarchy_5C < 2)

seq_high <- as.data.frame(apply(seq, 2, function(x) ifelse(as.numeric(x) <= 2, NA, x)))

ptab <- ptable(dplyr::select(vdem_seq, -country_name, -year, -suc_seq))

ptab_low <- ptable(seq_low)
#ptab_low2 <- ptable(seq_low2)

ptab_high <- ptable(seq_high)

ptab_all <- ptab %>%
  
  
  #set the cutoffs for domination 
  xdom_min <- 60
ydom_max <- 20

#identify the pairs that are dominated by x based on the cutoffs applied above 
ptab$x_dom_y <- ifelse(ptab$`X>Y`>= xdom_min & ptab$`X<Y` <= ydom_max, 1, 0)
ptab_low$x_dom_y <- ifelse(ptab_low$`X>Y`>= xdom_min & ptab_low$`X<Y` <= ydom_max, 1, 0)
#ptab_low2$x_dom_y <- ifelse(ptab_low2$`X>Y`>= xdom_min & ptab_low2$`X<Y` <= ydom_max, 1, 0)

ptab_high$x_dom_y <- ifelse(ptab_high$`X>Y`>= xdom_min & ptab_high$`X<Y` <= ydom_max, 1, 0)

#get a vector of all of the features that dominate another feature  based on the cutoff determined above
dominators <- ptab %>% filter(x_dom_y == 1) %>% select(X) %>% distinct()
dominators_low <- ptab_low %>% filter(x_dom_y == 1) %>% select(X) %>% distinct()
#dominators_low2 <- ptab_low2 %>% filter(x_dom_y == 1) %>% select(X) %>% distinct()

dominators_high <- ptab_high %>% filter(x_dom_y == 1) %>% select(X) %>% distinct()

#differences between groups
dominators_high$X[(dominators_high$X %in% dominators_low$X) == F]
dom_low_only <- dominators_low$X[(dominators_low$X %in% dominators_high$X) == F]
#dominators_low2$X[(dominators_low2$X %in% dominators_low$X) == F]

ptab_low %>% filter(x_dom_y == 1, X %in% dom_low_only)
ptab_high %>% filter(x_dom_y == 1, X %in% dom_high_only)



##for failing sequences

seq2 <- dplyr::select(vdem_seq2, -country_name, -year, -fail_seq)
seq_low2 <- as.data.frame(apply(seq2, 2, function(x) ifelse(as.numeric(x) >= 2, NA, x)))
#seq_low2 <- seq %>% filter(e_v2x_polyarchy_5C < 2)

seq_high2 <- as.data.frame(apply(seq2, 2, function(x) ifelse(as.numeric(x) <= 2, NA, x)))

ptab2 <- ptable(dplyr::select(vdem_seq2, -country_name, -year, -fail_seq))

ptab_low2 <- ptable(seq_low2)
#ptab_low2 <- ptable(seq_low2)

ptab_high2 <- ptable(seq_high2)

#set the cutoffs for domination 
xdom_min <- 60
ydom_max <- 20

#identify the pairs that are dominated by x based on the cutoffs applied above 
ptab2$x_dom_y <- ifelse(ptab2$`X>Y`>= xdom_min & ptab2$`X<Y` <= ydom_max, 1, 0)
ptab_low2$x_dom_y <- ifelse(ptab_low2$`X>Y`>= xdom_min & ptab_low2$`X<Y` <= ydom_max, 1, 0)

ptab_high2$x_dom_y <- ifelse(ptab_high2$`X>Y`>= xdom_min & ptab_high2$`X<Y` <= ydom_max, 1, 0)

#get a vector of all of the features that dominate another feature  based on the cutoff determined above
dominators2 <- ptab2 %>% filter(x_dom_y == 1) %>% select(X) %>% distinct()
dominators_low2 <- ptab_low2 %>% filter(x_dom_y == 1) %>% select(X) %>% distinct()

dominators_high2 <- ptab_high2 %>% filter(x_dom_y == 1) %>% select(X) %>% distinct()

#differences between groups
dom_high_only2 <- dominators_high2$X[(dominators_high2$X %in% dominators_low2$X) == F]
dom_low_only2 <- dominators_low2$X[(dominators_low2$X %in% dominators_high2$X) == F]
dom_low_fail <- dominators_low2$X[(dominators_low2$X %in% dominators_low$X) == F]
dom_low_suc <- dominators_low$X[(dominators_low$X %in% dominators_low2$X) == F]

fail_unique <- ptab_low2 %>% filter(x_dom_y == 1, X %in% dom_low_fail)
suc_unique <- ptab_low %>% filter(x_dom_y == 1, X %in% dom_low_suc)

fail_unique %>%
  left_join(ptab_low, by= c("X" , "Y"))

##bootstrapping to determine significant differences

#need to merge failing and successful sequences- this requires one of the tables to be duplicated with flipped X and Y in order to get a full merge

ptab2b <- ptab2 %>%
  mutate(X_f = Y, Y_f = X, `X>Y_f` = `X<Y`, `X=Y_f` = `X=Y`, `X<Y_f` = `X>Y`) %>%
  select(ends_with("_f"))
colnames(ptab2b) <- gsub("_f", "", colnames(ptab2b))

ptab2_all <- ptab2 %>%
  select(-x_dom_y) %>%
  bind_rows(ptab2b)

ptabs <- ptab %>%
  left_join(ptab2_all, by = c("X", "Y"))

ptabs$diff <- ptabs$`X>Y.x`-ptabs$`X>Y.y`
hist(ptabs$diff)

ptab_diff <- (filter(ptabs, abs(diff) > 10))
ptab_diff <- ptab_diff %>%
  mutate(pair = paste(X,Y, sep = "+"), sd_suc = sqrt((`X>Y.x`/100)*(1-(`X>Y.x`/100))/(nrow(ptab)))*100, sd_fail = sqrt((`X>Y.y`/100)*(1-(`X>Y.y`/100))/(nrow(ptab2)))*100) %>%
  select(pair, success = `X>Y.x`, fail = `X>Y.y`, sd_suc, sd_fail, diff) %>%
  arrange(success)

threshold <- 60
confidence <- 1.64
ptab_diff$cross_suc <- ifelse(ptab_diff$success- confidence*ptab_diff$sd_suc > threshold & ptab_diff$fail + confidence*ptab_diff$sd_fail < 50, 1, 0)
ptab_diff$cross_fail <- ifelse(ptab_diff$success + confidence*ptab_diff$sd_suc < 50 & ptab_diff$fail - confidence*ptab_diff$sd_fail > threshold, 1, 0)
ptab_diff <- filter(ptab_diff, cross_fail == 1 | cross_suc == 1)

plot(ptab_diff$success, 1:nrow(ptab_diff), pch = 16, xlim = c(-15,100), col = "blue", xlab = "pct X dominates Y", ylab = "", yaxt = "n", bty = "n")
#axis(2, at = 1:nrow(ptab_diff), labels = ptab_diff$pair, las = 1)
points(ptab_diff$fail, 1:nrow(ptab_diff), pch = 16, col= "red")
segments(ptab_diff$success,1:nrow(ptab_diff), ptab_diff$fail, 1:nrow(ptab_diff), col = "red")
segments(ptab_diff$success[which(ptab_diff$diff > 0)],which(ptab_diff$diff > 0), ptab_diff$fail[which(ptab_diff$diff > 0)], which(ptab_diff$diff > 0), col = "blue")
segments(ptab_diff$success[which(ptab_diff$cross_suc + ptab_diff$cross_fail == 0)],which(ptab_diff$cross_suc + ptab_diff$cross_fail == 0), ptab_diff$fail[which(ptab_diff$cross_suc + ptab_diff$cross_fail == 0)],which(ptab_diff$cross_suc + ptab_diff$cross_fail == 0), col = "grey60")
text(rep(0,nrow(ptab_diff)), 1:nrow(ptab_diff), ptab_diff$pair, cex = 0.7)
text(rep(0,nrow(ptab_diff[which(ptab_diff$cross_suc == 1),])), which(ptab_diff$cross_suc == 1), ptab_diff$pair[which(ptab_diff$cross_suc == 1)], cex = 0.7, col = "blue")
text(rep(0,nrow(ptab_diff[which(ptab_diff$cross_fail == 1),])), which(ptab_diff$cross_fail == 1), ptab_diff$pair[which(ptab_diff$cross_fail == 1)], cex = 0.7, col = "red")
abline(v = threshold, col = "grey20", lty = 2)
legend("bottomright", c("success", "fail", paste(threshold, "%", sep = "")), pch = c(16, 16, NA), lty = c(1, 1, 2), col= c("blue", "red", "grey20"))

##plot the confidence intervals
plot(ptab_diff$success, 1:nrow(ptab_diff), pch = 16, xlim = c(-15,100), col = "blue", xlab = "pct X dominates Y", ylab = "", yaxt = "n", bty = "n")
abline(h=1:nrow(ptab_diff), col = "grey90", lty =2)
polygon(c(50, threshold, threshold, 50, 50), c(0,0,nrow(ptab_diff),nrow(ptab_diff),0), density = 40, col = "grey90")
points(ptab_diff$success, 1:nrow(ptab_diff), pch = 16, col= "blue")
points(ptab_diff$fail, 1:nrow(ptab_diff), pch = 16, col= "red")
segments(ptab_diff$success- confidence*ptab_diff$sd_suc ,1:nrow(ptab_diff), ptab_diff$success+ confidence*ptab_diff$sd_suc, 1:nrow(ptab_diff), col = "blue", lend = 2)
segments(ptab_diff$fail- confidence*ptab_diff$sd_fail ,1:nrow(ptab_diff), ptab_diff$fail+ confidence*ptab_diff$sd_fail, 1:nrow(ptab_diff), col = "red", lend = 2)
text(rep(0,nrow(ptab_diff)), 1:nrow(ptab_diff), ptab_diff$pair, cex = 0.7)
text(rep(0,nrow(ptab_diff[which(ptab_diff$cross_suc == 1),])), which(ptab_diff$cross_suc == 1), ptab_diff$pair[which(ptab_diff$cross_suc == 1)], cex = 0.7, col = "blue")
text(rep(0,nrow(ptab_diff[which(ptab_diff$cross_fail == 1),])), which(ptab_diff$cross_fail == 1), ptab_diff$pair[which(ptab_diff$cross_fail == 1)], cex = 0.7, col = "red")



# Mann-Whitney test example
wilcox.test(ptabs$`X>Y.x`, ptabs$`X>Y.y`)

levene.test = function(data1, data2){
  levene.trans = function(data){
    a = sort(abs(data-median(data)))
    if (length(a)%%2)
      a[a!=0|duplicated(a)]
    else a
    }
  t.test(levene.trans(data1), levene.trans(data2), var.equal=T)
}

levene.test(ptabs$`X>Y.x`, ptabs$`X>Y.y`)

##pairwise comparisons 

prop_tests <- vector(mode = "list", length = nrow(ptabs))
for (i in 1:nrow(ptabs)){
  prop_tests[[i]] <- prop.test(p = c(ptabs$`X>Y.x`[i]/100, ptabs$`X>Y.y`[i])/100, n = c(nrow(vdem_seq), nrow(vdem_seq2)))
}

