##use the output of qtable to make more complicated ptables, cycling through all combinations of dominations
library(dplyr)
library(tidyr)
library(seqR)
library(ggplot2)

setwd("~/Documents/Fall 2017/FASDEM/")

vdem <- readRDS("~/Dropbox/V-Dem Research and DS for Research/V-Dem data for Analysis/V-Dem Datasets/v7.1/Team DS/V-Dem-DS-CY-v7.1.rds")
vdem$e_v2x_polyarchy_5C <- vdem$e_v2x_polyarchy_5C *4

vdem_seq <- vdem %>%
  dplyr::select_("e_v2x_polyarchy_5C", "year", "country_name", "v2mecenefi", "v2mecenefi", "v2meharjrn", "v2meslfcen","v2mebias", "v2mecrit", "v2merange", 
                 "v2cldiscm","v2cldiscw", "v2clacfree", "v2psparban", "v2psbars","v2psoppaut", "v2elmulpar", 
                 "v2cseeorgs", "v2csreprss","v2x_suffr", "v2elembaut", "v2elembcap", "v2elrgstry","v2elvotbuy", 
                 "v2elirreg", "v2elintim", "v2elpeace","v2elfrfair") %>%
  dplyr::group_by(country_name) %>%
  dplyr::mutate(suc_seq = findMovement(e_v2x_polyarchy_5C, direction = "up", upper_lim = 3, lower_lim = 2, lbuffer = 3)) %>%
  filter(suc_seq >= 1) %>%
  ungroup() %>%
  filter(e_v2x_polyarchy_5C <= 3)

vdem_seq2 <- vdem %>%
  dplyr::select_("e_v2x_polyarchy_5C", "year", "country_name", "v2mecenefi_ord", "v2mecenefi_ord", "v2meharjrn_ord", "v2meslfcen_ord","v2mebias_ord", "v2mecrit_ord", "v2merange_ord", 
                 "v2cldiscm_ord","v2cldiscw_ord", "v2clacfree_ord", "v2psparban_ord", "v2psbars_ord","v2psoppaut_ord", "v2elmulpar_ord", 
                 "v2cseeorgs_ord", "v2csreprss_ord","e_v2x_suffr_5C", "v2elembaut_ord", "v2elembcap_ord", "v2elrgstry_ord","v2elvotbuy_ord", 
                 "v2elirreg_ord", "v2elintim_ord", "v2elpeace_ord","v2elfrfair_ord") %>%
  group_by(country_name) %>%
  mutate(fail_seq = findMovement(e_v2x_polyarchy_5C, direction = "up", lower_lim = 2, lbuffer = 3), e_v2x_suffr_ord = e_v2x_suffr_5C * 4) %>%
  filter(fail_seq >= 1) %>%
  group_by(country_name, fail_seq) %>%
  filter(max(e_v2x_polyarchy_5C) < 3)

ptab <- ptable(dplyr::select(vdem_seq, -e_v2x_polyarchy_5C, -country_name, -year))

ptab_low <- ptable(dplyr::select(vdem_seq, -e_v2x_polyarchy_5C, -country_name, -year))

#set the cutoffs for domination 
xdom_min <- 60
ydom_max <- 20

#identify the pairs that are dominated by x based on the cutoffs applied above 
ptab$x_dom_y <- ifelse(ptab$`X>Y`>= xdom_min & ptab$`X<Y` <= ydom_max, 1, 0)

#get a vector of all of the features that dominate another feature  based on the cutoff determined above
dominators <- ptab %>% filter(x_dom_y == 1) %>% select(X) %>% distinct()

sequences <- vector("list", length = nrow(dominators))
names(sequences) <- dominators$X
for (i in dominators$X){
  dat <- filter(ptab, X == i, x_dom_y == 1)
  if(sum(dat$Y %in% dominators$X) > 0) {
   next_vec <- dat$Y[which(dat$Y %in% dominators$X)] 
   sub <- vector("list", length = length(next_vec))
   names(sub) <- next_vec
   for (j in next_vec){
     dat2 <- filter(ptab, X == j, x_dom_y == 1)
     if(sum(dat2$Y %in% dominators$X) > 0) {
       next_vec2 <- dat$Y[which(dat2$Y %in% dominators$X)] 
       sub2 <- vector("list", length = length(next_vec2))
       names(sub2) <- next_vec2
       for (k in next_vec2){
         dat3 <- filter(ptab, X == k, x_dom_y == 1)
         if(sum(dat3$Y %in% dominators$X) > 0) {
           next_vec3 <- dat$Y[which(dat3$Y %in% dominators$X)] 
           sub3 <- vector("list", length = length(next_vec3))
           names(sub3) <- next_vec3
           for (l in next_vec3){
             dat4 <- filter(ptab, X == l, x_dom_y == 1)
             if(sum(dat4$Y %in% dominators$X) > 0) {
                next_vec4 <- dat$Y[which(dat4$Y %in% dominators$X)] 
                sub4 <- vector("list", length = length(next_vec4))
                names(sub4) <- next_vec4
                  for (m in next_vec4){
                    dat5 <- filter(ptab, X == l, x_dom_y == 1)
                    if(sum(dat5$Y %in% dominators$X) > 0) {
                      next_vec5 <- dat$Y[which(dat5$Y %in% dominators$X)] 
                      sub5 <- vector("list", length = length(next_vec5))
                      names(sub5) <- next_vec5
                      sub4[[m]] <- sub5
                      }
                    else
                      sub4[[m]] <- dat5
                    }
                sub3[[l]] <- sub4
                }
             
             }
           else
             sub3[[l]] <- dat4
           }
         sub2[[k]] <- sub3
         }
       else
         sub2[[k]] <- dat3
       }
     sub[[j]] <- sub2
     }
   else
     sub[[j]] <- dat2
   }
  sequences[[i]] <- sub}

else
  sequences[[i]] <- dat
}



###another way...try to make some vectors for each sequence
vdem_seq <- as.data.frame(vdem_seq %>%
  dplyr::group_by(country_name, suc_seq) %>%
  dplyr::mutate(order = year - first(year)) %>%
  dplyr::ungroup()
)
  
slots <- do.call("c",lapply(colnames(vdem_seq[4:27]), function(x) paste(x, 0:4, sep = "_")))
seq_dat <- as.data.frame(vdem_seq %>%
  dplyr::group_by(country_name, suc_seq) %>%
  dplyr::summarize(byear = first(year), duration = max(year) - min(year), init_polyarchy = first(e_v2x_polyarchy_5C)) %>%
  ungroup()
)
for (i in 1:length(slots)){
  seq_dat[,i+5] <- NA
}
colnames(seq_dat)[6:125] <- slots

for (i in unique(vdem_seq$country_name)){
  dat <- filter(vdem_seq, country_name == i) 
  for (j in 1:max(dat$suc_seq)){
    sub_dat <- filter(dat, suc_seq == j)
    for (k in colnames(vdem_seq)[4:27]){
      nums <- na.omit(sub_dat[,c(k, "order")] %>% distinct_(k, .keep_all = T))
      if(nrow(nums) > 0){
        for (l in 1:nrow(nums)){
          col <- paste(k,nums[l,1], sep = "_")
          seq_dat[seq_dat$country_name == i & seq_dat$suc_seq == j,][,col] <- nums[l,2]
        }
      }
    }
  }
}

seq_dat <- seq_dat[, !apply(is.na(seq_dat), 2, all)]
write.csv(seq_dat, "~/Documents/Fall 2017/FASDEM/sequence_vectors.csv")

##doing some clustering on these vectors in a few different ways.

seq_dat_prop <- seq_dat
seq_dat_prop[,6:120] <- seq_dat_prop[,6:120]/seq_dat_prop$duration
seq_dat_prop[is.na(seq_dat_prop)] <- 99  


fit <- kmeans(seq_dat_prop[,4:119], 5)
table(fit$cluster)
seq_dat_prop$cluster <- fit$cluster

#ward hierarchical clustering
d <- dist(seq_dat_prop[,4:119], method= "manhattan")
fit2 <- hclust(d, method="ward") 
plot(fit2, labels = paste(seq_dat_prop$country_name, seq_dat_prop$suc_seq, sep = "_"))
groups <- cutree(fit2, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit2, k=5, border="red")

dist <- 1/d # one over, as qgraph takes similarity matrices as input
library(qgraph)

jpeg('example_forcedraw.jpg', width=1000, height=1000, unit='px')
qgraph(dist, layout='spring', vsize=3, minimum = 0.02)


#expectation maximization approach
fit3 <- Mclust(seq_dat_prop[,4:124])
plot(fit3) # plot results 
summary(fit3) 

###changing the construction of the vector so that it's min_feature, max_feature, time to get there, t of first change

endings <- c("min","max", "time_to_max", "first_move")
slots2 <- do.call("c",lapply(colnames(vdem_seq[4:27]), function(x) paste(x, endings, sep = "_")))
seq_dat2 <- as.data.frame(vdem_seq %>%
  dplyr::group_by(country_name, suc_seq) %>%
  dplyr::summarize(byear = first(year), duration = max(year) - min(year), init_polyarchy = first(e_v2x_polyarchy_5C)) %>%
  ungroup()
)
for (i in 1:length(slots2)){
  seq_dat2[,i+5] <- NA
}
colnames(seq_dat2)[6:101] <- slots2

for (i in unique(vdem_seq$country_name)){
  dat <- filter(vdem_seq, country_name == i) 
  for (j in 1:max(dat$suc_seq)){
    sub_dat <- filter(dat, suc_seq == j)
    for (k in colnames(vdem_seq)[4:27]){
      nums <- na.omit(sub_dat[,c(k, "order")] %>% distinct_(k, .keep_all = T))
      if (nrow(nums) > 1) {
        var_min <-  min(nums[,1], na.rm = T)
        var_max <- max(nums[,1], na.rm = T)
        seq_dat2[seq_dat2$country_name == i & seq_dat2$suc_seq == j,][,paste(k,"min", sep = "_")] <- var_min
        seq_dat2[seq_dat2$country_name == i & seq_dat2$suc_seq == j,][,paste(k,"max", sep = "_")] <- var_max
        seq_dat2[seq_dat2$country_name == i & seq_dat2$suc_seq == j,][,paste(k,"time_to_max", sep = "_")] <- nums$order[nums[,1] == var_max] - nums$order[nums[,1] == var_min] 
        seq_dat2[seq_dat2$country_name == i & seq_dat2$suc_seq == j,][,paste(k,"first_move", sep = "_")] <- na.omit(nums$order)[2] 
      }
      else if (nrow(nums) == 1){
        seq_dat2[seq_dat2$country_name == i & seq_dat2$suc_seq == j,][,paste(k,"min", sep = "_")] <- min(nums[,1], na.rm = T)
        seq_dat2[seq_dat2$country_name == i & seq_dat2$suc_seq == j,][,paste(k,"max", sep = "_")] <- max(nums[,1], na.rm = T)
        seq_dat2[seq_dat2$country_name == i & seq_dat2$suc_seq == j,][,paste(k,"time_to_max", sep = "_")] <- nums$order[nums[,1] == var_max] 
      }
    }
  }
}

seq_dat_prop2 <- seq_dat2
seq_dat_prop2 <- seq_dat_prop2 %>%
  mutate_at(vars(ends_with("time_to_max")),  funs(. /duration)) %>%
  mutate_at(vars(ends_with("first_move")), funs(. /duration))            
#seq_dat_prop2[is.na(seq_dat_prop2)] <- 0  

##doing some descriptive stuff

#get data in order to do boxplots...
min_seq_dat <- seq_dat2 %>%
  dplyr::select(country_name, byear, suc_seq, duration, init_polyarchy, ends_with("min")) %>%
  gather(feature, min_value, -country_name, -byear, -suc_seq, -duration, -init_polyarchy)
boxplot(min_seq_dat$min_value ~ min_seq_dat$feature)
p <- ggplot(min_seq_dat, aes(factor(feature), min_value))
p + geom_violin()

time_to_max_dat <- seq_dat_prop2 %>%
  dplyr::select(country_name, byear, suc_seq, duration, init_polyarchy, ends_with("time_to_max")) %>%
  gather(feature, time_to_max, -country_name, -byear, -suc_seq, -duration, -init_polyarchy) %>%
  filter(time_to_max >= 0)
boxplot(time_to_max_dat$time_to_max ~ time_to_max_dat$feature)
p <- ggplot(time_to_max_dat, aes(factor(feature), time_to_max))
p + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

first_move_dat <- seq_dat_prop2 %>%
  dplyr::select(country_name, byear, suc_seq, duration, init_polyarchy, ends_with("first_move")) %>%
  gather(feature, first_move, -country_name, -byear, -suc_seq, -duration, -init_polyarchy) %>%
  filter(first_move >= 0)
boxplot(first_move_dat$first_move ~ first_move_dat$feature)
p <- ggplot(first_move_dat, aes(factor(feature), first_move))
p + geom_boxplot(aes(fill = factor(suc_seq))) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#kmeans
fit4 <- kmeans(seq_dat_prop2[,6:101], 5)
fit4$tot.withinss
fit4$withinss
fit4$betweenss 
table(fit4$cluster)
seq_dat2$kmeans <- fit4$cluster
max_vars <- seq_dat2 %>%
  select(ends_with("ord_max"), "kmeans", "country_name")

#ward hierarchical clustering
d <- dist(seq_dat_prop2[,c(6:101)], method= "minkowski")
fit5 <- hclust(d, method="complete") 
plot(fit5, labels = paste(seq_dat_prop2$country_name, seq_dat_prop2$suc_seq, sep = "_"))
groups <- cutree(fit5, k=5) # cut tree into 5 clusters
# draw dendrogram with red borders around the 5 clusters 
rect.hclust(fit2, k=5, border="red")

##looking at the distance matrix
d <- dist(seq_dat_prop2[,c(6:101)], method= "euclidean")
d <- as.matrix(d)
nearest <- apply(as.data.frame(d), 1, order)

dim <- ncol(d)
image(1:dim, 1:dim, d)
text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", d), cex=0.6)

dist_mi <- 1/d # one over, as qgraph takes similarity matrices as input
library(qgraph)
qgraph(dist_mi, layout='spring', vsize=3, minimum = 0.02)

