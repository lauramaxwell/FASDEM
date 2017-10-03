### Options
OUTPUT_DIR <- "~/Documents/Fall 2017/FASDEM/"
CUTOFF <- 0.05
###

setwd("~/Dropbox/Sequencing methods")

source("functions.R")
library(xlsx)
library(dplyr)

### Data files
trans.df <- read.csv("Translation_table-2016-01-13.csv", sep=";", stringsAsFactors=F)
data.all <- read.csv2("V-Dem-DS-CY+Others-v6.csv", stringsAsFactors=F)

#Example ordinals
ex_ords <- c("v2meharjrn_ord","v2cseeorgs_ord","v2elfrfair_ord","v2clacjstw_ord",
             "v2juhcind_ord","v2lginvstp_ord","v2pehealth_ord","v2exbribe_ord")


#indicators <- c(dem_inds, dem_comps, dem_ords)
indicators <- ex_ords

data.all <- data.all[!(grepl("region", data.all$country_name)), ]
data.all[,names(data.all)[grep("3[cC]",names(data.all))]] <- data.all[,names(data.all)[grep("3[cC]",names(data.all))]] * 2
data.all[,names(data.all)[grep("4[cC]",names(data.all))]] <- round(data.all[,names(data.all)[grep("4[cC]",names(data.all))]] * 3)
data.all[,names(data.all)[grep("5[cC]",names(data.all))]] <- round(data.all[,names(data.all)[grep("5[cC]",names(data.all))]] * 4)

npols <- apply(data.all[,c(names(data.all)[grep("3[cC]",names(data.all))],
                           names(data.all)[grep("4[cC]",names(data.all))],
                           names(data.all)[grep("5[cC]",names(data.all))],
                           names(data.all)[grep("_ord$",names(data.all))])],
               2, max, na.rm=T) + 1

# write.xlsx will truncate our longer names so shorten & remove illegal chars
ql_trans <- function(s, trans.df) {
  subN <- trans.indicator(s, trans.df) %>%
    gsub('/', ' or ', .) %>%
    sub('Freedom (of|from) ', '', .) %>%
    sub('Government censorship effort - ', 'Gov censorship ', .)
  paste0(toupper(substring(subN, 1, 1)), substring(subN, 2))
}

ql <- quantile.by.value.all(data.all, indicators, CUTOFF) %>%
  setNames(sapply(names(.), ql_trans, trans.df))

cols <- c("Total Sum", "Rightmost Sum", "States", "Total Sum Standardized",
          " Rightmost Sum Standardized")
sums <- matrix(data=NA, ncol=5, nrow=length(ql), dimnames=list(names(ql), cols))

highest <- list()
expanded <- list()

highest_adj <- list()
expanded_adj <- list()

for (i in 1:length(ql)) {
  var <- names(ql[i])
  
  colnames(ql[[i]]) <- sapply(colnames(ql[[i]]), ql_trans, trans.df)
  ql[[i]] <- t(ql[[i]])
  
  orig_ma <- ql[[i]]
  for (j in 1:nrow(ql[[i]])) {
    s <- npols[names(rownames(ql[[i]]))[j] == names(npols)]
    if (s != 5)
      ql[[i]][j,] <- round((4 / (s - 1)) * ql[[i]][j,])
  }
  
  nr_cols <- ncol(ql[[i]])
  sum_total <- sum(ql[[i]], na.rm=T)
  sum_last_col <- sum(ql[[i]][,nr_cols], na.rm=T)
  sum_total_st <- sum(orig_ma, na.rm=T) * (5 / nr_cols)
  sum_last_col_st <-  sum(orig_ma[,nr_cols], na.rm=T) * (5 / nr_cols)
  
  sums[i,] <- c(sum_total, sum_last_col, nr_cols, sum_total_st, sum_last_col_st)
  
  highest[[var]] <- as.list(ql[[i]][,nr_cols])
  highest[[var]][var] <- NA
  expanded[[var]] <- split(ql[[i]], rep(1:ncol(ql[[i]]), each=nrow(ql[[i]]))) %>%
    lapply(function(l) setNames(l, rownames(ql[[i]])))
  
  orig_ma <- orig_ma * (5 / nr_cols)
  highest_adj[[var]] <- as.list(orig_ma[,ncol(orig_ma)])
  highest_adj[[var]][var] <- NA
  expanded_adj[[var]] <- split(orig_ma, rep(1:ncol(orig_ma),
                                            each=nrow(orig_ma))) %>%
    lapply(function(l) setNames(l, rownames(orig_ma)))
}

total <- function(df) {
  df <- cbind(df, Sums = apply(df, 1, sum, na.rm=T)) %>%
    rbind(., Sums = apply(., 2, sum, na.rm=T))
  df[nrow(df),ncol(df)] <- NA
  df[order(df$Sums, decreasing=T), order(df[nrow(df),])]
}

highest.df <- bind_rows(highest) %>% as.data.frame
rownames(highest.df) <- apply(highest.df, 1, function(x)
  colnames(highest.df)[which(is.na(x))])
highest.df <- total(highest.df)

expanded.df <- lapply(seq_along(expanded), function(i) {
  df <- do.call(rbind, expanded[[i]])
  rownames(df) <- paste(names(expanded[i]), 0:(nrow(df)-1))
  as.data.frame(df)
})

highest_adj.df <- bind_rows(highest_adj) %>% as.data.frame
rownames(highest_adj.df) <- apply(highest_adj.df, 1, function(x)
  colnames(highest_adj.df)[which(is.na(x))])
highest_adj.df <- total(highest_adj.df)

expanded_adj.df <- lapply(seq_along(expanded_adj), function(i) {
  df <- do.call(rbind, expanded_adj[[i]])
  rownames(df) <- paste(names(expanded_adj[i]), 0:(nrow(df)-1))
  as.data.frame(df)
})

rows <- lapply(expanded.df, rownames) %>% unlist
expanded.df <- bind_rows(expanded.df)
rownames(expanded.df) <- rows
expanded.df <- total(expanded.df)

rows <- lapply(expanded_adj.df, rownames) %>% unlist
expanded_adj.df <- bind_rows(expanded_adj.df)
rownames(expanded_adj.df) <- rows
expanded_adj.df <- total(expanded_adj.df)

sums <- sums[order(sums[,1], decreasing = T),]


# Write the new list w/ sep worksheets for each element (matrix)
write.xlsx.list(c(list('Summary' = sums, 'Matrix' = highest.df,
                       'Full Matrix' = expanded.df, 'Adjusted Matrix' = highest_adj.df,
                       'Adjusted Full Matrix' = expanded_adj.df), ql),
                paste0(OUTPUT_DIR, "Contingencies.xlsx"), showNA=F)