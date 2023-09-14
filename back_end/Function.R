# Re scale ----
## AUC, IC50
rescaleMode1 <- function(drugs_df){
  drugs_df2 <- drugs_df[,-1]
  drugs_df2 <- apply(drugs_df2, 2, function(x){
    1/x
  })
  drugs_df3 <- cbind.data.frame(cell = drugs_df[,1], drugs_df2)
  return(drugs_df3)
}

# Basic stat ----
calSDandMedian <- function(df){
  re <- apply(df[,-1], 2, function(x){
    sd1 <- sd(x, na.rm = T)
    median1 <- median(x, na.rm = T)
    c(sd1, median1)
  })
  re <- as.data.frame(t(re))
  colnames(re) <- c("SD", "Median")
  return(re)
}

# Drugs subtypes ----
kruskalDrugsTest <- function(drugs_df){
  drugs_df2 <- drugs_df[,-c(1:2)]
  re <- lapply(1:ncol(drugs_df2), function(x){
    drugs <- drugs_df2[,x]
    KW.p.sig <- kruskal.test(drugs ~ CellType, data = drugs_df)$p.value
    data.frame(
      drugs = colnames(drugs_df2)[x],
      pvalue = KW.p.sig
    )
  })
  re <- do.call(rbind, re)
  re$Sig <- ifelse(re$pvalue < .05, "Sig", "No")
  return(re)
}

wilcoxDrugsTest <- function(drugs_df){
  subtypes <- names(table(drugs_df$CellType)[table(drugs_df$CellType) >= 3])
  drugs_df2 <- drugs_df[,-c(1:2)]
  re <- lapply(subtypes, function(subtype){
    index <- which(drugs_df$CellType %in% subtype)
    re <- lapply(1:ncol(drugs_df2), function(x){
      all_drugs <- drugs_df2[,x]
      index_drugs <- all_drugs[index]
      conditions <- all(is.na(as.numeric(all_drugs))) | all(is.na(as.numeric(index_drugs)))
      if(!conditions){
        re <- c(wilcox.test(all_drugs, index_drugs)$p.value, 
                mean(index_drugs, na.rm = T)/mean(all_drugs, na.rm = T))
      } else{
        re <- c(NA,NA)
      }
    })
    re <- as.data.frame(do.call(rbind, re))
    colnames(re) <- c("pvalue", "FoldChange")
    re$Subtype <- subtype
    re$drugs <- colnames(drugs_df2)
    return(re)
  })
  re <- do.call(rbind, re)
  re$Sig <- ifelse(re$pvalue < .05, "Sig", "No")
  return(re)
}

rankDrugsTest <- function(drugs_df){
  # All AUC
  drugs_df2 <- drugs_df[,-c(1:2)]
  rank_all_df <- apply(drugs_df2, 2, function(x){
      median(x, na.rm = T)
  })
  rank_all_df <- data.frame(
    md_AUC = sort(rank_all_df),
    Drugs = names(sort(rank_all_df))
  )
  rank_all_df$Rank <- 1:nrow(rank_all_df)
  rank_all_df$Type <- "All"
  # Subtype AUC
  sel_types <- names(table(drugs_df$CellType)[table(drugs_df$CellType) >= 3])
  drugs_df3 <- drugs_df[drugs_df$CellType %in% sel_types,]
  drugs_list <- split(drugs_df3, f = drugs_df3$CellType)
  re <- lapply(drugs_list, function(df){
    # df = drugs_list[[1]]
    drugs_df4 <- df[,-c(1:2)]
    rank_all_df2 <- apply(drugs_df4, 2, function(x){
      median(x, na.rm = T)
    })
    rank_all_df2 <- data.frame(
      md_AUC = sort(rank_all_df2),
      Drugs = names(sort(rank_all_df2))
    )
    rank_all_df2$Rank <- 1:nrow(rank_all_df2)
    rank_all_df2$Type <- df$CellType[1]
    rank_all_df2
  })
  rank_sub_df <- do.call(rbind, re)
  rank_df <- rbind(rank_all_df, 
                   rank_sub_df)
  rownames(rank_df) <- NULL
  return(rank_df)
}

# Continuous omics and drugs ----
plotCor <- function(omics_df, drugs_df,
                        fea, drug){
  features <- omics_df[,fea]
  drugs <- drugs_df[,drug]
  df1 <- data.frame(
    features,
    drugs
  )
  colnames(df1) <- c(fea, drug)
  p <- ggscatter(df1, x = fea, y = drug) + 
    stat_cor()
  return(p)
}

preContinuous <- function(omics_df, drugs_df){
  inter <- intersect(drugs_df[,1], colnames(omics_df))
  omics_df2 <- omics_df[,colnames(omics_df) %in% inter]
  drugs_df2 <- drugs_df[match(colnames(omics_df2), drugs_df[,1]),]
  rownames(drugs_df2) <- drugs_df2[,1]
  drugs_df2[,1] <- NULL
  omics_df2 <- as.data.frame(t(omics_df2))
  return(
    list(omics_df2, drugs_df2)
  )
}

calSigpairs <- function(omics_df, drugs_df){
  sfExport('omics_df', 'drugs_df')
  re <- sfLapply(1:ncol(drugs_df), function(x){
    drugs <- drugs_df[,x]
    re <- lapply(1:ncol(omics_df), function(y){
      omics <- omics_df[,y]
      re <- tryCatch(cor.test(as.numeric(omics), drugs),
                     error = function(x){NA})
      if(all(is.na(re))){
        re2 <- data.frame(
          p = NA,
          R = NA
        )
      }else{
        re2 <- data.frame(
          p = re$p.value,
          R = re$estimate)
        # re2$FDR <- p.adjust(re2$p, method = "fdr") 
      }
      return(re2)
    })
    re <- do.call("rbind", re)
    re$omics <- colnames(omics_df)
    re$drugs <- colnames(drugs_df)[x]
    re <- na.omit(re)
    return(re)
  })
  re <- do.call(rbind, re)
  rownames(re) <- NULL
  return(re)
}

# extractSigpairs <- function(cor_list){
#   re <- lapply(1:length(cor_list), function(x){
#     re <- cor_list[[x]]
#     re <- na.omit(re)
#     re$drugs <- names(cor_list)[x]
#     re <- re[re$p < .05,]
#     return(re)
#   })
#   do.call(rbind, re)
# }

fisherOmicsTest <- function(omics_df, col_name){
  omics_df <- as.data.frame(omics_df)
  omics_df$omics <- omics_df[, col_name]
  lapply(unique(omics_df$omics), function(omics){
    tab1 <- merge(omics_df[omics_df$omics %in% omics,],
                  ccle_anno_leu[,c(1,2,6,7)], by = "DepMap_ID", all.y = T)
    tab1 <- as.data.frame(tab1)
    tab1[,omics] <- ifelse(is.na(tab1$omics), "no", "yes")
    all_tab1 <- table(tab1[,omics])
    re <- lapply(c("ALL", "AML", "CML"), function(types){
      # types = "ALL"
      tab2 <- tab1[tab1$lineage_subtype %in% types,]
      all_tab2 <- table(tab2[,omics])
      if(length(all_tab2) >= 2){
        test_re <- fisher.test(matrix(c(all_tab1["yes"], all_tab1["no"], all_tab2["yes"], all_tab2["no"]), nrow = 2))
        prop <- c(all_tab1["yes"]/nrow(tab1), all_tab2["yes"]/nrow(tab2))
        re <- c(round(as.numeric(prop), 3), test_re$p.value, types)
      } else{
        if(is.na(all_tab2["yes"])) {prop <- c(all_tab1["yes"]/nrow(tab1), 0)}
        else{prop <- c(all_tab1["yes"]/nrow(tab1), 1)}
        re <- c(round(as.numeric(prop), 3), 1, types)
      }
    })
    re <- do.call(rbind, re)
    re <- as.data.frame(re)
    colnames(re) <- c("All_prop", "subtype_prop", "fisher_Pvalue", "types")
    sapply(1:3, function(x){
      re[,x] <<- as.numeric(re[,x])
    })
    re$omics <- omics
    return(re)
  })
    
}

wilcoxOmicsTest <- function(omics_df, anno){
  omics_df <- as.data.frame(omics_df)
  index1 <- which(anno$lineage_subtype %in% "ALL")
  index2 <- which(anno$lineage_subtype %in% "AML")
  index3 <- which(anno$lineage_subtype %in% "CML")
  re <- lapply(1:nrow(omics_df), function(x){
    omics <- if(all(is.na(as.numeric(omics_df[x,])))){0} else{as.numeric(omics_df[x,])}
    ALL <- if(all(is.na(omics[index1]))){omics} else{omics[index1]}
    AML <- if(all(is.na(omics[index2]))){omics} else{omics[index2]}
    CML <- if(all(is.na(omics[index3]))){omics} else{omics[index3]}
    re <- data.frame(
      p.ALL = wilcox.test(ALL, omics)$p.value,
      fc.ALL = mean(ALL, na.rm = T)/mean(omics, na.rm = T),
      p.AML = wilcox.test(AML, omics)$p.value,
      fc.AML = mean(AML, na.rm = T)/mean(omics, na.rm = T),
      p.CML = wilcox.test(CML, omics)$p.value,
      fc.CML = mean(CML, na.rm = T)/mean(omics, na.rm = T)      
    )
    # print(x)
    return(re)
  })
  re <- do.call(rbind, re)
  rownames(re) <- rownames(omics_df)
  return(re)
}

# Discrete omics and drugs ----
preDiscrete <- function(omics_df, drugs_df){
  inter <- intersect(drugs_df[,1], unique(omics_df[,2]))
  omics_df2 <- omics_df[omics_df[,2] %in% inter,]
  drugs_df2 <- drugs_df[drugs_df[,1] %in% inter,]
  rownames(drugs_df2) <- drugs_df2[,1]
  drugs_df2[,1] <- NULL
  return(
    list(omics_df2, drugs_df2)
  )
}

wilcoxOmicsDrugsTest <- function(drugs_df, omics_df, col_name){
  omics_df <- as.data.frame(omics_df)
  omics_df$omics <- omics_df[, col_name]
  sfExport('omics_df', 'drugs_df')
  re <- sfLapply(unique(omics_df$omics), function(omics){
    # omics = "CDK11A"
    yes_cell <- omics_df[omics_df$omics %in% omics, 2]
    re <- lapply(1:ncol(drugs_df), function(x){
      yes <- na.omit(drugs_df[rownames(drugs_df) %in% yes_cell,x])
      no <- na.omit(drugs_df[!rownames(drugs_df) %in% yes_cell,x])
      if(length(yes) == 0 | length(no) == 0) {
        re <- data.frame(
          p = 1,
          fc = 0
        )
      }
      else{
        test_re <- wilcox.test(yes, no)
        re <- data.frame(
          p = test_re$p.value,
          fc = mean(yes)/mean(no)
        )
      }
      re$drugs <- colnames(drugs_df)[x]
      return(re)
    })
    re <- do.call(rbind, re)
    re$omics <- omics
    return(re)
  })
  re <- do.call(rbind, re)
  return(re)
}

plotMutsDrugs <- function(drugs_df, omics_df, drug_names, omics_names, 
                           mutgenes_mode = T){
  if(mutgenes_mode == T) omics = "genes_muts" else omics = "genes"
  yes_cell <- omics_df[omics_df[,omics] %in% omics_names, "cells"]
  yes <- na.omit(drugs_df[rownames(drugs_df) %in% yes_cell, drug_names])
  no <- na.omit(drugs_df[!rownames(drugs_df) %in% yes_cell, drug_names])
  mut_df <- data.frame(
    drugs = c(no, yes),
    events = rep(c("no","yes"), times = c(length(no), length(yes)))
  )
  ggboxplot(data = mut_df, x = "events", y = "drugs",
            fill = "events", palette = c("#BEBADAFF", "#FB8072FF"),
            add = "jitter") + 
    stat_compare_means() + theme_bw() + ggtitle(paste0("Drugs:", drug_names, "\n", "Omics:", omics_names))
}



