# FDS is the abbreviation of FeaturesSig

# UI ----
FDSfeaSelUI <- function(id){
  selectizeInput(
    NS(id, "select_specific_feature"), "Feature Selection", choices = NULL,
    options = list(
      placeholder = 'Please select a feature',
      onInitialize = I('function() { this.setValue(""); }')
    ))
}

# Server ----
FDSfeaSelServer <- function(id, data){
  moduleServer(id, function(input, output, session){
    feas_search_sel <- reactiveValues()
    observeEvent(data(), {
      feas_search_sel$feas <- switch(data(), 
                                       "mRNA" = omics_search[omics_search$type %in% "mRNA",]$omics,
                                       "meth" = omics_search[omics_search$type %in% "meth",]$omics,
                                       "protein" = omics_search[omics_search$type %in% "protein",]$omics,
                                       "cnv" = omics_search[omics_search$type %in% "cnv",]$omics,
                                       "mutation_genes" = omics_search[omics_search$type %in% "genes_mutation",]$omics,
                                       "mutations" = omics_search[omics_search$type %in% "mutations",]$omics,
                                       "gene_fusion" = omics_search[omics_search$type %in% "fusion",]$omics,
                                      "drug" = drugs_search$drugs
                                      )
      updateSelectizeInput(session = session, inputId = 'select_specific_feature',
                           label = 'Feature Selection', choices = feas_search_sel$feas, server = TRUE,
                           options = list(placeholder = 'Please select a feature', onInitialize = I('function() { this.setValue(""); }')),
      )
    })
    database_fea <- reactive(switch(data(), 
                                "mRNA" = list("ctrp1" = ctrp1_exp, 
                                              "ctrp2" = ctrp2_exp,
                                              "gdsc1" = gdsc1_exp, 
                                              "gdsc2" = gdsc2_exp,
                                              "tavor" = tavor_exp,
                                              "beatAML" = beatAML_exp),
                                "meth" = list("ctrp1" = ctrp1_meth,
                                              "ctrp2" = ctrp2_meth),
                                "protein" = list("ctrp1" = ctrp1_protein,
                                                 "ctrp2" = ctrp2_protein,
                                                 "FORALL" = FORALL_protein),
                                "cnv" = list("ctrp1" = ctrp1_cnv, 
                                             "ctrp2" = ctrp2_cnv,
                                             "gdsc1" = gdsc1_cnv, 
                                             "gdsc2" = gdsc2_cnv,
                                             "gCSI" = gCSI_cnv),
                                "mutation_genes" = list(
                                  "ctrp1" = ctrp1_mut[,1:2], 
                                  "ctrp2" = ctrp2_mut[,1:2],
                                  "gdsc1" = gdsc1_mut[,1:2], 
                                  "gdsc2" = gdsc2_mut[,1:2],
                                  "beatAML" = beatAML_mut[,1:2],
                                  "gCSI" = gCSI_mut[,1:2],
                                  "FORALL" = FORALL_mut[,1:2]),
                                "mutations" = list(
                                  "ctrp1" = ctrp1_mut[,c(4,2)], 
                                  "ctrp2" = ctrp2_mut[,c(4,2)],
                                  "gdsc1" = gdsc1_mut[,c(4,2)], 
                                  "gdsc2" = gdsc2_mut[,c(4,2)],
                                  "FORALL" = FORALL_mut[,c(4,2)]),
                                "gene_fusion" = list("ctrp1" = ctrp1_fusion,
                                                     "ctrp2" = ctrp2_fusion,
                                                     "FORALL" = FORALL_fusion),
                                "drug" = list(
                                  "ctrp1" = ctrp1_drug,
                                  "ctrp2" = ctrp2_drug,
                                  "gdsc1" = gdsc1_drug,
                                  "gdsc2" = gdsc2_drug,
                                  "FORALL" = FORALL_drug,
                                  "gCSI" = gCSI_drug,
                                  "beatAML" = beatAML_drug,
                                  "tavor" = tavor_drug
                                )))
    fea_sel_list <- reactive({
      if(data() %in% c("cnv",
                       "protein",
                       "meth",
                       "mRNA",
                       "drug"
      )){
        lapply(database_fea(), function(omics){
          index2 <- colnames(omics) %in% input$select_specific_feature 
          sel_omics <- omics[,index2]
          if(length(sel_omics) != 0){
            names(sel_omics) <- rownames(omics)
          }
          return(sel_omics)
        })
        } else {
        lapply(database_fea(), function(omics){
          index3 <- omics[,1] %in% input$select_specific_feature
          yes_cell <- unique(omics[index3,2])
          return(yes_cell)
        })
      }
    })
    return(fea_sel_list)
  })
}

FDSdbSelServer <- function(id, data){
  moduleServer(id, function(input, output, session){
    database_list <- reactive(switch(data(),
                                "mRNA" = list("ctrp1" = ctrp1_exp,
                                              "ctrp2" = ctrp2_exp,
                                              "gdsc1" = gdsc1_exp,
                                              "gdsc2" = gdsc2_exp,
                                              "tavor" = tavor_exp,
                                              "beatAML" = beatAML_exp),
                                "meth" = list("ctrp1" = ctrp1_meth,
                                              "ctrp2" = ctrp2_meth),
                                "protein" = list("ctrp1" = ctrp1_protein,
                                                 "ctrp2" = ctrp2_protein,
                                                 "FORALL" = FORALL_protein),
                                "cnv" = list("ctrp1" = ctrp1_cnv,
                                             "ctrp2" = ctrp2_cnv,
                                             "gdsc1" = gdsc1_cnv,
                                             "gdsc2" = gdsc2_cnv,
                                             "gCSI" = gCSI_cnv),
                                "mutation_genes" = list(
                                  "ctrp1" = ctrp1_mut[,1:2],
                                  "ctrp2" = ctrp2_mut[,1:2],
                                  "gdsc1" = gdsc1_mut[,1:2],
                                  "gdsc2" = gdsc2_mut[,1:2],
                                  "beatAML" = beatAML_mut[,1:2],
                                  "gCSI" = gCSI_mut[,1:2],
                                  "FORALL" = FORALL_mut[,1:2]),
                                "mutations" = list(
                                  "ctrp1" = ctrp1_mut[,c(4,2)],
                                  "ctrp2" = ctrp2_mut[,c(4,2)],
                                  "gdsc1" = gdsc1_mut[,c(4,2)],
                                  "gdsc2" = gdsc2_mut[,c(4,2)],
                                  "FORALL" = FORALL_mut[,c(4,2)]),
                                "gene_fusion" = list("ctrp1" = ctrp1_fusion,
                                                     "ctrp2" = ctrp2_fusion,
                                                     "FORALL" = FORALL_fusion),
                                "drug" = list(
                                  "ctrp1" = ctrp1_drug,
                                  "ctrp2" = ctrp2_drug,
                                  "gdsc1" = gdsc1_drug,
                                  "gdsc2" = gdsc2_drug,
                                  "FORALL" = FORALL_drug,
                                  "gCSI" = gCSI_drug,
                                  "beatAML" = beatAML_drug,
                                  "tavor" = tavor_drug
                                )
                                ))
     return(database_list)
      })
  }

# FDSpreprocessServer <- function(id, fea, database){
#   moduleServer(id, function(input, output, session){
#     fea <- reactive({
# 
#       fea
#     }) 
#     database <- reactive({
#       intersected_db <- intersect(names(database()), names(fea()))
#       database <- database()[names(database()) %in% intersected_db]
#       fea <- fea()[names(fea()) %in% intersected_db]
#       database
#     })
#     return(list(fea,
#                 database))
#   })
# }

FDSCorServer2 <- function(id, fea, database){
  moduleServer(id, function(input, output, session){
    fea_list <- reactive({
      intersected_db <- intersect(names(database()), names(fea()))
      database2 <- database()[names(database()) %in% intersected_db]
      fea2 <- fea()[names(fea()) %in% intersected_db]
      fea_list <- list()
      withProgress(message = "Calculation", value = 0, {
        for(index in 1:length(fea2)){
          if(length(fea2[[index]]) != 0){
            sel_name <- names(fea2)[index]
            db <- database2[[sel_name]]
            fea <- fea2[[index]]
            intersected_cells <- intersect(names(fea), rownames(db))
            db <- db[match(intersected_cells, rownames(db)),]
            fea <- fea[match(intersected_cells, names(fea))]
            re <- lapply(1:ncol(db), function(x){
              re <- tryCatch(cor.test(fea, db[,x]),
                             error = function(x){NA})
              if(all(is.na(re))){
                re2 <- data.frame(
                  p = NA,
                  R = NA
                )
              } else {
                re2 <- data.frame(
                  p = re$p.value,
                  R = re$estimate)
                # re2$FDR <- p.adjust(re2$p, method = "fdr") 
              }
              return(re2)
            })
            re <- do.call(rbind, re)
            re$fea <- colnames(db)
            re <- na.omit(re)
            re$q <- p.adjust(re$p, method = "fdr")
            re$source <- sel_name
            colnames(re) <- c("-log10(p)", "R/Diffs/Odds", "fea", "q", "source")
            re$group <-  factor(ifelse((re$q > 0.1), "qVal > 0.1", "qVal <= 0.1"),
                                levels = c("qVal > 0.1", "qVal <= 0.1"))
            # cor$p <- round(cor$p, digits = 3)
            re$`-log10(p)` <- round(-log10(re$`-log10(p)`), digits = 3)
            fea_list[[index]] <- re
          } else {fea_list[[index]] <- NULL}
          incProgress(1/length(fea2), detail = paste("Doing part", index))
        }
      })
      fea_list <- fea_list[!sapply(fea_list, is.null)]
      fea_list
    })
    return(fea_list)
  })
}

FDSWilcoxServer_feaDis2 <- function(id, fea, database){
  moduleServer(id, function(input, output, session){
    fea_list <- reactive({
      intersected_db <- intersect(names(database()), names(fea()))
      database2 <- database()[names(database()) %in% intersected_db]
      fea2 <- fea()[names(fea()) %in% intersected_db]
      fea_list <- list()
      withProgress(message = "Calculation", value = 0, {
        for(index in 1:length(fea2)){
          if(length(fea2[[index]]) != 0){
            sel_name <- names(fea2)[index]
            db <- database2[[sel_name]]
            fea <- fea2[[index]]
            re <- lapply(1:ncol(db), function(x){
              yes <- na.omit(db[rownames(db) %in% fea,x])
              no <- na.omit(db[!rownames(db) %in% fea,x])
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
                  fc = median(yes) - median(no)
                )
              }
              return(re)
            })
            re <- do.call(rbind, re)
            re$fea <- colnames(db)
            re <- na.omit(re)
            re$q <- p.adjust(re$p, method = "fdr")
            re$source <- sel_name
            colnames(re) <- c("-log10(p)", "R/Diffs/Odds", "fea", "q", "source")
            re$group <-  factor(ifelse((re$q > 0.1), "qVal > 0.1", "qVal <= 0.1"),
                                levels = c("qVal > 0.1", "qVal <= 0.1"))
            re$`-log10(p)` <- round(-log10(re$`-log10(p)`), digits = 3)
            re$`R/Diffs/Odds` <- round(re$`R/Diffs/Odds`, digits = 3)
            fea_list[[index]] <- re
          } else {fea_list[[index]] <- NULL}
          incProgress(1/length(fea2), detail = paste("Doing part", index))
        }
      })
      fea_list <- fea_list[!sapply(fea_list, is.null)]
      fea_list
    })
    return(fea_list)
  })
}

FDSWilcoxServer_feaCon2 <- function(id, fea, database){
  moduleServer(id, function(input, output, session){
    fea_list <- reactive({
      intersected_db <- intersect(names(database()), names(fea()))
      database2 <- database()[names(database()) %in% intersected_db]
      fea2 <- fea()[names(fea()) %in% intersected_db]
      fea_list <- list()
      withProgress(message = "Calculation", value = 0, {
        for(index in 1:length(fea2)){
          if(length(fea2[[index]]) != 0){
            sel_name <- names(fea2)[index]
            db <- unique(database2[[sel_name]])
            db_fea <- unique(db[,1])
            fea <- fea2[[index]]
            re <- lapply(db_fea, function(x){
              cells <- db[db[,1] %in% x,2]
              yes <- na.omit(fea[names(fea) %in% cells])
              no <- na.omit(fea[!names(fea) %in% cells])
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
                  fc = median(yes) - median(no)
                )
              }
              return(re)
            })
            re <- do.call(rbind, re)
            re$fea <- db_fea
            re <- na.omit(re)
            re$q <- p.adjust(re$p, method = "fdr")
            re$source <- sel_name
            colnames(re) <- c("-log10(p)", "R/Diffs/Odds", "fea", "q", "source")
            re$group <-  factor(ifelse((re$q > 0.1), "qVal > 0.1", "qVal <= 0.1"),
                                levels = c("qVal > 0.1", "qVal <= 0.1"))
            re$`-log10(p)` <- round(-log10(re$`-log10(p)`), digits = 3)
            re$`R/Diffs/Odds` <- round(re$`R/Diffs/Odds`, digits = 3)
            fea_list[[index]] <- re
          } else {fea_list[[index]] <- NULL}
          incProgress(1/length(fea2), detail = paste("Doing part", index))
        }
      })
      fea_list <- fea_list[!sapply(fea_list, is.null)]
      fea_list
    })
    return(fea_list)
  })
}

FDSFisherServer2 <- function(id, fea, database, cell_names){
  moduleServer(id, function(input, output, session){
    fea_list <- reactive({
      intersected_db <- intersect(names(database()), names(fea()))
      database2 <- database()[names(database()) %in% intersected_db]
      fea2 <- fea()[names(fea()) %in% intersected_db]
      fea_list <- list()
      withProgress(message = "Calculation", value = 0, {
        for(index in 1:length(fea2)){
          if(length(fea2[[index]]) != 0){
            sel_name <- names(fea2)[index]
            db <- unique(database2[[sel_name]])
            cell_name <- cell_names()[[sel_name]]
            fea <- fea2[[index]]
            no_fea <- cell_name[!cell_name %in% fea]
            db_fea <- unique(db[,1])
            re <- lapply(db_fea, function(x){
              db2 <- db[db[,1] %in% x,]
              intersect_yes_yes <- length(intersect(fea, db2[,2]))
              intersect_yes_no <- length(fea) - intersect_yes_yes
              intersect_no_yes <- length(intersect(no_fea, db2[,2]))
              intersect_no_no <- length(no_fea) - intersect_no_yes
              df <- t(data.frame(
                yes = c(intersect_yes_yes, intersect_yes_no),
                no = c(intersect_no_yes, intersect_no_no)
              ))
              colnames(df) <- c("yes","no")
              re <- fisher.test(df)
              re <- data.frame(
                p = re$p.value,
                odd = re$estimate
              )
              re
            })
            re <- do.call(rbind, re)
            re$fea <- db_fea
            re <- na.omit(re)
            re$odd[re$odd %in% Inf] <- 1
            re$odd[re$odd %in% 0] <- 1
            re$q <- p.adjust(re$p, method = "fdr")
            re$source <- sel_name
            colnames(re) <- c("-log10(p)", "R/Diffs/Odds", "fea", "q", "source")
            re$group <-  factor(ifelse((re$q > 0.1), "qVal > 0.1", "qVal <= 0.1"),
                                levels = c("qVal > 0.1", "qVal <= 0.1"))
            re$`-log10(p)` <- round(-log10(re$`-log10(p)`), digits = 3)
            re$`R/Diffs/Odds` <- round(re$`R/Diffs/Odds`, digits = 3)
            fea_list[[index]] <- re
          } else {fea_list[[index]] <- NULL}
          incProgress(1/length(fea2), detail = paste("Doing part", index))
        }
      })
      fea_list <- fea_list[!sapply(fea_list, is.null)]
      fea_list
    })
    return(fea_list)
  })
}

FDSvolcanoPlotServer <- function(id, fea_sig_list){
  moduleServer(id, function(input, output, session){
    sig_plot <- reactive({
      sig_plot <- lapply(fea_sig_list(), function(sig){
        p <- ggplot(data = sig,
                    aes(x = `R/Diffs/Odds`,
                        y = `-log10(p)`,
                        label = fea)) +
          geom_point(size=3.5, alpha = 0.6,
                     aes(color = group)) +
          theme_bw() + theme(
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12)) +
          ylab("-log10(Pvalue)") + scale_color_manual(values = c("grey", "#44bce4")) +
          facet_wrap(~source)
        return(p)
      })
      validate(
        need(length(sig_plot) > 0, "There is no result for this feature-database pair, or you have not choosen yet.")
      )
      sig_plotly <- if(length(sig_plot) >= 3){
        sig_plot %>% subplot(nrows = 2) %>%
          layout(showlegend = FALSE)
        # layout(annotations = list(
        #   list(x = -0.1, y = 0.5, text = "-log10(Pvalue)",
        #        font = list(color = "black", size = 18,
        #                    face = "bold"),
        #        textangle = 270,
        #        showarrow = F, xref='paper', yref='paper', size=48)
        # )) %>%
        # layout(annotations = list(
        #   list(x = 0.5, y = -0.2, text = "R/Diffs/Odds",
        #        font = list(color = "black", size = 18,
        #                    face = "bold"),
        #        textangle = 0,
        #        showarrow = F, xref='paper', yref='paper', size=48)
        # ))
      } else {
        sig_plot %>% subplot(nrows = 1) %>% layout(showlegend = FALSE)
      }
    })
    return(sig_plot)
  })
}

