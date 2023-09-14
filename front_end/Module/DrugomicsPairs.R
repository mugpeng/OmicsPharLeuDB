# DOP is the abbreviation of Drug_Omics_Pairs

# Omics ----
DOPselectOmicsServer <- function(id, data){
  moduleServer(id, function(input, output, session){
    omics_search_sel <- reactiveValues()
    observeEvent(data(), {
      omics_search_sel$omics <- switch(data(), 
                                       "mRNA" = omics_search[omics_search$type %in% "mRNA",]$omics,
                                       "meth" = omics_search[omics_search$type %in% "meth",]$omics,
                                       "protein" = omics_search[omics_search$type %in% "protein",]$omics,
                                       "cnv" = omics_search[omics_search$type %in% "cnv",]$omics,
                                       "mutation_genes" = omics_search[omics_search$type %in% "genes_mutation",]$omics,
                                       "mutations" = omics_search[omics_search$type %in% "mutations",]$omics,
                                       "gene_fusion" = omics_search[omics_search$type %in% "fusion",]$omics)
      updateSelectizeInput(session = session, inputId = 'select_specific_omic',
                           label = 'Omics Selection', choices = omics_search_sel$omics, server = TRUE,
                           options = list(placeholder = 'Please select an omic', onInitialize = I('function() { this.setValue(""); }')),
                           selected = "A1BG"
      )
    })
    omics_search_df1 <- reactive({switch(data(), 
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
                                                      "gCSI" = gCSI_cnv
                                                      ))})
    omics_search_df2 <- reactive({switch(data(), 
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
                                                              "FORALL" = FORALL_fusion
                                                              ))})
    omics_select_list1 <- reactive({
      lapply(omics_search_df1(), function(omics){
        index2 <- colnames(omics) %in% input$select_specific_omic 
        sel_omics <- omics[,index2]
        return(sel_omics)
      })
    })
    omics_select_list2 <- reactive({
      lapply(omics_search_df2(), function(omics){
        index3 <- omics[,1] %in% input$select_specific_omic
        yes_cell <- unique(omics[index3,2])
        return(yes_cell)
      })
    })
    return(list(omics_select_list1,
                omics_select_list2))
  })
}
DOPselectOmicsUI <- function(id){
  selectizeInput(
    NS(id, "select_specific_omic"), "Drug Selection", choices = NULL,
    options = list(
      placeholder = 'Please select a drug',
      onInitialize = I('function() { this.setValue(""); }'), selected = "LAPATINIB"
    ))
}

# DOPPlotOmicsServer <- function(id, data){
#   moduleServer(id, function(input, output, session){
#     
#   })
# }

# Drugs ----
DOPselectDrugsServer <- function(id, data){
  moduleServer(id, function(input, output, session){
    # Different omics have different choices in UI
    drugs_search_sel <- reactiveValues()
    observeEvent(data(), {
      drugs_search_sel$drugs <- switch(data(), 
                                       "mRNA" = drugs_search[drugs_search$type %in% "mRNA",]$drugs,
                                       "meth" = drugs_search[drugs_search$type %in% "meth",]$drugs,
                                       "protein" = drugs_search[drugs_search$type %in% "protein",]$drugs,
                                       "cnv" = drugs_search[drugs_search$type %in% "cnv",]$drugs,
                                       "mutation_genes" = drugs_search[drugs_search$type %in% "genes_mutation",]$drugs,
                                       "mutations" = drugs_search[drugs_search$type %in% "mutations",]$drugs,
                                       "gene_fusion" = drugs_search[drugs_search$type %in% "fusion",]$drugs)
      updateSelectizeInput(session = session, inputId = 'select_specific_drug',
                           label = 'Drugs Selection', choices = drugs_search_sel$drugs, server = TRUE,
                           options = list(placeholder = 'Please select a drug', onInitialize = I('function() { this.setValue(""); }')),
                           selected = "LAPATINIB"
      )
    })
    # Select targeted drug in each dataset
    drug_search_df <- reactive({switch(data(), 
                                       "mRNA" = list(ctrp1_expdrugs, 
                                                     ctrp2_expdrugs,
                                                     gdsc1_expdrugs, gdsc2_expdrugs,
                                                     tavor_expdrugs, beatAML_expdrugs),
                                       "meth" = list(ctrp1_methdrugs,
                                                     ctrp2_methdrugs),
                                       "protein" = list(ctrp1_proteindrugs,
                                                        ctrp2_proteindrugs,
                                                        FORALL_proteindrugs),
                                       "cnv" = list(ctrp1_cnvdrugs, ctrp2_cnvdrugs,
                                                    gdsc1_cnvdrugs, gdsc2_cnvdrugs,
                                                    gCSI_cnvdrugs),
                                       "mutation_genes" = list(ctrp1_mutdrugs, ctrp2_mutdrugs,
                                                               gdsc1_mutdrugs, gdsc2_mutdrugs,
                                                               beatAML_mutdrugs, gCSI_mutdrugs,
                                                               FORALL_mutdrugs),
                                       "mutations" = list(ctrp1_mutdrugs, ctrp2_mutdrugs,
                                                          gdsc1_mutdrugs, gdsc2_mutdrugs,
                                                          FORALL_mutdrugs),
                                       "gene_fusion" = list(ctrp1_fusiondrugs,
                                                            ctrp2_fusiondrugs,
                                                            FORALL_fusiondrugs))})
    drugs_select_list <- reactive({
      lapply(drug_search_df(), function(drugs){
        index1 <- colnames(drugs) %in% input$select_specific_drug
        sel_drugs <- drugs[,index1]
        return(sel_drugs)
      })
    })
    return(list(
      drugs_select_list, drug_search_df
    ))
  })
}
DOPselectDrugsUI <- function(id){
  selectizeInput(
    NS(id, "select_specific_drug"), "Drug Selection", choices = NULL,
    options = list(
      placeholder = 'Please select a drug',
      onInitialize = I('function() { this.setValue(""); }'), selected = "LAPATINIB"
    ))
}

# Plot ----
DOPCorPlotServer <- function(id, drugs_list, omics_list){
  moduleServer(id, function(input, output, session){
    cor_search_list <- reactive({
      cor_search_list <- lapply(1:length(drugs_list()), function(index){
        omics_sel <- omics_list()[[index]]
        drugs_sel <- drugs_list()[[index]]
        if(class(drugs_sel) == class(omics_sel) & class(drugs_sel) != "data.frame"){
          ex_drug_df <- data.frame(
            genes = as.numeric(omics_sel),
            drugs = as.numeric(drugs_sel),
            database = names(omics_list())[index]
          )
        } else {}
      })
      cor_search_list <- cor_search_list[!sapply(cor_search_list, is.null)]
      }
    )
    p_cor_serach <- reactive({
      p_list <- lapply(cor_search_list(), function(cor){
        ggscatter(cor, x = "genes", y = "drugs") + 
          stat_cor() + stat_smooth(method = "lm") + theme_bw() + 
          ggtitle(cor$database[1])
      })
      # Warning 
      validate(
        need(length(p_list) > 0, "There is no result for this drug-omic pair, or you have not chosen yet.")
      )
      wrap_plots(p_list)
      }
    )
    return(p_cor_serach)
  })
}
DOPBoxPlotServer <- function(id, drugs_list, omics_list, drugs_df){
  moduleServer(id, function(input, output, session){
    box_search_list <- reactive({
      box_search_list <- lapply(1:length(drugs_list()), function(index){
        cells_sel <- omics_list()[[index]]
        drugs_sel <- drugs_list()[[index]]
        drugs_df <- drugs_df()[[index]]
        if(length(cells_sel) != 0 & class(drugs_sel) != "data.frame"){
          yes <- na.omit(drugs_sel[rownames(drugs_df) %in% cells_sel])
          no <- na.omit(drugs_sel[!rownames(drugs_df) %in% cells_sel])
          data.frame(
            drugs = c(no, yes),
            events = rep(c("no","yes"), times = c(length(no), length(yes))),
            database = names(omics_list())[index]
          )
        } else {}
      })
      box_search_list <- box_search_list[!sapply(box_search_list, is.null)]
    }
    )
    p_box_serach <- reactive({
      p_list <- lapply(box_search_list(), function(box){
        ggboxplot(data = box, x = "events", y = "drugs",
                  fill = "events", palette = c("#BEBADAFF", "#FB8072FF"),
                  add = "jitter") + 
          stat_compare_means() + theme_bw() + ggtitle(box$database[1])
      })
      # Warning
      validate(
        need(length(p_list) > 0, "There is no result for this drug-omic pair, or you have not choosen yet.")
      )
      p <- wrap_plots(p_list) + plot_layout(guides = 'collect') & 
        theme(legend.position='top')
      p
    })
    return(p_box_serach)
  })
}