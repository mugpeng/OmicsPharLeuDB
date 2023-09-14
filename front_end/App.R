# Preparation ----
library(shiny)
library(rsconnect)
library(DT)
library(plotly)
library(ggrepel)
library(ggpubr)
library(patchwork)
library(paletteer)
library(periscope)

## Debug
library(reactlog)

# Load ----
source("Module/LoadData.R")

## Load module 
source("Module/DrugomicsPairs.R")
source("Module/FeaturesDatabasesSig.R")
source("Module/StatAnno.R")

# Preprocess ----
## UI function 
str1 <- "Nice to meet you."
str2 <- "Very welcome to my version 2.0. —22/05/16"
modal_notification <- modalDialog(
  # p("Nice to meet you. \n, test"),
  HTML(paste(str1, str2, sep = '<br/>')),
  title = "Update Notification",
  footer = tagList(
    actionButton("close_modal", "Close")
  )
)

# ui ----
ui <- navbarPage("OmicsPharLeuDB (mugpeng@foxmail.com)",
                 ## Drugs sensitivity display ----
                 navbarMenu("Drugs Sensitivity Display",
                            tabPanel("Comparison between subtypes",
                                     fluidPage(
                                       fluidRow(
                                         column(12,
                                                selectizeInput(
                                                  "select_drug", "Drug Selection", choices = NULL,
                                                  options = list(
                                                    placeholder = 'Please select a drug',
                                                    onInitialize = I('function() { this.setValue(""); }'), selected = "LAPATINIB"
                                                  ))),
                                         column(
                                           12,
                                           wellPanel(
                                             plotOutput("p_drug_sensitivity"),
                                             column(3,
                                                    downloadLink("downloadData_sens","Download Plot")
                                             )
                                           )
                                         )
                                       )
                                     )
                                     ),
                            tabPanel("TSNE/SD-MEDIAN",
                                     fluidPage(
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput(inputId = "sensitivity_select", 
                                                       h4("Please select a Drug Sensitivity Database:"), 
                                                       choices = c("GDSC1", "GDSC2", 
                                                                   "CTRP1", "CTRP2",
                                                                   "FORALL", "gCSI",
                                                                   "Tavor", "BeatAML")
                                           )
                                         ),
                                         mainPanel(
                                           column(6,
                                                  radioButtons(inputId = "sensitivity_radio", 
                                                               strong("Visualization Types"),
                                                               choices = list("TSNE" = "TSNE",
                                                                              "SD&MEDIAN" = "SD"), selected = "TSNE")),
                                           column(6,
                                                  textInput("sensitivity_text_search", "You can highlight targeted drug", 
                                                            value = "")),
                                           column(12,
                                                  plotly::plotlyOutput("p_drug_sum"),
                                                  # p("You can interact with the above ggplotly-based plot", align = "center"))
                                         )
                                       )
                                     ))
                 )),
                 ## Drugs-omics pairs analysis ----
                 tabPanel("Drugs-omics pairs Analysis",
                          fluidPage(
                            fluidRow(
                              column(4,
                                     selectInput(inputId = "select_omics", 
                                                 "Please select the omic type:", 
                                                 choices = c("mRNA", "meth",
                                                             "protein", "cnv",
                                                             "mutation_genes", "mutations",
                                                             "gene_fusion")
                                     )),
                              column(4,
                                     DOPselectOmicsUI("DOPselectOmics"),
                              ),
                              column(4,
                                     DOPselectDrugsUI("DOPselectDrugs"),
                              ),
                            ),
                            wellPanel(
                              # textOutput("total")
                              plotOutput("p_search"),
                              column(3,
                                     downloadLink("downloadData_DO","Download Plot")
                              )
                            )
                          )
                          ),
                 ## Features database significant analysis ----
                 tabPanel("Features Database Significant Analysis",
                          fluidPage(
                            fluidRow(
                              column(4,
                                     selectInput(inputId = "select_FDS_types", 
                                                 "Please select the omics or drugs:", 
                                                 choices = c("drug", 
                                                             "mRNA", "meth",
                                                             "protein", "cnv",
                                                             "mutation_genes", "mutations",
                                                             "gene_fusion")
                                     )),
                              column(4,
                                     FDSfeaSelUI("FDSfeaSel"),
                              ),
                              column(4,
                                     selectInput(inputId = "select_databases", 
                                                 "Please select the omics or drug database:", 
                                                 choices = c("drug", 
                                                             "mRNA", "meth",
                                                             "protein", "cnv",
                                                             "mutation_genes", "mutations",
                                                             "gene_fusion")
                                     )),
                              column(12,
                                     plotly::plotlyOutput("p_sig_plotly"),
                                     # textOutput("test"),
                                     downloadButton('downloadData', 'Download Significant Results'),
                                     wellPanel(
                                       p("The y-axis is ", strong("-log10(Pvalue)"), ", while the x-axis is", strong("R/Diffs/Odds"), align = "center"),
                                       p("R(0-1) for continuous feature and continuous dataset",
                                         align = "center"),
                                       p("Odds(>1 means the selected feaA has more prop of feaB events from db) for discrete feature and discrete dataset",
                                         align = "center"),
                                       p("Diff(median of Fea group substract no fea group) for the other situations(discrete feature and continuous dataset)",
                                         align = "center"),
                                       p("The features with q-value <= 0.1 are highlighted in ", span("blue", style = "color:blue"), align = "center")
                                     )
                                     )
                            )
                          )
                 ),
                 ## Statistics and Annotations ----
                 tabPanel("Statistics and Annotations",
                          fluidPage(
                            column(12,
                                   navlistPanel(
                                     tabPanel("Overall Drug Info",
                                              column(6,
                                                     plotOutput("p_count_drugandcell")),
                                              column(6,
                                                     plotOutput("p_count_subtype")),
                                              column(3,
                                                     downloadLink("downloadData_stat","Download Plot")
                                                     )
                                     ),
                                     tabPanel("Drugs Annotation",
                                              tabsetPanel(
                                                tabPanel("GDSC1",
                                                         dataTableOutput("gdsc1_drug_anno")),
                                                tabPanel("GDSC2",
                                                         dataTableOutput("gdsc2_drug_anno")),
                                                tabPanel("CTRP1",
                                                         dataTableOutput("ctrp1_drug_anno")),
                                                tabPanel("CTRP2",
                                                         dataTableOutput("ctrp2_drug_anno")),
                                                tabPanel("beatAML",
                                                         dataTableOutput("beatAML_drug_anno")),
                                                tabPanel("tavor",
                                                         dataTableOutput("tavor_drug_anno")),
                                                tabPanel("gCSI",
                                                         dataTableOutput("gCSI_drug_anno")),
                                                tabPanel("FORALL",
                                                         dataTableOutput("FORALL_drug_anno")),
                                              )),
                                     tabPanel("Cell Lines Annotation",
                                              tabsetPanel(
                                                tabPanel("GDSC",
                                                         dataTableOutput("gdsc_anno")),
                                                tabPanel("CCLE(CTRP)",
                                                         dataTableOutput("ccle_anno")),
                                                tabPanel("beatAML",
                                                         dataTableOutput("beatAML_anno")),
                                                tabPanel("tavor",
                                                         dataTableOutput("tavor_anno")),
                                                tabPanel("gCSI",
                                                         dataTableOutput("gCSI_anno")),
                                                tabPanel("FORALL",
                                                         dataTableOutput("FORALL_anno"))
                                              )),
                                     tabPanel("Cell Lines Abbreviation",
                                              wellPanel(p("Acute Lymphoblastic Leukemia: ALL"),
                                                        p("Acute Biphenotypic Leukemia: ABL"),
                                                        p("Acute Myelogenous Leukemia: AML"),
                                                        p("Chronic Lymphoblastic Leukemia: CLL"),
                                                        p("Chronic Myeloid Leukaemia: CML"),
                                                        p("Hairy Cell Leukemia: HCL"),
                                                        p("Acute Promyelocytic Leukemia: APL")
                                                        )
                                              )
                                   )),
                          )
                 ),
                 ## Contact ----
                 tabPanel("Contact",
                          fluidPage(
                            strong("Feel free to talk with me if you find any bugs or have any suggestions. :)"),
                            p(""),
                            p("Email: mugpeng@foxmail.com"),
                            p("github: https://github.com/mugpeng")
                          ))
)

# server ----
server <- function(input, output, session) {
  # Notification ----
  showModal(modal_notification)
  observeEvent(input$close_modal, {
    removeModal()
  })
  storeWarn <- getOption("warn")
  options(warn = -1) 
  # Drugs sensitivity display ----
  ## Choose drugs
  updateSelectizeInput(session = session, inputId = 'select_drug',
                       label = 'Drugs Selection', choices = unique(drugs_search$drugs), server = TRUE,
                       options = list(placeholder = 'Please select a drug', onInitialize = I('function() { this.setValue(""); }')),
                       selected = "LAPATINIB"
  )
  ## Collect data
  all_drugs <- reactive({
    list(
      "ctrp1" = ctrp1_drug,
      "ctrp2" = ctrp2_drug,
      "gdsc1" = gdsc1_drug,
      "gdsc2" = gdsc2_drug,
      "FORALL" = FORALL_drug,
      "gCSI" = gCSI_drug,
      "tavor" = tavor_drug
    )
  })
  all_drugs_anno <- reactive({
    list(
      "ctrp1" = ccle_anno,
      "ctrp2" = ccle_anno,
      "gdsc1" = gdsc_anno,
      "gdsc2" = gdsc_anno,
      "FORALL" = FORALL_anno2,
      "gCSI" = gCSI_anno,
      "tavor" = tavor_anno2
    )
  })
  pre_plot <- reactive({
    re <- lapply(1:length(all_drugs()), function(index){
      drugs_df <- all_drugs()[[index]]
      cells_anno <- as.data.frame(all_drugs_anno()[[index]])
      intersect_names <- intersect(rownames(drugs_df), cells_anno[,1])
      drugs_df <- drugs_df[match(intersect_names, rownames(drugs_df)),]
      cells_anno <- cells_anno[match(intersect_names, cells_anno[,1]),]
      if(input$select_drug %in% colnames(drugs_df)){
        data.frame(
          drugs = drugs_df[,colnames(drugs_df) %in% input$select_drug],
          lineage = cells_anno[,3]
        )
      } else {
        NULL
      }
    })
    names(re) <- names(all_drugs())
    re <- re[!sapply(re, is.null)]
    re
  })
  p_drug_sensitivity <- reactive({
    validate(
      need(length(pre_plot()) > 0, "You have not choosen yet.")
    )
    p_list <- lapply(1:length(pre_plot()), function(index){
      ggboxplot(data = pre_plot()[[index]], x = "lineage", y = "drugs",
                fill = "lineage",
                add = "jitter") + scale_fill_paletteer_d("RColorBrewer::Set3") +
        stat_compare_means() + theme_bw() + ggtitle(names(pre_plot())[index])
    })
    p <- wrap_plots(p_list) + plot_layout(guides = 'collect') &
      theme(legend.position='top')
    p
  })
  output$p_drug_sensitivity <- renderPlot(p_drug_sensitivity())
  output$downloadData_sens <- downloadHandler(
    filename = function() {
      paste("myplot", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file=file, width = 960, height = 480)
      plot(p_drug_sensitivity())
      dev.off()
    }
  )
  ## TSNE/SD-MEDIAN 
  p_drug_list <- reactive({switch(input$sensitivity_select, 
                             "GDSC1" = list(p_tsne_gdsc1,
                                            p_sd_gdsc1),
                             "GDSC2" = list(p_tsne_gdsc2,
                                            p_sd_gdsc2),
                             "CTRP1" = list(p_tsne_ctrp1,
                                            p_sd_ctrp1),
                             "CTRP2" = list(p_tsne_ctrp2,
                                            p_sd_ctrp2),
                             "FORALL" = list(p_tsne_forall,
                                             p_sd_FORALL),
                             "gCSI" = list(p_tsne_gCSI,
                                           p_sd_gCSI),
                             "Tavor" = list(p_tsne_tavor,
                                            p_sd_tavor),
                             "BeatAML" = list(p_tsne_beatAML,
                                              p_sd_BeatAML)
                             )
  })
  p_drug_list2 <- reactive({switch(input$sensitivity_radio, 
                             "TSNE" = p_drug_list()[[1]],
                             "SD" = p_drug_list()[[2]])
  })
  p_drug_list3 <- reactive({
    if(input$sensitivity_text_search == ""){
      p_drug_list2()
    } else{
      p_data1 <- p_drug_list2()$data
      index2 <- grepl(input$sensitivity_text_search, p_data1$Drugs, ignore.case = T)
      for_label <- p_data1[index2,]
      p_drug_list2() + geom_point(size = 4, shape = 1, data = for_label) +
        geom_label_repel(
          aes(label = Drugs),
          data = for_label,
          color="black")
    }
  })
  output$p_drug_sum <- plotly::renderPlotly({
    p_drug_list3()
  })
  # Drugs-omics pairs analysis ----
  ## Omics
  select_omics <- reactive(input$select_omics)
  omics_select_list1 <- DOPselectOmicsServer("DOPselectOmics", select_omics)[[1]]
  omics_select_list2 <- DOPselectOmicsServer("DOPselectOmics", select_omics)[[2]]
  ## Drugs
  drugs_select_list <- DOPselectDrugsServer("DOPselectDrugs", select_omics)[[1]]
  drug_search_df <- DOPselectDrugsServer("DOPselectDrugs", select_omics)[[2]]
  ## Before plot
  p_cor_serach <- DOPCorPlotServer("DOPPlot", drugs_select_list, omics_select_list1)
  p_box_serach <- DOPBoxPlotServer("DOPPlot", drugs_select_list, omics_select_list2,
                                   drug_search_df)
  ## Plot
  output$p_search <- renderPlot({
    if(input$select_omics %in% c("cnv",
                                        "protein",
                                        "meth",
                                        "mRNA")){p_cor_serach()}
    else{
      p_box_serach()
    }
  })
  output$downloadData_DO <- downloadHandler(
    filename = function() {
      paste("myplot", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file=file, width = 960, height = 480)
      p <- if(input$select_omics %in% c("cnv",
                                        "protein",
                                        "meth",
                                        "mRNA")){p_cor_serach()}
      else{
        p_box_serach()
      }
      plot(p)
      dev.off()
    }
  )
  # Features database significant analysis ----
  ## Feature select 
  select_FDS_types <- reactive(input$select_FDS_types)
  fea_sel_list <- FDSfeaSelServer("FDSfeaSel", select_FDS_types)
  ## Database select
  select_databases <- reactive(input$select_databases)
  database_list <- FDSdbSelServer("FDSdbSel", select_databases)
  # Sig calculation
  ## Cor
  fea_db_cor_list <- FDSCorServer2("FDSCor", fea_sel_list, database_list)
  sig_cor_plotly <- FDSvolcanoPlotServer("FDSvolcano", fea_db_cor_list)
  ## wilcox
  fea_db_wc1_list <- FDSWilcoxServer_feaDis2("FDSWilcox1", fea_sel_list, database_list)
  sig_wc1_plotly <- FDSvolcanoPlotServer("FDSvolcano", fea_db_wc1_list)
  fea_db_wc2_list <- FDSWilcoxServer_feaCon2("FDSWilcox2", fea_sel_list, database_list)
  sig_wc2_plotly <- FDSvolcanoPlotServer("FDSvolcano", fea_db_wc2_list)
  ## Fisher
  cell_names <- reactive({
    list(
      "ctrp1" = rownames(ctrp1_drug),
      "ctrp2" = rownames(ctrp2_drug),
      "gdsc1" = rownames(gdsc1_drug),
      "gdsc2" = rownames(gdsc2_drug),
      "tavor" = rownames(tavor_drug),
      "beatAML" = rownames(beatAML_drug),
      "gCSI" = rownames(gCSI_drug),
      "FORALL" = rownames(FORALL_drug)
    )
  })
  fea_db_fisher_list <- FDSFisherServer2("FDSFisher", fea_sel_list, database_list, cell_names)
  sig_fisher_plotly <- FDSvolcanoPlotServer("FDSvolcano", fea_db_fisher_list)
  # Download
  download_list <- reactive({
    if(input$select_FDS_types %in% c("cnv",
                                     "meth",
                                     "mRNA",
                                     "protein",
                                     "drug") &&
       input$select_databases %in% c("cnv",
                                     "meth",
                                     "mRNA",
                                     "protein",
                                     "drug")){
      fea_db_cor_list()
    } else if(!input$select_FDS_types %in% c("cnv",
                                             "meth",
                                             "mRNA",
                                             "protein",
                                             "drug") &&
              input$select_databases %in% c("cnv",
                                            "meth",
                                            "mRNA",
                                            "protein",
                                            "drug")){
      fea_db_wc1_list()
    } else if(input$select_FDS_types %in% c("cnv",
                                            "meth",
                                            "mRNA",
                                            "protein",
                                            "drug") &&
              !input$select_databases %in% c("cnv",
                                             "meth",
                                             "mRNA",
                                             "protein",
                                             "drug")){
      fea_db_wc2_list()
    } else{
      fea_db_fisher_list()
    }
  })
  # Plot
  output$p_sig_plotly <- plotly::renderPlotly({
    if(input$select_FDS_types %in% c("cnv",
                                     "meth",
                                     "mRNA",
                                     "protein",
                                     "drug") &&
       input$select_databases %in% c("cnv",
                                     "meth",
                                     "mRNA",
                                     "protein",
                                     "drug")){
      sig_cor_plotly()
    } else if(!input$select_FDS_types %in% c("cnv",
                                            "meth",
                                            "mRNA",
                                            "protein",
                                            "drug") &&
              input$select_databases %in% c("cnv",
                                            "meth",
                                            "mRNA",
                                            "protein",
                                            "drug")){
      sig_wc1_plotly()
    } else if(input$select_FDS_types %in% c("cnv",
                                            "meth",
                                            "mRNA",
                                            "protein",
                                            "drug") &&
              !input$select_databases %in% c("cnv",
                                            "meth",
                                            "mRNA",
                                            "protein",
                                            "drug")){
      sig_wc2_plotly()
    } else{
      sig_fisher_plotly()
    }
  })
  # output$test <- renderText({
  #   class(download_list())
  # })
  output$downloadData <- downloadHandler(
    filename =  paste("Leu-", Sys.Date(), ".Rds", sep=""),
    content = function(filename) {
      saveRDS(download_list(), 
              file = filename)
    })
  # Statistics and Annotations ----
  ## Plot
  output$p_count_subtype <- renderPlot({
    p_count_subtype
  })
  output$p_count_drugandcell <- renderPlot({
    p_count_drugandcell
  })
  output$downloadData_stat <- downloadHandler(
    filename = function() {
      paste("myplot", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file=file, width = 960, height = 480)
      p <- p_count_drugandcell + p_count_subtype
      plot(p)
      dev.off()
    }
  )
  ## Cell anno dataframe
  output$gdsc_anno <- renderDataTable({ 
    gdsc_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  output$ccle_anno <- renderDataTable({ 
    ccle_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  output$beatAML_anno <- renderDataTable({ 
    beatAML_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  output$tavor_anno <- renderDataTable({ 
    tavor_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  output$FORALL_anno <- renderDataTable({ 
    FORALL_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  output$gCSI_anno <- renderDataTable({ 
    gCSI_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  ## Drug anno dataframe
  output$gdsc1_drug_anno <- renderDataTable({ 
    gdsc1_drug_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  output$gdsc2_drug_anno <- renderDataTable({ 
    gdsc2_drug_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  output$ctrp1_drug_anno <- renderDataTable({ 
    ctrp1_drug_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  output$ctrp2_drug_anno <- renderDataTable({ 
    ctrp2_drug_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  output$beatAML_drug_anno <- renderDataTable({ 
    beatAML_drug_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  output$tavor_drug_anno <- renderDataTable({ 
    tavor_drug_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  output$gCSI_drug_anno <- renderDataTable({ 
    gCSI_drug_anno
  }, options = list(scrollX = TRUE), selection = 'single')
  output$FORALL_drug_anno <- renderDataTable({ 
    FORALL_drug_anno
  }, options = list(scrollX = TRUE), selection = 'single')
}

shinyApp(ui, server)