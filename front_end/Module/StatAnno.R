# SA is the abbreviation of Statistics_Annotations
SAallServer <- function(id, data){
  moduleServer(id, function(input, output, session){
    # Stats Plot ----
    output$p_count_subtype <- renderPlot({
      data()[[1]]
    })
    output$p_count_drugandcell <- renderPlot({
      data()[1]
    })
    # Cell anno dataframe ----
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
    # Drug anno dataframe ----
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
  })
}
