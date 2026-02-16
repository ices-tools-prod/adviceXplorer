msg <- function(...) {
  emph <- "\n****************\n"
  cat(emph, ..., emph)
}


# required if using most recent version of sf
sf::sf_use_s2(FALSE)







############# Start server function ################

server <- function(input, output, session) {
  msg("server loop start:\n  ", getwd())
  shinyjs::disable(selector = '.navbar-nav a[data-value="Development over time"]')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Quality of assessment"]')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Catch scenarios"]')

  

  
  ############################################################################################
  # values of the query string and first visit flag
  query <- reactiveValues(query_from_table = FALSE)

  ############################ Map panel server ##############################################
  map_panel_server(input, output, session)

  
  ############################ Stock selection table server ######################################
  eco_filter <- reactive({
    req(input$selected_locations, input$selected_years)

    # stock_list_long <- fread(sprintf("Data/SID_%s/SID.csv", input$selected_years))
    # stock_list_long <- getSID(input$selected_years)
    stock_list_long <- getStockList_for_active_year(active_year = input$selected_years)

    browser()
    head(stock_list_long)
    stock_list_long <- purrr::map_dfr(
      .x = input$selected_locations,
      .f = function(.x) stock_list_long %>% dplyr::filter(str_detect(EcoRegion, .x))
    )

    if (nrow(stock_list_long) != 0) {
      stock_list_long %>%
        dplyr::arrange(StockKeyLabel)
    }
  }) %>%
    bindCache(input$selected_locations, input$selected_years) %>%
    bindEvent(input$selected_locations, input$selected_years)


  res_mod <- select_group_server(
    id = "my-filters",
    data = eco_filter,
    vars = reactive(c("StockKeyLabel", "SpeciesCommonName"))
  )

  ###########################################################  Render stock selection table

  res_modo <- reactive({
    validate(
      need(!nrow(eco_filter()) == 0, "No published stocks in the selected ecoregion and year")
    )

    if (length(input$selected_locations) > 1) {
      res_mod() %>%
        select(
          "StockKeyLabel",
          "AssessmentComponent",
          "EcoRegion",
          "icon",
          "SpeciesCommonName",
          # "YearOfLastAssessment",
          "stock_location"
        ) %>%
        mutate(AssessmentComponent = ifelse((is.na(AssessmentComponent)) | AssessmentComponent == "NA", "", AssessmentComponent)) %>%
        rename(
          "Stock code" = StockKeyLabel,
          "Component" = AssessmentComponent,
          "Ecoregion" = EcoRegion,
          " " = icon,
          "Common name" = SpeciesCommonName,
          # "Year of last assessment" = YearOfLastAssessment,
          "Location" = stock_location
        ) %>%
        {
          if (all(nchar(.$Component) == 0)) select(., -Component) else .
        }
    } else {
      res_mod() %>%
        select(
          "StockKeyLabel",
          "AssessmentComponent",
          "icon",
          "SpeciesCommonName",
          # "YearOfLastAssessment",
          "stock_location"
        ) %>%
        mutate(AssessmentComponent = ifelse((is.na(AssessmentComponent)) | AssessmentComponent == "NA", "", AssessmentComponent)) %>%
        rename(
          "Stock code" = StockKeyLabel,
          "Component" = AssessmentComponent,
          " " = icon,
          "Common name" = SpeciesCommonName,
          # "Year of last assessment" = YearOfLastAssessment,
          "Location" = stock_location
        ) %>%
        {
          if (all(nchar(.$Component) == 0)) select(., -Component) else .
        }
    }
  })


  output$tbl <- renderReactable({
    reactable(res_modo(),
      selection = "single",
      filterable = TRUE,
      onClick = "select",
      highlight = TRUE,
      defaultPageSize = 100,
      striped = TRUE,
      defaultColDef = colDef(
        headerStyle = list(background = "#99AABF")
      ),
      columns = list(
        " " = colDef(
          html = TRUE,
          filterable = FALSE,
          align = "center",
          aggregate = "unique"
        )
        # "Year of last assessment" = colDef(
        #   filterable = TRUE,
        #   align = "left"
        # )
      ),
      theme = reactableTheme(
        stripedColor = "#eff2f5",
        highlightColor = "#f9b99f",
        cellPadding = "2px 2px"
      )
    )
  })



  selected <- reactive(getReactableState("tbl", "selected"))


  observeEvent(selected(), {
    shinyjs::enable(selector = '.navbar-nav a[data-value="Development over time"]')
    shinyjs::enable(selector = '.navbar-nav a[data-value="Quality of assessment"]')
    shinyjs::enable(selector = '.navbar-nav a[data-value="Catch scenarios"]')

    filtered_row <- res_mod()[selected(), ]

    # Update server-side state immediately
    query$assessmentkey <- filtered_row$AssessmentKey
    query$assessmentcomponent <- filtered_row$AssessmentComponent %||% "NA"

    # Write URL (push to history)
    nav$restoring <- TRUE
    updateQueryString(
      make_url_search(query$assessmentkey, query$assessmentcomponent, "Development over time"),
      mode = "push"
    )

    query$query_from_table <- TRUE

    updateNavbarPage(session, "tabset", selected = "Development over time")

    nav$restoring <- FALSE
  })


  ################################## Bookmarking code #############################################

  # Track whether we're in a restore phase (prevents writer loops)
  nav <- reactiveValues(restoring = TRUE)

  observeEvent(session$clientData$url_search,
    {
      nav$restoring <- TRUE
      on.exit(
        {
          nav$restoring <- FALSE
        },
        add = TRUE
      )

      qs <- shiny::parseQueryString(session$clientData$url_search %||% "")
      names(qs) <- tolower(names(qs))

      ak <- qs$assessmentkey %||% ""
      ac0 <- qs$assessmentcomponent
      tab <- qs$tab %||% ""

      # Backward compatibility: old URLs had no tab -> go to Development over time
      if (!nzchar(tab) && nzchar(ak)) tab <- "Development over time"
      # If no stock and no tab specified, do nothing (stay at UI default)

      # Only update stock state if assessmentkey actually changed (prevents refetch on tab-only changes)
      cur_ak <- isolate(query$assessmentkey %||% "")

      if (nzchar(ak) && !identical(ak, cur_ak) && !isTRUE(query$query_from_table)) {
        query$assessmentkey <- ak

        if (is.null(ac0) || !nzchar(ac0)) ac0 <- "NA"
        query$assessmentcomponent <- modify_assessment_component(ac0)

        info <- getStockInfoFromSAG(query$assessmentkey)
        query$stockkeylabel <- info$StockKeyLabel
        query$year <- info$AssessmentYear

        shinyjs::enable(selector = '.navbar-nav a[data-value="Development over time"]')
        shinyjs::enable(selector = '.navbar-nav a[data-value="Quality of assessment"]')
        shinyjs::enable(selector = '.navbar-nav a[data-value="Catch scenarios"]')
      }

      # Restore tab (even if stock already loaded)
      if (nzchar(tab) && !identical(isolate(input$tabset %||% ""), tab)) {
        updateNavbarPage(session, "tabset", selected = tab)
      }
    },
    ignoreInit = FALSE
  )


  observeEvent(input$tabset,
    {
      if (isTRUE(nav$restoring)) {
        return()
      }

      new_search <- make_url_search(query$assessmentkey, query$assessmentcomponent, input$tabset)
      cur_search <- session$clientData$url_search %||% ""

      if (!identical(cur_search, new_search)) {
        updateQueryString(new_search, mode = "replace")
      }
    },
    ignoreInit = TRUE
  )

  ################################ End bookmarking code ###################################################

  ######### SAG data
  SAG_data_reactive <- reactive({
    info <- getStockInfoFromSAG(query$assessmentkey)

    query$stockkeylabel <- info$StockKeyLabel
    query$year <- info$AssessmentYear ####
    query$sagStamp <- info$SAGStamp

    stock_name <- query$stockkeylabel
    msg("downloading:", stock_name)

    year <- query$year #####
    msg("downloading:", year)
    # filtered_row <- res_mod()[selected(), ]
    #   # Dowload the data
    getSAGData(query$assessmentkey)
    # getSAGData(stock_code = stock_name, year = filtered_row$YearOfLastAssessment) %>%
    #   filter(AssessmentKey == query$assessmentkey)
  })

  sagSettings <- reactive({
    temp_setting <- icesSAG::getSAGSettingsForAStock(query$assessmentkey)
    temp_setting[!(temp_setting$settingValue == ""), ]
  })

  drop_plots <- reactive({
    filter(sagSettings(), settingKey == 22 & settingValue == "yes" | settingValue == "y") %>%
      pull(SAGChartKey) %>%
      as.numeric()
  })

  # ##### get link to library pdf advice
  # advice_doi <- eventReactive((req(SAG_data_reactive())),{
  #   SAG_data_reactive()$LinkToAdvice[1]
  # })

  replaced_advice_doi <- eventReactive(req(query$assessmentkey), {
    get_link_replaced_advice(SAG_data_reactive())
  })


  ###### info about the stock selected for top of page
  stock_info <- reactive({
    filtered_row <- res_mod()[res_mod()$AssessmentKey == query$assessmentkey, ]
    # Conditional check if filtered_row is empty
    if (nrow(filtered_row) == 0) {
      filtered_row <- icesSD::getSD(query$stockkeylabel, query$year)
    }

    # filtered_row <- res_mod()[res_mod()$AssessmentKey == query$assessmentkey,]


    get_Stock_info(filtered_row$SpeciesCommonName[1], query$stockkeylabel, SAG_data_reactive()$AssessmentYear[1], query$assessmentcomponent, SAG_data_reactive()$StockDescription[1])
  })

  output$stock_infos1 <- output$stock_infos2 <- output$stock_infos3 <- renderUI(
    stock_info()
  )

  ##### advice headline (right side of page)
  advice_view_headline <- reactive({
    get_Advice_View_Headline(advice_view_info(), replaced_advice_doi(), input$tabset, catch_scenario_table()$table, drop_plots())
  })

  output$Advice_Headline1 <- output$Advice_Headline2 <- output$Advice_Headline3 <- renderUI({
    advice_view_headline()
  })


  output$download_SAG_Data <- downloadHandler(
    filename = paste0("adviceXplorer_data-", Sys.Date(), ".zip"),
    content = function(fname) {
      fs <- c("Disclaimer.txt", paste0(SAG_data_reactive()$StockKeyLabel[1], "-adviceXplorer_SAG_data.csv"))

      data_out <- SAG_data_reactive() %>%
        select(where(~ !all(is.na(.)))) %>%
        mutate(across(where(is.numeric), ~ format(., decimal.mark = ".", scientific = FALSE)))

      write.csv(data_out, file = paste0(SAG_data_reactive()$StockKeyLabel[1], "-adviceXplorer_SAG_data.csv"), row.names = FALSE, quote = FALSE)
      write.table(read.delim("https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_adviceXplorer.txt"), file = "Disclaimer.txt", row.names = FALSE)

      zip::zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )

  ######################### Stock development over time plots

  output$plot1 <- renderPlotly({
    if (is.null(sagSettings() %>% filter(SAGChartKey == 1) %>% filter(settingKey == 22) %>% pull(settingValue) %>% nullifempty())) {
      validate(
        need(c(SAG_data_reactive()$Landings, SAG_data_reactive()$Catches) != "", "") # ,
        # need(all(!c(0, 1) %in% drop_plots()), "Figure not included in the published advice for this stock")
      )
      suppressWarnings(ICES_plot_1(SAG_data_reactive(), sagSettings(), query$sagStamp))
    } else {
      return(NULL)
    }
  })

  output$plot2 <- renderPlotly({
    if (is.null(sagSettings() %>% filter(SAGChartKey == 2) %>% filter(settingKey == 22) %>% pull(settingValue) %>% nullifempty())) {
      validate(
        need(SAG_data_reactive()$Recruitment != "", "")
      )
      suppressWarnings(ICES_plot_2(SAG_data_reactive(), sagSettings(), query$sagStamp))
    } else {
      return(NULL)
    }
  })

  output$plot3 <- renderPlotly({
    if (is.null(sagSettings() %>% filter(SAGChartKey == 3) %>% filter(settingKey == 22) %>% pull(settingValue) %>% nullifempty())) {
      validate(
        need(SAG_data_reactive()$FishingPressure != "", "")
      )
      suppressWarnings(ICES_plot_3(SAG_data_reactive(), sagSettings(), query$sagStamp))
    } else {
      return(NULL)
    }
  })

  output$plot4 <- renderPlotly({
    if (is.null(sagSettings() %>% filter(SAGChartKey == 4) %>% filter(settingKey == 22) %>% pull(settingValue) %>% nullifempty())) {
      validate(
        need(SAG_data_reactive()$StockSize != "", "")
      )
      suppressWarnings(ICES_plot_4(SAG_data_reactive(), sagSettings(), query$sagStamp))
    } else {
      return(NULL)
    }
  })

  output$customPlot1 <- renderPlotly({
    if (nrow(sagSettings() %>% filter(SAGChartKey == 15)) >= 1) {
      suppressWarnings(ICES_custom_plot(SAG_data_reactive(), sagSettings(), ChartKey = 15, query$sagStamp))
    } else {
      return(NULL)
    }
  })

  output$customPlot2 <- renderPlotly({
    if (nrow(sagSettings() %>% filter(SAGChartKey == 16)) >= 1) {
      suppressWarnings(ICES_custom_plot(SAG_data_reactive(), sagSettings(), ChartKey = 16, query$sagStamp))
    } else {
      return(NULL)
    }
  })

  output$customPlot3 <- renderPlotly({
    if (nrow(sagSettings() %>% filter(SAGChartKey == 17)) >= 1) {
      suppressWarnings(ICES_custom_plot(SAG_data_reactive(), sagSettings(), ChartKey = 17, query$sagStamp))
    } else {
      return(NULL)
    }
  })

  output$customPlot4 <- renderPlotly({
    if (nrow(sagSettings() %>% filter(SAGChartKey == 18)) >= 1) {
      suppressWarnings(ICES_custom_plot(SAG_data_reactive(), sagSettings(), ChartKey = 18, query$sagStamp))
    } else {
      return(NULL)
    }
  })

  ####################### Quality of assessment data
  advice_action_quality <- reactive({
    # Extract custom data and graph type
    yearsToDisplay <- sagSettings() %>%
      filter(settingKey == 58) %>%
      pull(settingValue) %>%
      # str_split(",", simplify = TRUE) %>%
      as.numeric() %>%
      nullifempty()

    getSAGQualityAssessment(query$stockkeylabel, query$year, query$assessmentcomponent, yearsToDisplay)
  })



  ##### button to download SAG data for quality of assessemnt
  output$download_QualAss_Data <- downloadHandler(
    filename = paste0("adviceXplorer_data-", Sys.Date(), ".zip"),
    content = function(fname) {
      fs <- c("Disclaimer.txt", paste0(SAG_data_reactive()$StockKeyLabel[1], "-adviceXplorer_QualityofAssessment_data.csv"))

      data_out_qual <- advice_action_quality() %>%
        select(where(~ !all(is.na(.)))) %>%
        mutate(across(where(is.numeric), ~ format(., decimal.mark = ".", scientific = FALSE)))

      write.csv(data_out_qual, file = paste0(SAG_data_reactive()$StockKeyLabel[1], "-adviceXplorer_QualityofAssessment_data.csv"), row.names = FALSE, quote = FALSE)
      write.table(read.delim("https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_adviceXplorer.txt"), file = "Disclaimer.txt", row.names = FALSE)

      zip::zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )

  ######################### quality of assessment plots
  output$plot5 <- renderPlotly({
    validate(
      need(advice_action_quality()$StockSize != "", "SSB not available for this stock"),
      need(all(!10 %in% drop_plots()), "Figure not included in the published advice for this stock")
    )

    suppressWarnings(ICES_plot_5(advice_action_quality(), sagSettings(), query$sagStamp))
  })
  output$plot6 <- renderPlotly({
    validate(
      need(advice_action_quality()$FishingPressure != "", "FishingPressure not available for this stock"),
      need(all(!10 %in% drop_plots()), "Figure not included in the published advice for this stock")
    )

    suppressWarnings(ICES_plot_6(advice_action_quality(), sagSettings(), query$sagStamp))
  })
  output$plot7 <- renderPlotly({
    validate(
      need(advice_action_quality()$Recruitment != "", "Recruitment not available for this stock"),
      need(all(!10 %in% drop_plots()), "Figure not included in the published advice for this stock")
    )
    suppressWarnings(ICES_plot_7(advice_action_quality(), sagSettings(), query$sagStamp))
  })

  #### this function is used to replace N.A. with NA in the assessment component, it's just a placeholder
  # until I fix the ASD package
  # replace_na_with_na_string <- function(assessment_component) {
  #   if (assessment_component == "NA") {
  #     return("N.A.")
  #   } else {
  #     return(assessment_component)
  #   }
  # }
  # ##### ASD info
  # advice_view_info <- reactive({
  #   browser()
  #   asd_record <- getAdviceViewRecord(assessmentKey = query$assessmentkey)
  #   if (!is_empty(asd_record)) {
  #     asd_record <- asd_record %>% filter(
  #       adviceViewPublished == TRUE,
  #       adviceStatus == "Advice",
  #       adviceComponent == replace_na_with_na_string(query$assessmentcomponent)
  #     )
  #   }
  # })
  replace_na_with_na_string <- function(assessment_component) {
    if (is.na(assessment_component) || assessment_component == "NA") {
      return("N.A.")
    } else {
      return(assessment_component)
    }
  }

  advice_view_info <- reactive({
    asd_record <- getAdviceViewRecord(assessmentKey = query$assessmentkey)

    if (!is_empty(asd_record)) {
      target_component <- replace_na_with_na_string(query$assessmentcomponent)

      asd_record <- asd_record %>% filter(
        adviceViewPublished == TRUE,
        adviceStatus == "Advice",
        adviceComponent == target_component | (is.na(adviceComponent) & target_component == "N.A.")
      )
    }
  })



  ##### ASD info previous year
  advice_view_info_previous_year <- eventReactive(req(query$stockkeylabel, query$year), {
    filtered_row <- res_mod()[res_mod()$AssessmentKey == query$assessmentkey, ]
    # Conditional check if filtered_row is empty
    if (nrow(filtered_row) == 0) {
      filtered_row <- icesSD::getSD(query$stockkeylabel, query$year)
    }

    asd_record_previous <- getAdviceViewRecord(query$stockkeylabel, query$year - filtered_row$AssessmentFrequency[1])

    # this is a fix to cover an exeption (like aru.27.123a4) when the assessment frequency is 2 but there is an advice in the previous year.
    if (is_empty(asd_record_previous)) {
      asd_record_previous <- try(getAdviceViewRecord(query$stockkeylabel, query$year))
    }

    if (!is_empty(asd_record_previous)) {
      asd_record_previous <- asd_record_previous %>% filter(
        adviceViewPublished == TRUE,
        adviceStatus == "Advice",
        adviceComponent == replace_na_with_na_string(query$assessmentcomponent)
      )
    }
  })



  ##### catch scenarios table
  catch_scenario_table <- eventReactive(req(advice_view_info()), {
    standardize_catch_scenario_table(icesASD::get_catch_scenario_table(advice_view_info()$adviceKey, query$year))
  })

  ##### catch scenarios table previous year in percentages (for radial plot)
  catch_scenario_table_previous_year <- eventReactive(req(advice_view_info_previous_year()), {
    standardize_catch_scenario_table(icesASD::get_catch_scenario_table(advice_view_info_previous_year()$adviceKey, query$year))
  })

  ##### catch scenario table scaled with the values of previous advice to get percentage of change
  catch_scenario_table_percentages <- eventReactive(req(catch_scenario_table_previous_year(), catch_scenario_table()), {
    validate(
      need(!is_empty(catch_scenario_table_previous_year()$table), "No catch scenario table in previous assessment year"),
      need(!is_empty(catch_scenario_table()$table), "No catch scenario table in this assessment year")
    )

    scale_catch_scenarios_for_radialPlot(catch_scenario_table_previous_year()$table, catch_scenario_table()$table)
  })


  ### F_SSB and chatches plot linked to table
  output$catch_scenario_plot_F_SSB_Catch <- renderPlotly({
    validate(
      need(!is_empty(catch_scenario_table()$table), "Catch scenarios not available for this stock")
    )

    if (str_detect(tail(query$stockkeylabel), "nep")) {
      catch_scenario_plot_1_nephrops(catch_scenario_table(), SAG_data_reactive(), sagSettings())
    } else {
      catch_scenario_plot_1(catch_scenario_table(), SAG_data_reactive(), sagSettings())
    }
  })

  ########## Historical catches panel (preparation of data)
  test_table <- eventReactive(catch_scenario_table(), {
    req(query$stockkeylabel, query$year)
    validate(
      need(!is_empty(catch_scenario_table()$table), ""),
      need(!is_empty(advice_view_info_previous_year()), "No ASD entry in previous assessment year")
    )
    wrangle_catches_with_scenarios(SAG_data_reactive(), query$assessmentkey, catch_scenario_table()$table, advice_view_info_previous_year()$adviceValue, advice_view_info_previous_year()$adviceApplicableUntil, query$year)
  })

  ########## Historical catches panel (Definition of basis of advice)
  Basis <- eventReactive(catch_scenario_table(), {
    catch_scenario_table()$table[catch_scenario_table()$table$cS_Purpose == "Basis Of Advice", ]
  })

  ########## Historical catches panel (Selection panel)
  output$catch_scenarios <- renderUI({
    if (!is_empty(test_table())) {
      virtualSelectInput(
        inputId = "catch_choice",
        label = "Select one or more catch scenarios:",
        choices = unique(test_table()$Scenario),
        selected = c("Historical Catches", "Previous advice", Basis()$Scenario),
        multiple = TRUE,
        width = "100%",
        search = TRUE
      )
    } else {
      HTML("No data available")
    }
  })

  ########## Historical catches panel (Plot)
  output$TAC_timeline <- renderPlotly({
    validate(
      need(!is_empty(catch_scenario_table()$table), "Catch scenarios not available for this stock")
    )
    TAC_timeline(test_table(), input$catch_choice, SAG_data_reactive())
  })

  # Update plot using plotlyProxy when scenarios change
  eventReactive(input$catch_choice, {
    # Filter data based on selected scenarios
    filtered_data <- test_table() %>% filter(Scenario %in% input$catch_choice)

    # Prepare lists for the `restyle` method, specifying color for each scenario
    x_values <- split(filtered_data$Year, filtered_data$Scenario)
    y_values <- split(filtered_data$Catches, filtered_data$Scenario)
    colors <- split(filtered_data$Color, filtered_data$Scenario) %>% lapply(unique)
    # markers = split(filtered_data$MarkerSize, filtered_data$Scenario) %>% lapply(unique)

    plotlyProxy("TAC_timeline", session) %>%
      plotlyProxyInvoke("restyle", list(
        x = x_values,
        y = y_values,
        # "line.color" = colors # Use colors from the Color column
        colors = colors
      ))
  })


  output$download_TAC_Data <- downloadHandler(
    filename = paste0("adviceXplorer_data-", Sys.Date(), ".zip"),
    content = function(fname) {
      fs <- c("Disclaimer.txt", paste0(SAG_data_reactive()$StockKeyLabel[1], "-adviceXplorer_HistCatches_data.csv"))
      write.csv(test_table(), file = paste0(SAG_data_reactive()$StockKeyLabel[1], "-adviceXplorer_HistCatches_data.csv"))
      write.table(read.delim("https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_adviceXplorer.txt"), file = "Disclaimer.txt", row.names = FALSE)

      zip::zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )

  output$TAC_download <- renderUI({
    validate(
      need(!is_empty(catch_scenario_table()$table), "")
    )
    HTML(paste0(
      "<br/>",
      "<span class='hovertext' data-hover='Catch time series data download'>",
      downloadLink("download_TAC_Data", HTML("<font size= 3>Download catch time series data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
    ))
  })
  ############ Radial plot panel (Selection panel)
  output$catch_scenarios_radial <- renderUI({
    if (!is_empty(catch_scenario_table_previous_year()$table)) {
      virtualSelectInput(
        inputId = "catch_choice_radial",
        label = "Select one or more catch scenarios:",
        choices = unique(catch_scenario_table_percentages()$Scenario),
        selected = c(Basis()$Scenario),
        multiple = TRUE,
        width = "100%",
        search = TRUE
      )
    } else {
      HTML("")
    }
  })

  ############ Radial plot panel (radial plot)
  output$Radial_plot <- renderPlotly({
    validate(
      need(!is_empty(advice_view_info()), "No ASD entry in assessment year"),
      need(!is_empty(advice_view_info_previous_year()), "No ASD entry in previous assessment year")
    )
    radial_plot(catch_scenario_table_percentages(), input$catch_choice_radial)
  })

  output$Radial_plot_disclaimer <- renderUI(
    HTML("<br><br> Disclaimer: the relative change for F, F wanted and HR has been calculated using the basis of advice of the previous year assessment. <br/>
  The scale of the plot is relative across the scenarios presented, please refer to the table or the % of change plot for actual percentage of change.")
  )
  ############ Lollipop plot panel (Selection panel)
  output$catch_indicators_lollipop <- renderUI({
    if (!is_empty(catch_scenario_table_previous_year()$table)) {
      virtualSelectInput(
        inputId = "indicator_choice_lollipop",
        label = "Select one ore more indicators:",
        choices = catch_scenario_table_percentages() %>%
          select(where(~ !any(is.na(.)))) %>%
          names() %>%
          str_subset(pattern = c("Year|Scenario|cS_Purpose"), negate = TRUE),
        selected = c("SSB change"),
        multiple = TRUE,
        width = "100%",
        search = TRUE
      )
    } else {
      HTML("")
    }
  })

  ############ Lollipop plot panel (Lollipop plot)
  output$Lollipop_plot <- renderPlotly({
    validate(
      need(!is_empty(advice_view_info()), "No ASD entry in assessment year"),
      need(!is_empty(advice_view_info_previous_year()), "No ASD entry in previous assessment year")
    )
    lollipop_plot(catch_scenario_table_percentages(), input$indicator_choice_lollipop)
  })

  output$lollipop_plot_disclaimer <- renderUI(
    HTML("<br> <br> Disclaimer: the relative change for F, F wanted and HR has been calculated using the basis of advice of the previous year assessment.")
  )


  ###### Calendar of stock with modal
  observeEvent(input$preview, {
    # Show a modal when the button is pressed
    shinyalert(
      title = " Advice Calendar",
      tags$body(HTML(html_calendar(advice_view_info(), res_mod(), input$rdbtn))),
      type = "info",
      html = TRUE,
      closeOnClickOutside = TRUE,
      confirmButtonText = "Close",
      size = "s",
    )
  })


  catch_scenario_table_collated <- eventReactive(catch_scenario_table(), {
    validate(
      need(!is_empty(catch_scenario_table()$table), "Catch scenarios not available for this stock")
    )

    catch_scenario_table()$table %>%
      arrange(cS_Purpose) %>%
      rename_all(~ catch_scenario_table()$cols) %>%
      rename("Basis" = cS_Label) %>% # , " " = cS_Purpose
      select_if(~ !(all(is.na(.)) | all(. == "")))
  })




  output$table <- renderReactable({
    reactable(catch_scenario_table_collated() %>% select(!(Year)),
      rowStyle = function(index) {
        if (catch_scenario_table_collated()[index, "cS_Purpose"] == "Basis Of Advice") list(fontWeight = "bold")
      },
      filterable = TRUE,
      highlight = TRUE,
      defaultPageSize = 100,
      striped = TRUE,
      defaultColDef = colDef(
        headerStyle = list(background = "#99AABF", borderRight = "1px solid #eee")
      ),
      columns = list(
        "cS_Purpose" = colDef(
          show = FALSE
        )
      ),
      theme = reactableTheme(
        stripedColor = "#eff2f5",
        highlightColor = "#f9b99f",
        cellPadding = "2px 2px"
      )
    )
  })

  output$download_catch_table <- downloadHandler(
    filename = paste0(query$stockkeylabel, "-adviceXplorer_data-", Sys.Date(), ".zip"),
    content = function(fname) {
      fs <- c("Disclaimer.txt", "adviceXplorer_catchScenario_data.csv", "adviceXplorer_catchScenarioNotes_data.csv")
      write.csv(icesASD::get_catch_scenario_table(advice_view_info()$adviceKey, query$year), file = "adviceXplorer_catchScenario_data.csv")
      write.csv(icesASD::getCatchScenariosNotes(advice_view_info()$adviceKey), file = "adviceXplorer_catchScenarioNotes_data.csv")
      write.table(read.delim("https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_adviceXplorer.txt"), file = "Disclaimer.txt", row.names = FALSE)

      zip::zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )


  ##### footnotes of catch scenario table
  footnotes <- eventReactive(req(advice_view_info()), {
    format_catch_scenario_notes(advice_view_info()$adviceKey)
  })
  output$footnotes <- renderUI({
    validate(
      need(!is_empty(footnotes()), " ")
    )

    footnotes()
  })


  mod_resources_server("resources")


  share_url <- reactiveVal("")

  observeEvent(input$share_btn, {
    ak <- query$assessmentkey %||% ""
    ac <- query$assessmentcomponent %||% "NA"
    tab <- input$tabset %||% ""

    # If no stock selected yet, you can still share the app root (optional)
    final <- if (nzchar(ak)) {
      paste0(.base_url(session), make_url_search(ak, ac, tab))
    } else {
      paste0(.base_url(session), make_url_search(NULL, NULL, tab))
    }

    share_url(final)

    showModal(modalDialog(
      title = "Share this view",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("copy_share_link", "Copy link", icon = icon("copy"), class = "btn btn-primary btn-sm")
      ),
      tags$p("This link reproduces this exact view:"),
      tags$div(
        style = "word-break: break-all; padding: 6px; border: 1px solid #ddd; border-radius: 4px;",
        final
      ),
      tags$div(
        style = "margin-top: 8px;",
        tags$a(href = final, target = "_blank", rel = "noopener", "Open in new tab")
      ),
      size = "m"
    ))
  })

  observeEvent(input$copy_share_link, {
    session$sendCustomMessage("copyText", list(text = share_url()))
  })

  observeEvent(input$share_copy_success, {
    showNotification("Link copied to clipboard", type = "message")
  })

  observeEvent(input$share_copy_error, {
    showNotification(paste("Copy failed:", input$share_copy_error), type = "error")
  })
}
