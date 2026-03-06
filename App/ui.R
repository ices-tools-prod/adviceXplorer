############# Libraries ############
library(data.table)
library(dplyr)
library(dygraphs)
library(DT)
library(htmltools)
library(htmlwidgets)
library(ggplot2)
library(ggradar)
library(ggtext)
library(glue)
library(gsubfn)
library(icesSAG)
library(icesTAF)
library(icesVocab)
library(leaflet)
library(plotly)
library(reshape2)
library(rintrojs)
library(RColorBrewer)
library(RCurl)
library(rvest)
library(scales)
library(sf)
library(shinyalert)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(stringr)
library(tidyr)
library(tidyverse)
library(tm)
library(widgetframe)
library(icesASD)
library(zip)
library(datamods)
library(reactable)
library(ggthemes)
library(bslib)

library(future)
library(promises)
library(data.table)
library(memoise)
library(future.apply)
library(cachem)

plan(multisession, workers = 2) # Enable parallel execution


########## Load utilities ############
source("utilities_dataPipeline.R")
source("utilities_SID_data.R")
source("update_SID_data.R")
source("utilities_load_shapefiles.R")
source("utilities_plotting.R")
source("utilities_mapping.R")
source("utilities_sag_data.R")
source("update_SAG_data.R")
source("utilities_catch_scenarios.R")
source("utilities_shiny_formatting.R")
source("utilities_resources.R")
source("utilities_ASD.R")


title_html <- tags$a(
  href = "https://ices-taf.shinyapps.io/advicexplorer/",
  tags$img(
    src = "NEGATIVE ICES-logo.png",
    style = "margin-top: -10px; padding-right:10px;padding-bottom:10px",
    height = "50px"
  )
)

options(
  spinner.type = 5,
  spinner.color = "#f15d22",
  spinner.size = 0.7
)


tagList(
  useShinyjs(),
  introjsUI(),
  tags$script(src = "https://kit.fontawesome.com/ac71e9cf8e.js"),
  tags$head(includeHTML(("google-analytics.html")), tags$link(rel = "shortcut icon", href = "X.png")),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/gothic-a1.css"),
  tags$style("body {font-family: 'Gothic A1', sans-serif;}"),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  tags$script( ##### we can modify this to have the tabs inactive until a stock is chosen
    '
    var tab = $(\'a[data-value="Stock Selection"]\').parent().addClass("disabled");
    $(function(){
      $(tab.parent()).on("click", "li.disabled", function(e) {
        e.preventDefault();
        return false;
      });
    });
    '
  ),
  tags$script(HTML("
  Shiny.addCustomMessageHandler('copyText', function(message) {
    if (!navigator.clipboard) {
      Shiny.setInputValue('share_copy_error', 'Clipboard API not available', {priority: 'event'});
      return;
    }
    navigator.clipboard.writeText(message.text).then(
      function(){ Shiny.setInputValue('share_copy_success', Date.now(), {priority: 'event'}); },
      function(err){ Shiny.setInputValue('share_copy_error', err.toString(), {priority: 'event'}); }
    );
  });
")),
tags$script(HTML("
  Shiny.addCustomMessageHandler('startupProgress', function(msg) {
    var el = document.getElementById('startup-progress-bar');
    var txt = document.getElementById('startup-progress-text');
    if (el) el.style.width = (msg.value || 0) + '%';
    if (txt) txt.textContent = msg.text || '';
  });
")),
  navbarPage(
    position = "static-top",
    collapsible = TRUE,
    # tab title
    windowTitle = "adviceXplorer",
    id = "tabset",
    fluid = TRUE,
    # navbar title
    title = title_html,
    tabPanel(
      "Stock selection",
      style = "max-height: 90vh; overflow-y: auto; overflow-x: hidden !important;",
      sidebarLayout(
        sidebarPanel = stock_selection_left_side(),
        mainPanel = stock_selection_right_side()
      )
    ),

    ########################################## New version of SAG plots ############################
    tabPanel(
      "Development over time",
      style = "max-height: 90vh; overflow-y: auto; overflow-x: hidden !important;",
      header_info_and_headline("stock_infos1", "Advice_Headline1"),
      sidebarPanel(
        width = 12,
        SAG_plots_1_2_fluid(),
        br(),
        SAG_plots_3_4_fluid(),
        br(),
        SAG_plots_custom_1_2_fluid(),
        br(),
        SAG_plots_custom_3_4_fluid()
      )
    ),
    tabPanel(
      "Quality of assessment",
      style = "overflow-y: auto; overflow-x: hidden;",
      header_info_and_headline("stock_infos2", "Advice_Headline2"),
      quality_of_assessment_fluid()
    ),

    ######################################################################################################

    tabPanel(
      "Catch scenarios",
      style = " max-height: 90vh; overflow-y: auto; overflow-x: hidden !important;",
      header_info_and_headline("stock_infos3", "Advice_Headline3"),
      mainPanel(
        width = 12,
        sidebarLayout(
          sidebarPanel = catch_scenarios_left_panel(),
          mainPanel = catch_scenarios_right_panel()
        )
      )
    ),
    bslib::nav_spacer(),
    bslib::nav_item(
      actionButton(
        "share_btn",
        label = "Share",
        icon  = icon("link"),
        class = "btn btn-default",
        style = "margin-right: 8px;"
      )
    ),
    tabPanel("Resources", mod_resources_ui("resources"))
  )
)
