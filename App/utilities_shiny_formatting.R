#' Creates the UI element for the left side of the stock selection tab, which includes
#' the ecoregion map and the additional filterinr panel
#'
#' @return UI element
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
stock_selection_left_side <- function() {
  sidebarPanel(
    width = 4,
    tabPanel(
      "ICES Ecoregions",
      tags$img(id = "logo", class = "center-block", src = "adviceXplorer logo_color.png"),
      withSpinner(leafletOutput("map1", height = "100%", width = "100%"))
    ),
    HTML("</br>"),
    virtualSelectInput(
      inputId = "selected_locations",
      label = "ICES Ecoregions:",
      choices = sort(shape_eco$Ecoregion),
      selected = "Greater North Sea",
      multiple = TRUE,
      width = "100%",
      search = TRUE,
      optionsCount = 5
      ),
    virtualSelectInput(
      inputId = "selected_years",
      label = "Active year:",
      choices = Years$Year,
      selected = as.numeric(format(Sys.Date(), "%Y")),
      multiple = FALSE,
      width = "100%",
      search = TRUE,
      optionsCount = 5
    ),
    select_group_ui(
      id = "my-filters",
      params = list(
        StockKeyLabel = list(inputId = "StockKeyLabel", label = "Stock code:"),
        SpeciesCommonName = list(inputId = "SpeciesCommonName", label = "Common name:")
      ),
      inline = FALSE,
      vs_args = list(search = TRUE,
                    optionsCount = 5)
    ),
    htmlOutput("app_last_update")
  ) 
}

#' Creates the UI element for the right side of the stock selection tab, which includes
#' the stock list table
#'
#' @return UI element
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
stock_selection_right_side <- function(){
  mainPanel(
    width = 8,
    style = "overflow-x: auto; background-color:#e6e7e8;",
    HTML("<br/><b><font size= 5> Stock selection</b></font></br><font size= 4> To select a stock, click on the corresponding button on the left side of the table. </font><br/><br/>"),
    withSpinner(reactableOutput("tbl"))
  )
}


################################## SAG plots tab

#' Creates the UI element of the left panel of the "Stock dev over time" tab,
#' which includes the landings and F plots
#'
#' @return UI element
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
SAG_plots_left_panel <- function() {
  sidebarPanel(
    width = 6,
    withSpinner(plotlyOutput("plot1", height = "100%", width = "100%")),
    withSpinner(plotlyOutput("plot3", height = "100%", width = "100%"))
  )
}


#' Creates the UI element of the right panel of the "Stock dev over time" tab,
#' which includes the recruitment and SSB plots.
#'
#' @return UI element
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
SAG_plots_right_panel <- function() {
  sidebarPanel(
    width = 6,
    withSpinner(plotlyOutput("plot2", height = "100%", width = "100%")),
    withSpinner(plotlyOutput("plot4", height = "100%", width = "100%"))
  )
}

#' Creates the UI element of the right panel of the "Stock dev over time" tab,
#' which includes the landings and recruitment.
#'
#' @return UI element
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
SAG_plots_1_2_fluid <- function() {
  fluidRow(
    column(6, withSpinner(plotlyOutput("plot1", height = "100%", width = "100%"))),
    
    column(6, withSpinner(plotlyOutput("plot2", height = "100%", width = "100%")))
  )
}

#' Creates the UI element of the right panel of the "Stock dev over time" tab,
#' which includes the F and SSB.
#'
#' @return UI element
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
SAG_plots_3_4_fluid <- function() {
  fluidRow(
    column(6, withSpinner(plotlyOutput("plot3", height = "100%", width = "100%"))),    
    column(6, withSpinner(plotlyOutput("plot4", height = "100%", width = "100%")))

  )
}


SAG_plots_custom_1_2_fluid <- function() {
  fluidRow(
    column(6, withSpinner(plotlyOutput("customPlot1", height = "100%", width = "100%"))),
    column(6, withSpinner(plotlyOutput("customPlot2", height = "100%", width = "100%")))
    
  )
}

SAG_plots_custom_3_4_fluid <- function() {
  fluidRow(
    
    column(6, withSpinner(plotlyOutput("customPlot3", height = "100%", width = "100%"))),
    column(6, withSpinner(plotlyOutput("customPlot4", height = "100%", width = "100%")))
  )
}
##############################################Quality of assessment tab

#' Creates the UI element of the quality of assessment plots
#'
#' @return UI element
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
quality_of_assessment <- function(){
  splitLayout(
    cellWidths = c("33%", "33%", "33%"),
    cellArgs = list(style = "padding: 6px"),
    panel(
      title = "5",
      fillPage(
        tags$style(type = "text/css", "#plot5  overflow-y: auto; !important;}"), # {height: calc(5vh - 10px); width:calc(100vw - 10px)
        withSpinner(plotlyOutput("plot5", height = "100%", width = "100%"))
      )
    ),
    panel(
      title = "6",
      fillPage(
        tags$style(type = "text/css", "#plot6  overflow-y: auto; !important;}"), # {height: calc(5vh - 10px); width:calc(100vw - 10px)
        withSpinner(plotlyOutput("plot6", height = "100%", width = "100%"))
      )
    ),
    panel(
      title = "7",
      fillPage(
        tags$style(type = "text/css", "#plot7  overflow-y: auto; !important;}"), # {height: calc(5vh - 10px); width:calc(100vw - 10px)
        withSpinner(plotlyOutput("plot7", height = "100%", width = "100%"))
      )
    )
  )
}

#' Creates the UI element of the quality of assessment plots
#'
#' @return UI element
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
quality_of_assessment_fluid <- function() {
  sidebarPanel(
    width = 12,
  fluidRow(
    column(4, withSpinner(plotlyOutput("plot5", height = "100%", width = "100%"))),
    
    column(4, withSpinner(plotlyOutput("plot6", height = "100%", width = "100%"))),
    
    column(4, withSpinner(plotlyOutput("plot7", height = "100%", width = "100%")))
  )
  )
}
####################################### Advice tab

#' Creates the UI element of left panel of the Advice tab, which includes 
#' the F/SSB/Catches plot, the historical catches plot, the radial plot
#' and the lollipop plot.
#' 
#' @return UI element
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
catch_scenarios_left_panel <- function() {
  sidebarPanel(
    width = 6,
    withSpinner(plotlyOutput("catch_scenario_plot_F_SSB_Catch", height = "30%", width = "100%")),
    br(),
    tabsetPanel(
      tabPanel(
        "Catch time series",
        uiOutput("catch_scenarios"),
        withSpinner(plotlyOutput("TAC_timeline", height = "100%", width = "100%")),
        uiOutput("TAC_download")       
      ),
      tabPanel(
        "Relative change: radial plot",
        uiOutput("catch_scenarios_radial"),
        withSpinner(plotlyOutput("Radial_plot", height = "100%", width = "100%")),
        htmlOutput("Radial_plot_disclaimer")
      ),
      tabPanel(
        "% of change: lollipop plot",
        uiOutput("catch_indicators_lollipop"),
        withSpinner(plotlyOutput("Lollipop_plot", height = "100%", width = "100%")),
        htmlOutput("lollipop_plot_disclaimer")
        # )
      )
    )
  )
}


#' Creates the UI element of right panel of the Advice tab, which includes 
#' the headline of advice, catch scenario table and its footnotes.
#' 
#' @return UI element
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
catch_scenarios_right_panel <- function() {
  sidebarPanel(
    width = 6,
    style = "overflow-x: auto;",
    # withSpinner(DTOutput("table", height = "100%", width = "100%")),
    HTML("<b><font size= 6> Catch scenario table</b></font></br><font size= 4> The basis of the advice is indicated in bold. </font><br/><br/>"),
    withSpinner(reactableOutput("table")),
    htmlOutput("footnotes", height = "100%", width = "100%")
  )
}


#' Creates the UI element of the header, showing 
#' stock info and the headline of the advice.
#'
#' @param info_id 
#' @param headline_id 
#'
#' @return
#' @export
#'
#' @examples
header_info_and_headline <- function(info_id, headline_id) {
  mainPanel(width = 12,
            fluidRow(
              column(5,
                     wellPanel(withSpinner(htmlOutput(info_id, height = "100%", width = "100%")))
              ),
              column(7, 
                     wellPanel(withSpinner(htmlOutput(headline_id, height = "100%", width = "100%"))) 
              )
            )
  )
}



