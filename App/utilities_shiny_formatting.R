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



#' Build a generic action link with optional tooltip and icon
#'
#' Create a reusable UI element for either an external hyperlink or a Shiny
#' download link. The element can optionally include a tooltip via the
#' \code{hovertext} CSS class and a Font Awesome icon rendered with
#' \code{shiny::icon()}.
#'
#' @param text Character string used as the visible label.
#' @param hover_text Optional character string used as tooltip text in the
#'   \code{data-hover} attribute. If \code{NULL} or empty, no tooltip wrapper
#'   is added.
#' @param icon Optional character string giving the Font Awesome icon name
#'   passed to \code{shiny::icon()} (without the \code{fa-} prefix).
#' @param size Character string giving the CSS font size (e.g. \code{"14px"}).
#' @param href Optional character string for an external link target.
#' @param outputId Optional character string for a Shiny \code{downloadLink()}.
#' @param target Character string passed to \code{tags$a()} for external links.
#'   Defaults to \code{"_blank"}.
#' @param rel Character string passed to \code{tags$a()} for external links.
#'   Defaults to \code{"noopener"}.
#'
#' @return A Shiny tag object.
#'
#' @details
#' Exactly one of \code{href} or \code{outputId} must be supplied. If
#' \code{href} is supplied, the function returns an external link created with
#' \code{shiny::tags$a()}. If \code{outputId} is supplied, it returns a
#' \code{shiny::downloadLink()}.
#'
#' @examples
#' \dontrun{
#' icon_action_link(
#'   text = "Open report",
#'   hover_text = "Open report in a new tab",
#'   icon = "up-right-from-square",
#'   href = "https://example.org/report"
#' )
#'
#' icon_action_link(
#'   text = "Download data",
#'   hover_text = "Download CSV file",
#'   icon = "cloud-arrow-down",
#'   outputId = "download_data"
#' )
#' }
#'
#' @importFrom shiny tags tagList icon downloadLink
#' @noRd
icon_action_link <- function(text,
                             hover_text = NULL,
                             icon = NULL,
                             size = "14px",
                             href = NULL,
                             outputId = NULL,
                             target = "_blank",
                             rel = "noopener") {

  if (is.null(href) == is.null(outputId)) {
    stop("Provide exactly one of `href` or `outputId`.")
  }

  label_tag <- shiny::tags$span(
    style = paste0("font-size:", size, ";"),
    text,
    if (!is.null(icon)) shiny::tagList(" ", shiny::icon(icon))
  )

  link_tag <- if (!is.null(href)) {
    shiny::tags$a(
      href = href,
      target = target,
      rel = rel,
      label_tag
    )
  } else {
    shiny::downloadLink(
      outputId = outputId,
      label = label_tag
    )
  }

  if (!is.null(hover_text) && nzchar(hover_text)) {
    shiny::tags$span(
      class = "hovertext",
      `data-hover` = hover_text,
      link_tag
    )
  } else {
    link_tag
  }
}

#' Build a download link label with icon and optional tooltip
#'
#' Thin wrapper around \code{icon_action_link()} for download links.
#'
#' @param text Character string used as the visible label.
#' @param outputId Character string passed to \code{downloadLink()}.
#' @param hover_text Optional tooltip text.
#' @param size Character string giving the CSS font size.
#'
#' @return A Shiny tag object.
#'
#' @examples
#' \dontrun{
#' download_icon_label(
#'   text = "Download data",
#'   outputId = "download_data",
#'   hover_text = "Download CSV file"
#' )
#' }
#'
#' @noRd
download_icon_label <- function(text = "Download data",
                                outputId,
                                hover_text = NULL,
                                size = "14px") {
  icon_action_link(
    text = text,
    hover_text = hover_text,
    icon = "cloud-arrow-down",
    size = size,
    outputId = outputId
  )
}

#' Build an external link with icon and optional tooltip
#'
#' Thin wrapper around \code{icon_action_link()} for external links.
#'
#' @param text Character string used as the visible label.
#' @param href Character string giving the external URL.
#' @param hover_text Optional tooltip text.
#' @param size Character string giving the CSS font size.
#' @param target Character string passed to \code{tags$a()}.
#' @param rel Character string passed to \code{tags$a()}.
#'
#' @return A Shiny tag object.
#'
#' @examples
#' \dontrun{
#' external_icon_link(
#'   text = "Open application",
#'   href = "https://example.org",
#'   hover_text = "Open in a new tab"
#' )
#' }
#'
#' @noRd
external_icon_link <- function(text,
                               href,
                               hover_text = NULL,
                               size = "14px",
                               target = "_blank",
                               rel = "noopener") {
  icon_action_link(
    text = text,
    hover_text = hover_text,
    icon = "up-right-from-square",
    size = size,
    href = href,
    target = target,
    rel = rel
  )
}