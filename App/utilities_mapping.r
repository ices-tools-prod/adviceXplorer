#' Function to plot the intercative map of ecoregions.
#'
#' @param shape_eco (ecoregions' shapefile)
#' @param eu_shape (europe's shapefile)
#'
#' @return leaflet object
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' map_ecoregion(shape_eco, eu_shape)
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
#' 
# map_ecoregion <- function(shape_eco, eu_shape) {
#   minZoom <- 0.5
#   maxZoom <- 14
#   resolutions <- 1.8 * (2^(maxZoom:minZoom))
#   crs_laea <- leafletCRS(
#     crsClass = "L.Proj.CRS", code = "EPSG:3035",
#     proj4def = "+proj=laea +x_0=0 +y_0=0 +lon_0= -1.235660 +lat_0=60.346958",
#     resolutions = resolutions
#   )

#   leaflet(options = leafletOptions(crs = crs_laea, minZoom = 1, maxZoom = 2)) %>%
#     addPolygons(
#       data = eu_shape,
#       color = "black",
#       weight = 1,
#       fillOpacity = 1,
#       fillColor = "#99AABF", # "#E8EAEA"
#       group = "Europe",
#       options = pathOptions(clickable = FALSE)
#     ) %>%
#     addPolygons(
#       data = shape_eco,
#       fillColor = "#E6E7E8",
#       fillOpacity = 1,
#       color = "black",
#       stroke = TRUE,
#       weight = 1,
#       layerId = ~Ecoregion,
#       group = "Eco_regions",
#       label = ~Ecoregion
#     ) %>%
#     addPolygons(
#       data = shape_eco,
#       fillColor = "#F15D22",
#       fillOpacity = 1,
#       weight = 1,
#       color = "black",
#       stroke = TRUE,
#       layerId = ~OBJECTID,
#       group = ~Ecoregion
#     ) %>%
#     setView(lng = -1.235660, lat = 60.346958, zoom = 0.5) %>%
#     hideGroup(group = shape_eco$Ecoregion)
# }


# #' Server side functionality for the landing page map panel
# #'
# #' @param input 
# #' @param output 
# #' @param session 
# #'
# #' @return
# #' @export
# #'
# #' @examples
# map_panel_server <- function(input, output, session) {
  
#   # Render Map 1
#   output$map1 <- renderLeaflet({
#     map_ecoregion(shape_eco, eu_shape)
#   }) # END RENDER LEAFLET map1
  
#   proxy_1 <- leafletProxy("map1")
  
#   # create empty vector to hold all click ids
#   selected_1 <- reactiveValues(groups = vector())
  
#   # find index
#   observeEvent(input$map1_shape_click, {
    
#     ## calculate index of ecoregion selected in shape_eco
#     idx_1 <- match(input$map1_shape_click$id, shape_eco$Ecoregion)
    
#     if (input$map1_shape_click$group == "Eco_regions") {
#       selected_1$groups <- c(selected_1$groups, input$map1_shape_click$id) #
#       # print(selected_1$groups)
      
#       proxy_1 %>%
#         showGroup(group = input$map1_shape_click$id) #%>% 
#         #hideGroup(group = setdiff(selected_1$groups, input$map1_shape_click$id))
#     } else {
#       selected_1$groups <- setdiff(selected_1$groups, input$map1_shape_click$group)
#       proxy_1 %>%
#         hideGroup(group = input$map1_shape_click$group) #%>%
            
#     }

#     updateVirtualSelect(inputId = "selected_locations",
#                          label = "ICES Ecoregions:",
#                          choices = shape_eco$Ecoregion,
#                         #  selected = input$map1_shape_click$id,
#                          selected = selected_1$groups,
#                          session = session
#                          )
        
#   })
  
#   observeEvent(input$selected_locations,
#                {
#                  removed_via_selectInput <- setdiff(selected_1$groups, input$selected_locations)
#                  added_via_selectInput <- setdiff(input$selected_locations, selected_1$groups)
                 
#                  if (length(removed_via_selectInput) > 0) {
#                    selected_1$groups <- input$selected_locations
                   
#                    proxy_1 %>% hideGroup(group = removed_via_selectInput)
#                  }
                 
#                  if (length(added_via_selectInput) > 0) {
#                    selected_1$groups <- input$selected_locations
                   
#                    proxy_1 %>% showGroup(group = added_via_selectInput)
#                  }
#                },
#                ignoreNULL = FALSE
#   )
# }
map_ecoregion <- function(shape_eco, eu_shape) {

  shape_eco <- shape_eco %>% dplyr::mutate(Ecoregion = as.character(Ecoregion))

  minZoom <- 0.5
  maxZoom <- 14
  resolutions <- 1.8 * (2^(maxZoom:minZoom))
  crs_laea <- leafletCRS(
    crsClass = "L.Proj.CRS", code = "EPSG:3035",
    proj4def = "+proj=laea +x_0=0 +y_0=0 +lon_0= -1.235660 +lat_0=60.346958",
    resolutions = resolutions
  )

  leaflet(options = leafletOptions(crs = crs_laea, minZoom = 1, maxZoom = 2)) %>%
    addPolygons(
      data = eu_shape,
      color = "black",
      weight = 1,
      fillOpacity = 1,
      fillColor = "#99AABF",
      group = "Europe",
      options = pathOptions(clickable = FALSE)
    ) %>%
    addPolygons(
      data = shape_eco,
      fillColor = "#E6E7E8",          # default grey (will be overridden via proxy redraw)
      fillOpacity = 1,
      color = "black",
      stroke = TRUE,
      weight = 1,
      layerId = ~Ecoregion,
      group = "Eco_regions",
      label = ~Ecoregion
    ) %>%
    setView(lng = -1.235660, lat = 60.346958, zoom = 0.5)
}
map_panel_server <- function(input, output, session) {

  # Render base map once
  output$map1 <- renderLeaflet({
    map_ecoregion(shape_eco, eu_shape)
  })

  proxy_1 <- leafletProxy("map1")

  # Keep selected ecoregions
  selected_1 <- reactiveValues(groups = character(0))

  # helper: redraw eco polygons with conditional fill
  redraw_ecoregions <- function(selected_groups) {
    sel <- as.character(selected_groups)
    shp <- shape_eco %>% dplyr::mutate(Ecoregion = as.character(Ecoregion))

    proxy_1 %>%
      clearGroup("Eco_regions") %>%   # removes the old eco polygons only
      addPolygons(
        data = shp,
        fillColor = ifelse(shp$Ecoregion %in% sel, "#F15D22", "#E6E7E8"),
        fillOpacity = 1,
        color = "black",
        stroke = TRUE,
        weight = 1,
        layerId = ~Ecoregion,
        group = "Eco_regions",
        label = ~Ecoregion
      )
  }

  observeEvent(input$map1_shape_click, {
    req(input$map1_shape_click$id)

    clicked <- as.character(input$map1_shape_click$id)

    if (clicked %in% selected_1$groups) {
      selected_1$groups <- setdiff(selected_1$groups, clicked)
    } else {
      selected_1$groups <- c(selected_1$groups, clicked)
    }

    redraw_ecoregions(selected_1$groups)

    updateVirtualSelect(
      inputId = "selected_locations",
      label = "ICES Ecoregions:",
      choices = as.character(shape_eco$Ecoregion),
      selected = selected_1$groups,
      session = session
    )
  })

  observeEvent(input$selected_locations, {
    selected_1$groups <- as.character(input$selected_locations %||% character(0))
    redraw_ecoregions(selected_1$groups)
  }, ignoreNULL = FALSE)
}
