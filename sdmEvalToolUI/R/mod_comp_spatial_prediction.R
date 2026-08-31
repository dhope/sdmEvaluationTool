#' Test the Spatial Prediction Component
#'
#' @param ... Arguments passed to other functions.
#'
#' @returns A Shiny app object
#'
#' @export
#' @examplesIf have_data()
#' test_comp_spatial_prediction()

test_comp_spatial_prediction <- function(...) {
  test_comp("mod_comp_spatial_prediction", ...)
}

#' Spatial Prediction Component UI
#'
#' @param id Shiny module ID
#' @param header Header
#'
#' @returns Shiny UI
#'
#' @export
#' @examples
#' mod_comp_spatial_prediction_ui()

mod_comp_spatial_prediction_ui <- function(
  id = "comp_spatial_prediction",
  header = "Predictions"
) {
  layout_columns(
    gap = 0, # No gap between top and bottom
    col_widths = 12, # One column
    row_heights = c("60%", "40%"), # More map than selection area
    sdm_card(
      class = "sub-card",
      sdm_card_header(header, uiOutput(NS(id, "tooltip"))),
      card_body(
        class = "p-0",
        sdm_spinner(leaflet::leafletOutput(NS(id, "map")))
      ),
    ),
    sdm_card(
      class = "sub-card",
      card_body(mod_utils_map_selections_ui(NS(id, "select")))
    )
  )
}

#' Spatial Prediction component Server
#'
#' @param id Module ID
#' @param parent_id Parent tab ID (used to identify which tab is active)
#' @param deployment_id Deployment ID. Required for subunits.
#' @param model_id Model ID
#' @param species_id Species ID
#' @param spatial_selection Spatial selection. Required for spatial evaluations
#' @param spatial_ids Spatial IDs. Required for spatial evaluations.
#' @param map_views List. List of reactiveVals `active_tab`, `set_by` and
#'   `view` (list with zoom and lat/lon).
#'
#' @returns Module server function
#'
#' @export
mod_comp_spatial_prediction_server <- function(
  id = "comp_spatial_prediction",
  parent_id,
  deployment_id,
  model_id,
  species_id,
  spatial_selection,
  spatial_ids,
  map_views
) {
  stopifnot(is.reactive(deployment_id))
  stopifnot(is.reactive(model_id))
  stopifnot(is.reactive(species_id))
  purrr::walk(map_views, \(v) stopifnot(is.reactive(v)))

  moduleServer(id, function(input, output, session) {
    # Tooltip -------------------------------------------------------
    p <- reactive({
      prep_material_settings(
        "spatial_prediction",
        model_id(),
        species_id()
      )
    })
    output$tooltip <- renderUI({
      tt_material_settings(p())
    })
    # Map --------------------------------------------------------------------
    spatial_prediction <- reactive({
      spatial_prediction_prep(model_id(), species_id())
    })
    names_spatial_prediction <- reactive({
      if (is.null(p()$raster_labels)) {
        o <- names(spatial_prediction())
        names(o) <- o
      } else {
        if ("data.frame" %in% class(p()$raster_labels)) {
          p_ <- p()$raster_labels |>
            as.list() |>
            unlist()
        } else {
          p_ <- p()$raster_labels
        }
        o <- p_[
          p_ %in% names(spatial_prediction())
        ]
        # o <- names(o1)
        # names(o) <- o1
      }
      o
    })
    subunits <- reactive({
      deployment_subunits_prep(deployment_id())
    })

    output$map <- leaflet::renderLeaflet({
      validate_ids(
        deployment_id = deployment_id(),
        model_id = model_id(),
        species_id = species_id()
      )
      # spatial_prediction_map(
      #   spatial_prediction(),
      #   subunits(),
      #   ns = session$ns
      # )
      spatial_prediction_map_mod(
        spatial_prediction(),
        names_spatial_prediction(),
        subunits(),
        ns = session$ns
      ) |>
        set_view(map_views, tab = parent_id)
    }) |>
      bindEvent(spatial_prediction())

    # Synchronize map views ------------------------------------------------
    mod_utils_map_sync_server(
      "sync",
      parent_id,
      this_view = map_view(input, "map"),
      map_views,
      parent_session = session
    )

    # Process and show map selections ---------------------------------------
    interactions <- map_reactive_vals(input, "map")

    mod_utils_map_selections_server(
      "select",
      data = subunits,
      spatial_selection,
      interactions,
      parent_session = session
    )

    # Return ---------------------
    observe({
      spatial_ids(subunits()$id)
    })
  })
}


#' Create a Leaflet Map of Spatial Prediction Data
#'
#' @param spatial_prediction terra Raster. Spatial predictions
#' @param subunits Spatial Data frame. Deployment subunits.
#' @param ns Namespace.
#'
#' @returns A leaflet map object
#'
#' @export
#' @examplesIf have_data()
#' p <- spatial_prediction_prep(model_id = "bam_v5_can71", species_id = "BBWO")
#' s <- deployment_subunits_prep("deployment1")
#' spatial_prediction_map(p)
#' spatial_prediction_map(p, s)

spatial_prediction_map <- function(
  spatial_prediction,
  subunits = NULL,
  ns = identity
) {
  base_map(ns = ns) |>
    add_raster(
      spatial_prediction,
      layer = "mean",
      name = "Distribution",
      palette = "Spectral",
      min_0 = F
    ) |>
    add_raster(
      spatial_prediction,
      layer = "cv",
      name = "Uncertainty",
      palette = "viridis",
      min_0 = TRUE
    ) |>
    add_subunits(subunits) |>
    add_control(groups = c("Distribution", "Uncertainty"))
}

#' Create a Leaflet Map of Spatial Prediction Data
#'
#' @param spatial_prediction terra Raster. Spatial predictions
#' @param subunits Spatial Data frame. Deployment subunits.
#' @param ns Namespace.
#'
#' @returns A leaflet map object
#'
#' @export
#' @examplesIf have_data()
#' p <- spatial_prediction_prep(model_id = "Bayesian", species_id = "Alder_Flycatcher")
#' s <- deployment_subunits_prep("deployment1")
#' spatial_prediction_map_mod(p, "p_obs")
#' spatial_prediction_map_mod(p, s)

spatial_prediction_map_mod <- function(
  spatial_prediction,
  layers = NULL,
  subunits = NULL,
  ns = identity
) {
  map <- base_map(ns = ns) |>
    predictor_raster_layer(
      raster = spatial_prediction,
      layers = layers
    ) |>
    add_subunits(subunits)

  # Add in layer controls at map creation level because `add_control()` can't
  # use leafletProxy

  # Show selections for multiple layers only
  if (length(layers) > 0) {
    g <- layers
  } else {
    g <- character(0)
  }
  map <- add_control(map, groups = g)
  map
}


#' Prepare Spatial Prediction Data
#'
#' @param model_id Character. Model ID
#' @param species_id Character. Model ID
#'
#' @returns terra Raster
#'
#' @export
#' @examplesIf have_data()
#' spatial_prediction_prep(model_id = "bam_v5_can71", species_id = "BBWO")

spatial_prediction_prep <- function(model_id, species_id) {
  prep_materials(
    "spatial_prediction",
    model_id = model_id,
    species_id = species_id
  )
}
