#' Test the Template Component
#'
#' @param ... Arguments passed to other functions.
#'
#' @returns A Shiny app object
#'
#' @export
#' @examplesIf have_data()
#' test_comp_static()

test_comp_static <- function(...) {
  test_comp("mod_comp_static", use = c("model_id", "species_id"), ...)
}


#' Template Component UI
#'
#' @param id Shiny module ID
#' @param header Header
#'
#' @returns Shiny UI
#'
#' @export
#' @examples
#' mod_comp_static_ui()

mod_comp_static_ui <- function(
  id = "comp_static",
  header = "Additional Figures and Tables"
) {
  # Compare to mod_comp_template_spatial_ui, don't set card_body(class = "p-0"...)
  # because we want some padding around table contents.

  sdm_card(
    class = "sub-card",
    uiOutput(NS(id, "figure")),
    reactable::reactableOutput(NS(id, "table"))
  )
}

#' Template component Server
#'
#' @param id Module ID
#' @param model_id Model ID
#' @param species_id Species ID
#'
#' @returns Module server function
#'
#' @export

mod_comp_static_server <- function(
  id = "comp_static",
  model_id,
  species_id
) {
  # NOTE: No deployment id required because materials only associated
  #   with model and species

  moduleServer(id, function(input, output, session) {
    figures <- reactive(figures_prep(model_id(), species_id()))
    tables <- reactive(waic_table_prep(model_id(), species_id()))
    output$figure_legend <- renderUI(tt_material_settings(figures()))
    # output$template <- reactable::renderReactable(template_table(template()))

    output$figure <- renderUI({
      #reactable::renderReactable({
      req(figures())

      tagList(
        lapply(figures(), function(path) {
          tags$img(
            src = path,
            class = "gallery-img"
          )
        })
      )
    })

    output$table <- reactable::renderReactable(waic_table(tables()))
  })
}


#' Create a Table for Template Data
#'
#' @param template Data frame with template information
#'
#' @returns Reactable table
#'
#' @export
#' @examplesIf have_data()
#' waic_table_prep(model_id = "Bayesian", species_id = "Palm_Warbler")|>
#'   waic_table()

waic_table <- function(waic_tab) {
  names_ <- names(waic_tab)
  names_ <- names_[names_ != "Model"]
  cols_ <- vector(length = length(names_), mode = 'list')
  names(cols_) <- names_
  cols_ <- purrr::map(
    cols_,
    ~ reactable::colDef(format = reactable::colFormat(digits = 3))
  )
  reactable::reactable(
    waic_tab,
    defaultPageSize = nrow(waic_tab),
    minRows = nrow(waic_tab),
    columns = cols_,
    searchable = TRUE
  )
}

#' Prepare Template Data
#'
#' @param model_id Character. Model ID
#' @param species_id Character. Species ID
#'
#' @returns Data frame
#'
#' @export
#' @examplesIf have_data()
#' waic_table_prep(model_ = "Bayesian", species_id = "Palm_Warbler")

waic_table_prep <- function(model_id, species_id) {
  # TEMPLATE: Normally would use `prep_materials` function to prepare the
  # component materials, see the following example for the "model_fit"
  # component:

  # prep_materials("model_fit", model_id = model_id, species_id = species_id)

  # TEMPLATE: For this example, we'll use dummy data
  wai_t <- make_target_path(
    "materials/{model_id}/species/waic_res.parquet",
    data = list(model_id = model_id)
  )
  if (file.exists(wai_t)) {
    waic_table <- read_file(wai_t) |>
      dplyr::filter(.data$species == species_id) |>
      dplyr::select(-species)
  } else {
    waic_table <- dplyr::tibble(
      Model = NA_character_,
      WAIC = NA_real_,
      Delta.WAIC = NA_real_
    )
  }
  # The output object returned by `prep_materials` contains an attribute
  # "material_settings" that has the legend
  attr(waic_table, "material_settings") <- list(
    legend = list(en = "Table of WAIC results", fr = "")
  )
  waic_table
}

#' Prepare Template Data
#'
#' @param model_id Character. Model ID
#' @param species_id Character. Species ID
#'
#' @returns Data frame
#'
#' @export
#' @examplesIf have_data()
#' figures_prep(model_id = "bam_v5_can71", species_id = "BBWO")
#'

figures_prep <- function(model_id, species_id) {
  p_ <- make_target_path(
    "materials/{model_id}/species/{species_id}",
    list(model_id = model_id, species_id = species_id)
  )

  f_ <- list.files(
    make_target_path(
      "materials/{model_id}/species/{species_id}",
      list(model_id = model_id, species_id = species_id)
    ),
    ".jpeg",
    full.names = F
  )

  addResourcePath(
    prefix = glue::glue("{model_id}-{species_id}-plots"),
    directoryPath = make_target_path(
      "materials/{model_id}/species/{species_id}",
      list(model_id = model_id, species_id = species_id)
    )
  )

  out <- glue::glue(
    "{model_id}-{species_id}-plots/{f_}"
  )

  # The output object returned by `prep_materials` contains an attribute
  # "material_settings" that has the legend
  attr(out, "material_settings") <- list(
    legend = list(en = "Static figures", fr = "")
  )
  out
}
