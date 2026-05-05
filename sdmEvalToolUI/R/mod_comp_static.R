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
    uiOutput(NS(id, "table"))
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
    tables <- reactive(static_table_prep(model_id(), species_id()))
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

    output$table <-
      renderUI({
        # reactable::renderReactable({
        req(tables())
        purrr::imap(tables(), function(tab, idx) {
          #  purrr::map(static_tab_list, function(static_table) {
          names_ <- dplyr::select(tab, dplyr::where(is.numeric)) |>
            names()
          # names_ <- names_[names_ != "Model"]
          cols_ <- vector(length = length(names_), mode = 'list')
          names(cols_) <- names_
          cols_ <- purrr::map(
            cols_,
            ~ reactable::colDef(format = reactable::colFormat(digits = 3))
          )
          tagList(
            h3(paste("Table Number:", idx)),
            reactable::reactable(
              tab,
              defaultPageSize = nrow(tab),
              minRows = nrow(tab),
              columns = cols_,
              searchable = TRUE
            ),
            hr()
          )
        })
      })
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
#' static_table_prep(model_id = "Bayesian", species_id = "Palm_Warbler") |>
#'   static_table()
static_table <- function(static_tab_list) {
  purrr::map(static_tab_list, function(static_table) {
    names_ <- dplyr::select(static_table, dplyr::where(is.numeric)) |>
      names()
    # names_ <- names_[names_ != "Model"]
    cols_ <- vector(length = length(names_), mode = 'list')
    names(cols_) <- names_
    cols_ <- purrr::map(
      cols_,
      ~ reactable::colDef(format = reactable::colFormat(digits = 3))
    )
    reactable::reactable(
      static_table,
      defaultPageSize = nrow(static_table),
      minRows = nrow(static_table),
      columns = cols_,
      searchable = TRUE
    )
  })
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
#' static_table_prep(model_ = "Bayesian", species_id = "Palm_Warbler")

static_table_prep <- function(model_id, species_id) {
  # TEMPLATE: Normally would use `prep_materials` function to prepare the
  # component materials, see the following example for the "model_fit"
  # component:

  # prep_materials("model_fit", model_id = model_id, species_id = species_id)

  # TEMPLATE: For this example, we'll use dummy data
  spp_t <- list.files(
    make_target_path(
      "materials/{model_id}/species/",
      list(model_id = model_id)
    ),
    "table.parquet",
    full.names = T
  )
  table_files <- list.files(
    make_target_path(
      "materials/{model_id}/species/{species_id}",
      list(model_id = model_id, species_id = species_id)
    ),
    "table.parquet",
    full.names = T
  )
  process_tables <- function(table_file_v) {
    purrr::map(
      table_file_v,
      ~ {
        if (file.exists(.x)) {
          df <- read_file(.x)
          if ("species" %in% names(df)) {
            df <- df |>
              dplyr::filter(.data$species == species_id) |>
              dplyr::select(-species)
          }
          df
        }
      }
    )
  }

  # if (file.exists(wai_t)) {
  #   waic_table <- read_file(wai_t) |>
  #     dplyr::filter(.data$species == species_id) |>
  #     dplyr::select(-species)
  # } else {
  #   waic_table <- dplyr::tibble(
  #     Model = NA_character_,
  #     WAIC = NA_real_,
  #     Delta.WAIC = NA_real_
  #   )
  # }

  tables_static <- process_tables(c(spp_t, table_files))
  if (length(tables_static) > 0) {
    names(tables_static) <- glue::glue("table_{1:length(tables_static)}")
  }

  # The output object returned by `prep_materials` contains an attribute
  # "material_settings" that has the legend
  attr(tables_static, "material_settings") <- list(
    legend = list(en = "Extra static tables results", fr = "")
  )
  tables_static
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
