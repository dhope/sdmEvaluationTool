#' Which Components Are Ready
#'
#' @param ready Character, component names that are ready.
#'
#' @return A data frame with components and a column telling which ones are ready.
#' The `"percent_ready"` attribute gives a percentage value of the
#' mandatory components that are ready.
#'
#' @examples
#' r <- get_comp_ready(c("observations", "model_metadata"))
#' r
#' attr(r, "percent_ready")
#'
#' @export
get_comp_ready <- function(ready = character(0L)) {
  out <- get_sdm_comp()[, c("component", "mandatory")]
  out$ready <- out$component %in% ready
  attr(out, "percent_ready") <- round(
    100 * sum(out$ready & out$mandatory) / sum(out$mandatory),
    1
  )
  out
}

#' Get default or custom configuration
#'
#'
#' @return A data frame with default components
get_sdm_comp <- function() {
  suppressWarnings({
    rlang::try_fetch(
      {
        cmp <- yaml::read_yaml(make_target_path(
          sdmevaltool_options()$conf
        ))$components
        components <- data.frame(
          component = character(0L),
          description = character(0L),
          mandatory = logical(0L),
          type = character(0L),
          path = character(0L),
          upload = character(0L),
          display = character(0L),
          evaluation = character(0L)
        )
        for (i in names(cmp)) {
          l <- cmp[[i]]
          c1 <- data.frame(
            component = i,
            description = l$description,
            mandatory = l$mandatory,
            type = l$type,
            path = l$upload$output$path,
            upload = NA_character_,
            display = NA_character_,
            evaluation = NA_character_
          )
          for (j in c("upload", "display", "evaluation")) {
            if (!is.null(l[[j]])) {
              c1[[j]] <- list(l[[j]])
            }
          }
          components <- rbind(components, c1)

          components
        }
      },
      error = function(cnd) sdmEvalToolCore::components
    )
  })
}

#' Get Rule for a Component
#'
#' @param component_id Character, component name (length 1).
#' @param rule_type CHaracter, the type of rule.
#'
#' @return A list with the rules, or `NULL`.
#'
#' @examples
#' str(get_comp_rule("observations", "upload"))
#' str(get_comp_rule("observations", "evaluation"))
#'
#' @export
get_comp_rule <- function(
  component_id,
  rule_type = c("upload", "display", "evaluation")
) {
  if (length(component_id) > 1L) {
    stop("component_id must have length of 1.")
  }
  rule_type <- match.arg(rule_type)

  cmp <- get_sdm_comp()

  rownames(cmp) <- cmp$component
  if (is.na(cmp[component_id, rule_type])) {
    NULL
  } else {
    cmp[component_id, rule_type][[1L]]
  }
}
