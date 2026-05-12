devtools::load_all()
sdmevaltool_options(
  base = "c:/Users/HopeD/Documents/TMP_A3_SHINY_DEP/_RoF_Baseline/",
  conf = "../config.yml"
  #    base = "c:/Users/HopeD/Documents/sdm_evaluation_results/"
)
# start the app
sdm_tool(
  user = "dhope",
  tabs = c(
    "overview",
    "predictions",
    "observations",
    "model",
    "predictors",
    # "model_metadata",
    "summary",
    "static"
  ),
  options = list(host = "0.0.0.0", port = 8080)
)
arrow::write_parquet(
  mtcars,
  make_target_path(
    "materials/Bayesian/species/Palm_Warbler/dummy_table.parquet"
  )
)
