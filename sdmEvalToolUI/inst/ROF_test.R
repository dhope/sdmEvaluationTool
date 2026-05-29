devtools::load_all()
sdmevaltool_options(
  base = "c:/Users/HopeD/Documents/TMP_A3_SHINY_DEP/_RoF_Baseline/",
  conf = "config.yml"
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
    "static",
    "summary"
  ),
  options = list(host = "0.0.0.0", port = 7405)
)
