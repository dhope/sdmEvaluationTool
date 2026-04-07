devtools::load_all()
sdmevaltool_options(
  base = "c:/Users/HopeD/Documents/TMP_A3_SHINY_DEP/ROF_results/"
)
# start the app
sdm_tool(
  user = "rw",
  # tabs = c(
  #   "overview",
  #   "predictions",
  #   "observations",
  #   "predictors"
  # ),
  options = list(host = "0.0.0.0", port = 8080)
)
