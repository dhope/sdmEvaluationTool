devtools::load_all()

sdmevaltool_options(
  base = "c:/Users/HopeD/Documents/sdm_evaluation_results/"
)
# start the app
sdm_tool(
  user = "test_user",
  tabs = c(
    "overview",
    "predictions",
    "observations",
    "predictors",
    "static"
  ),
  options = list(host = "0.0.0.0", port = 7405)
)


devtools::load_all()
sdmevaltool_options(
  base = "c:/Users/HopeD/Documents/TMP_A3_SHINY_DEP/ARCHIVE/OntarioBreedingBirdAtlas/",
  conf = "../../config.yml"
)
# start the app
sdm_tool(
  user = "test_user",
  options = list(host = "0.0.0.0", port = 7428)
)
