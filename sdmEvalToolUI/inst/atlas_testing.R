devtools::load_all()
devtools::load_all("../sdmEvalToolUI/")

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
  options = list(host = "0.0.0.0", port = 8080)
)


devtools::load_all()
sdmevaltool_options(
  base = "c:/Users/HopeD/Documents/TMP_A3_SHINY_DEP/PACKAGED_PROJECTS/OntarioAtlasReview/OntarioBreedingBirdAtlas/",
  conf = "../../../config.yml"
)
# start the app
sdm_tool(
  user = "test_user",
  options = list(host = "0.0.0.0", port = 8080)
)
"c:/Users/HopeD/Desktop/spatial_prediction.tif" |> terra::rast()
"c:/Users/HopeD/Documents/TMP_A3_SHINY_DEP/RoF_Baseline/materials/Bayesian/species/Hudsonian_Godwit/spatial_prediction.tif" |>
  terra::rast()
