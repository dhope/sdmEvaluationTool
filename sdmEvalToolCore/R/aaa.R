# Create internal environment
.sdmeval <- rlang::new_environment(parent = rlang::empty_env())
.sdmeval$show_popup <- TRUE
