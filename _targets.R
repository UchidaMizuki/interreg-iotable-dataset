library(targets)

tar_option_set(
  packages = c("tidyverse", "fs", "readxl", "vctrs"),
  format = "qs"
)

tar_source()

list(
  # interreg-iotable-japan --------------------------------------------------
  tar_target(
    file_interreg_iotable_japan, 
    dir_ls("data-raw/interreg-iotable-japan", 
           regexp = "xlsx$"),
    format = "file"
  ),
  tar_target(
    interreg_iotable_japan,
    get_interreg_iotable_japan(file_interreg_iotable_japan = file_interreg_iotable_japan)
  )
)
