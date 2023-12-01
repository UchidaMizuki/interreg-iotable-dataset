library(targets)

tar_option_set(
  packages = c("tidyverse", "fs", "readxl", "vctrs", "rvest"),
  format = "qs"
)

tar_source()

list(
  # interreg-iotable-japan --------------------------------------------------
  # Inter-regional input-output tables for Japan (competitive import type)
  tar_target(
    file_interreg_iotable_japan,
    download_interreg_iotable_japan(),
    format = "file"
  ),
  tar_target(
    interreg_iotable_japan,
    get_interreg_iotable_japan(file_interreg_iotable_japan = file_interreg_iotable_japan)
  ),

  # interreg-iotable-indonesia ----------------------------------------------
  tar_target(
    file_interreg_iotable_indonesia_2016,
    download_interreg_iotable_indonesia_2016(),
    format = "file"
  ),
  tar_target(
    interreg_iotable_indonesia_2016,
    get_interreg_iotable_indonesia_2016(file_interreg_iotable_indonesia_2016 = file_interreg_iotable_indonesia_2016)
  )
)
