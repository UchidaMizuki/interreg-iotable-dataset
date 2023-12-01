
# interreg-iotable-indonesia ----------------------------------------------

download_interreg_iotable_indonesia_2016 <- function() {
  exdir <- "data-raw/interreg-iotable-indonesia/2016"
  if (dir_exists(exdir)) dir_delete(exdir)
  dir_create(exdir)
  
  # https://www.bps.go.id/subject/105/input-output.html#subjekViewTab3
  
  # By 34 Province and 17 Industrial Origin
  file <- path(exdir, "34reg-17ind",
               ext = "xlsx")
  curl::curl_download("https://www.bps.go.id/statictable/download.html?nrbvfeve=MjEyNw%3D%3D&sdfs=sdngrbfjgrdejrh&zxcv=L3dlYnNpdGU%3D&xzmn=aHR0cHM6Ly93d3cuYnBzLmdvLmlkL3N0YXRpY3RhYmxlLzIwMjEvMDQvMzAvMjEyNy90YWJlbC1pbnRlci1yZWdpb25hbC1pbnB1dC1vdXRwdXQtaW5kb25lc2lhLXRyYW5zYWtzaS1kb21lc3Rpay1hdGFzLWRhc2FyLWhhcmdhLXByb2R1c2VuLW1lbnVydXQtMzQtcHJvdmluc2ktZGFuLTE3LWxhcGFuZ2FuLXVzYWhhLS0yMDE2LS1qdXRhLXJ1cGlhaC0uaHRtbA%3D%3D&twoadfnoarfeauf=MjAyMy0xMi0wMSAxNTo0Njo1Mg%3D%3D",
                      file)
  
  # By 34 Province and 52 Industry
  file <- path(exdir, "34reg-52ind",
               ext = "xlsx")
  curl::curl_download("https://www.bps.go.id/statictable/download.html?nrbvfeve=MjEyOA%3D%3D&sdfs=sdngrbfjgrdejrh&zxcv=L3dlYnNpdGU%3D&xzmn=aHR0cHM6Ly93d3cuYnBzLmdvLmlkL3N0YXRpY3RhYmxlLzIwMjEvMDQvMzAvMjEyOC90YWJlbC1pbnRlci1yZWdpb25hbC1pbnB1dC1vdXRwdXQtaW5kb25lc2lhLXRyYW5zYWtzaS1kb21lc3Rpay1hdGFzLWRhc2FyLWhhcmdhLXByb2R1c2VuLW1lbnVydXQtMzQtcHJvdmluc2ktZGFuLTUyLWluZHVzdHJpLS0yMDE2LS1qdXRhLXJ1cGlhaC0uaHRtbA%3D%3D&twoadfnoarfeauf=MjAyMy0xMi0wMSAxNTo0NzoyMw%3D%3D",
                      file)
  
  # By 6 Island Group and 17 Industrial Origin
  file <- path(exdir, "6reg-17ind",
               ext = "xlsx")
  curl::curl_download("https://www.bps.go.id/statictable/download.html?nrbvfeve=MjEyNA%3D%3D&sdfs=sdngrbfjgrdejrh&zxcv=L3dlYnNpdGU%3D&xzmn=aHR0cHM6Ly93d3cuYnBzLmdvLmlkL3N0YXRpY3RhYmxlLzIwMjEvMDQvMzAvMjEyNC90YWJlbC1pbnRlci1yZWdpb25hbC1pbnB1dC1vdXRwdXQtaW5kb25lc2lhLXRyYW5zYWtzaS1kb21lc3Rpay1hdGFzLWRhc2FyLWhhcmdhLXByb2R1c2VuLW1lbnVydXQtNi1rZWxvbXBvay1wdWxhdS1kYW4tMTctbGFwYW5nYW4tdXNhaGEtLTIwMTYtLWp1dGEtcnVwaWFoLS5odG1s&twoadfnoarfeauf=MjAyMy0xMi0wMSAxNTo0MzowNw%3D%3D",
                      file)
  
  # By 6 Island Group and 52 Industry
  file <- path(exdir, "6reg-52ind",
               ext = "xlsx")
  curl::curl_download("https://www.bps.go.id/statictable/download.html?nrbvfeve=MjEyNQ%3D%3D&sdfs=sdngrbfjgrdejrh&zxcv=L3dlYnNpdGU%3D&xzmn=aHR0cHM6Ly93d3cuYnBzLmdvLmlkL3N0YXRpY3RhYmxlLzIwMjEvMDQvMzAvMjEyNS90YWJlbC1pbnRlci1yZWdpb25hbC1pbnB1dC1vdXRwdXQtaW5kb25lc2lhLXRyYW5zYWtzaS1kb21lc3Rpay1hdGFzLWRhc2FyLWhhcmdhLXByb2R1c2VuLW1lbnVydXQtNi1rZWxvbXBvay1wdWxhdS1kYW4tNTItaW5kdXN0cmktLTIwMTYtLWp1dGEtcnVwaWFoLS5odG1s&twoadfnoarfeauf=MjAyMy0xMi0wMSAxNTo0Mzo1Nw%3D%3D",
                      file)
  
  dir_ls(exdir,
         regexp = "\\.xlsx$")
}


get_interreg_iotable_indonesia_2016 <- function(file_interreg_iotable_indonesia_2016) {
  tibble(file = file_interreg_iotable_indonesia_2016,
         type = str_c("indonesia-2016",
                      file |> 
                        path_file() |> 
                        path_ext_remove(),
                      sep = "-") |> 
           as_factor()) |> 
    mutate(data = file |> 
             map(\(file) {
               read_interreg_iotable_indonesia_2016(file) |> 
                 check_interreg_iotable_indonesia_2016()
             },
             .progress = TRUE),
           .keep = "unused") |> 
    unnest(data)
}

read_interreg_iotable_indonesia_2016 <- function(file) {
  col_names <- read_excel(file,
                          skip = 1,
                          n_max = 5,
                          col_names = FALSE,
                          col_types = "text",
                          .name_repair = "minimal") |> 
    t() |> 
    as_tibble(.name_repair = ~str_c("col_name", 1:5,
                                    sep = "_")) |> 
    select(!c(col_name_2, col_name_3)) |> 
    fill(everything()) |> 
    unite("col_name", everything()) |> 
    pull(col_name)
  
  interreg_iotable <- read_excel(file,
                                 skip = 6,
                                 col_names = col_names,
                                 col_types = "text",
                                 .name_repair = "minimal")
  
  interreg_iotable <- interreg_iotable[vec_unique_loc(names(interreg_iotable))] |> 
    select(where(\(x) !is.na(first(x)))) |> 
    rename(input_region = Provinsi_NA_NA,
           input_name = Deskripsi_NA_NA,
           input_code = Kode_NA_NA) |> 
    fill(input_region) |> 
    mutate(across(!c(input_region, input_name, input_code),
                  parse_number)) |> 
    pivot_longer(!c(input_region, input_name, input_code),
                 names_to = c("output_region", "output_name", "output_code"),
                 names_sep = "_") |> 
    mutate(input_type = case_when(str_starts(input_code, "[:alpha:]") ~ "industry",
                                  input_code == "2000" ~ "import",
                                  input_code %in% c("2010", "2020", "2045") ~ "valueadded",
                                  input_code == "2100" ~ "total") |> 
             as_factor(),
           input_name = case_match(input_type,
                                   c("import", "valueadded") ~ input_region,
                                   .default = input_name),
           input_region = case_match(input_type,
                                     c("import", "valueadded") ~ NA_character_,
                                     .default = input_region),
           .after = input_region) |>
    drop_na(input_type) |> 
    mutate(output_type = case_when(str_starts(output_code, "[:alpha:]") ~ "industry",
                                   output_code %in% c("3011", "3012", "3020", "3030", "3041") ~ "finaldemand",
                                   output_code == "3080" ~ "export",
                                   output_code == "6000" ~ "total") |>
             as_factor(),
           output_region = case_match(output_type,
                                      c("export", "total") ~ NA_character_,
                                      .default = output_region),
           .after = output_region) |> 
    drop_na(output_type) |> 
    unite("input_name", input_code, input_name) |> 
    unite("output_name", output_code, output_name)
  
  total_output <- interreg_iotable |> 
    filter(input_type == "industry",
           output_type == "total") |> 
    summarise(total_output = sum(value),
              .by = starts_with("input"))
  
  total_input <- interreg_iotable |> 
    filter(input_type == "total",
           output_type == "industry") |> 
    summarise(total_input = sum(value),
              .by = starts_with("output"))
  
  interreg_iotable <- interreg_iotable |> 
    filter(if_all(c(input_region, output_region),
                  \(x) is.na(x) | str_starts(x, "\\d")),
           input_type != "total",
           output_type != "total")
  
  list(interreg_iotable = interreg_iotable,
       total_output = total_output,
       total_input = total_input)
}

check_interreg_iotable_indonesia_2016 <- function(data) {
  interreg_iotable <- data$interreg_iotable
  total_output <- data$total_output
  total_input <- data$total_input
  
  stopifnot(
    check_total_output = interreg_iotable |> 
      filter(input_type == "industry") |> 
      summarise(across(value, sum),
                .by = starts_with("input")) |>
      left_join(total_output, 
                by = join_by(input_region, input_type, input_name)) |> 
      filter(is.na(value) | is.na(total_output) | 
               !near(value, total_output, 
                     tol = 1)) |> 
      vec_is_empty(),
    check_total_input = interreg_iotable |> 
      filter(output_type == "industry") |>
      summarise(across(value, sum),
                .by = starts_with("output")) |>
      left_join(total_input, 
                by = join_by(output_region, output_type, output_name)) |> 
      filter(is.na(value) | is.na(total_input) | 
               !near(value, total_input,
                     tol = 1)) |> 
      vec_is_empty()
  )
  interreg_iotable
}
