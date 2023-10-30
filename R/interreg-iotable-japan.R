
# interreg-iotable-japan --------------------------------------------------

read_interreg_iotable_japan <- function(file) {
  col_names <- read_excel(file,
                          skip = 2, 
                          n_max = 4,
                          col_names = FALSE,
                          col_types = "text",
                          .name_repair = "minimal") |> 
    t() |> 
    as_tibble(.name_repair = ~c("col_name_1", "col_name_2", "col_name_3", "col_name_4")) |> 
    slice(-(1:4)) |> 
    unite("col_name", starts_with("col_name")) |> 
    pull()
  
  interreg_iotable <- read_excel(file,
                                 skip = 6,
                                 col_names = c("input_region_code", "input_region_name", "input_code", "input_name", col_names),
                                 col_types = "text") |> 
    pivot_longer(!starts_with(c("region", "input")),
                 names_to = c("output_region_code", "output_region_name", "output_code", "output_name"),
                 names_sep = "_",
                 values_transform = list(value = partial(parse_number, 
                                                         na = c("***", "aaa")))) |> 
    mutate(across(c(input_region_code, output_region_code),
                  \(x) str_pad(x, 2, 
                               pad = "0"))) |> 
    unite("input_region", starts_with("input_region")) |>
    unite("input_name", input_code, input_name) |> 
    unite("output_region", starts_with("output_region")) |>
    unite("output_name", output_code, output_name)

  pattern_industry_total <- "内生部門計$"
  pattern_valueadded_total <- "粗?付加価値部門計$"
  pattern_finaldemand_total <- "最終需要(部門)?計$"
  pattern_total <- "地?域内生産額$"
  pattern_export <- "(輸出|調整項)$"
  pattern_import <- "（控除）(輸入|関税|輸入品商品税)$"
  pattern_region_total <- "地域計$"
  pattern_subtotal <- "計$"
  
  input_name <- interreg_iotable  |> 
    distinct(input_name) |> 
    mutate(input_type = case_when(str_detect(input_name, pattern_industry_total) ~ "industry",
                                  str_detect(input_name, pattern_valueadded_total) ~ "valueadded")) |> 
    fill(input_type, 
         .direction = "up") |> 
    mutate(input_type = case_when(str_detect(input_name, pattern_industry_total) ~ "industry_total",
                                  str_detect(input_name, pattern_valueadded_total) ~ "valueadded_total",
                                  str_detect(input_name, pattern_total) ~ "total",
                                  .default = input_type) |> 
             as_factor())
  
  output_name <- interreg_iotable  |> 
    distinct(output_name) |> 
    mutate(output_type = case_when(str_detect(output_name, pattern_industry_total) ~ "industry",
                                   str_detect(output_name, pattern_finaldemand_total) ~ "finaldemand")) |> 
    fill(output_type,
         .direction = "up") |> 
    mutate(output_type = case_when(str_detect(output_name, pattern_industry_total) ~ "industry_total",
                                   str_detect(output_name, pattern_finaldemand_total) ~ "finaldemand_total",
                                   str_detect(output_name, pattern_export) ~ "export",
                                   str_detect(output_name, pattern_import) ~ "import",
                                   str_detect(output_name, pattern_total) ~ "total",
                                   .default = output_type) |> 
             as_factor())
  
  interreg_iotable <- interreg_iotable |> 
    left_join(input_name, 
              by = join_by(input_name)) |> 
    left_join(output_name, 
              by = join_by(output_name)) |> 
    relocate(input_region, input_type, input_name, 
             output_region, output_type, output_name)
  
  # Total check
  totaloutput <- interreg_iotable |> 
    filter(str_detect(output_region, pattern_region_total),
           output_type == "total") |> 
    select(!c(output_region, output_type, output_name)) |> 
    rename(totaloutput = value)
  
  totalinput <- interreg_iotable |>
    filter(str_detect(input_region, pattern_region_total),
           input_type == "total") |> 
    select(!c(input_region, input_type, input_name)) |> 
    rename(totalinput = value)
  
  interreg_iotable <- interreg_iotable |>
    filter(if_all(c(input_region, output_region),
                  \(x) str_detect(x, pattern_region_total,
                                  negate = TRUE)),
           input_type %in% c("industry", "valueadded"),
           output_type %in% c("industry", "finaldemand", "export", "import"),
           if_all(c(input_name, output_name),
                  \(x) str_detect(x, pattern_subtotal,
                                  negate = TRUE)))
  
  list(interreg_iotable = interreg_iotable,
       totaloutput = totaloutput,
       totalinput = totalinput)
}

check_interreg_iotable_japan <- function(data) {
  interreg_iotable <- data$interreg_iotable
  totaloutput <- data$totaloutput
  totalinput <- data$totalinput
  
  stopifnot(
    interreg_iotable |> 
      summarise(across(value, sum),
                .by = starts_with("input")) |>
      left_join(totaloutput, 
                by = join_by(input_region, input_type, input_name)) |> 
      filter(is.na(value) | is.na(totaloutput) | !near(value, totaloutput)) |> 
      vec_is_empty(),
    interreg_iotable |> 
      summarise(across(value, sum),
                .by = starts_with("output")) |>
      left_join(totalinput, 
                by = join_by(output_region, output_type, output_name)) |> 
      filter(is.na(value) | is.na(totalinput) | !near(value, totalinput)) |> 
      vec_is_empty()
  )
  interreg_iotable
}

get_interreg_iotable_japan <- function(file_interreg_iotable_japan) {
  tibble(file = file_interreg_iotable_japan,
         type = str_c("japan",
                      file |> 
                        path_file() |> 
                        path_ext_remove(),
                      sep = "-")) |> 
    mutate(across(type,
                  as_factor),
           data = file |> 
             map(\(file) {
               read_interreg_iotable_japan(file) |> 
                 check_interreg_iotable_japan()
             },
             .progress = TRUE),
           .keep = "unused") |> 
    unnest(data)
}
