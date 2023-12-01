
# utils -------------------------------------------------------------------

check_interreg_iotable <- function(data) {
  interreg_iotable <- data$interreg_iotable
  total_output <- data$total_output
  total_input <- data$total_input
  
  stopifnot(
    check_total_output = interreg_iotable |> 
      summarise(across(value, sum),
                .by = starts_with("input")) |>
      left_join(total_output, 
                by = join_by(input_region, input_type, input_name)) |> 
      filter(is.na(value) | is.na(total_output) | !near(value, total_output)) |> 
      vec_is_empty(),
    check_total_input = interreg_iotable |> 
      summarise(across(value, sum),
                .by = starts_with("output")) |>
      left_join(total_input, 
                by = join_by(output_region, output_type, output_name)) |> 
      filter(is.na(value) | is.na(total_input) | !near(value, total_input)) |> 
      vec_is_empty()
  )
  interreg_iotable
}
