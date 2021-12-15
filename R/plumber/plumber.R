library(plumber)
library(pins)
library(recipes)
library(rapidoc)
library(dplyr)

# Read in model
model_board <- board_rsconnect()

model <- model_board |> 
  pin_read("james/simple-glm-fit")

# Read in indication data
indication <- readr::read_csv("indication.csv")

# Helper functions
to_dbl <- function(dollar_string) {
  stringr::str_replace_all(dollar_string, "\\$|,", "") |> 
    as.numeric()
}

#* @apiTitle GLM Model
#* @apiDescription Simple GLM model deployment example

#* @get /health-check
function() {
  list(
    status = "Good",
    timestamp = Sys.time()
  )
}

#* Generate predicted outcomes
#* @post /predict
function(req, res) {
  # browser()
  new_data <- req$body |> 
    as_tibble() |> 
    mutate(
      n_power_units = exposure,
    ) |>
    (function(data) bake(model$recipe, data))() |> 
    mutate(
      incurred_loss_and_alae = to_dbl(req$body$incurred_loss_and_alae),
      manual_premium = to_dbl(req$body$manual_premium),
      exposure = n_power_units,
      coverage_type = req$body$coverage_type
    ) |> 
    left_join(indication, by = c("coverage_type" = "coverage", "risk_state"))
  
  out_data <- bind_cols(new_data, .pred = predict(model$model, new_data)) |> 
    mutate(
      experience_mod = case_when(
        manual_premium < 10000 ~ 0.8,
        manual_premium < 10000 & incurred_loss_and_alae > 20000 ~ 2,
        manual_premium >= 10000 ~ 1,
        manual_premium >= 10000 & incurred_loss_and_alae > 1000000 ~ 1.5
      ),
      clrt_mod = case_when(
        exposure < 100 ~ 0.8,
        exposure < 100 & incurred_loss_and_alae > 1000000 ~ 1.8,
        exposure < 100 & incurred_loss_and_alae > 500000 ~ 1.4,
        exposure < 100 & incurred_loss_and_alae > 250000 ~ 1.0,
        exposure < 100 & incurred_loss_and_alae > 100000 ~ 0.9,
        exposure >= 100 ~ 0.75,
        exposure >= 100 & incurred_loss_and_alae > 1000000 ~ 2.0,
        exposure >= 100 & incurred_loss_and_alae > 500000 ~ 1.5,
        exposure >= 100 & incurred_loss_and_alae > 250000 ~ 1.0,
        exposure >= 100 & incurred_loss_and_alae > 100000 ~ 0.85
      ),
      lrt = clrt_mod * experience_mod * manual_premium,
      dpg = .pred * experience_mod * manual_premium * (1 - state_sold_indication),
      .out = case_when(
        manual_premium > 500000 ~ .25 * lrt + (.75 * dpg),
        TRUE ~ dpg
      )
    )
  
  out_data |> 
    select(.out) |> 
    unlist()
}

#* @plumber
function(pr) {
  pr |> 
    pr_set_api_spec("openapi.yaml") |>
    pr_set_docs("rapidoc")
}