
discount_module <- function(simData, discount) {
  #Apply discounting as needed.
  discountMonth <- (1+discount)^(1/12)
  mutate_at(simData, vars(contains("cost"), "newdeaths", "newinfects", "ly", "qaly",
                 "pospopsize", "negpopsize", "popdenom"),
            list(~./(discountMonth^month))) %>%
  mutate(discounted = "Discounted") %>%
  select_order_output_data()
}
