
inflate_module <- function(simData, inflation) {
  #Inflate variables as needed for output.
  inflateMonth  <- (1+inflation)^(1/12)
  simData %>%
    mutate_at(vars(contains("cost")), list(~.*(inflateMonth^month))) %>%
    mutate(discounted = "Non-discounted") %>%
    select_order_output_data()
}
