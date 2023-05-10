
specific_month_prob <- function(month, df) {
  #Get month-specific probabilities as needed for those that can change over
  #time in the inputs across a user=specified time horizon.
  mutate(df,
         better = if_else(btime == 0 | (month > btime), bettere,
                          betters + (((bettere-betters)*month)/btime)),
         worse  = if_else(wtime == 0 | (month > wtime), worsee,
                          worses + (((worsee-worses)*month)/wtime))
  )
}
