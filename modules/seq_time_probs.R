
gen_seq_time_probs <- function(file_loc_input) {

seq_event1 = read_combine_format_demo_breakdowns(filepath = file_loc_input,
                                                 rwhap = 0,
                                                 sheet = "Sequence_Probabilities",
                                                 "probability",
                                                 "C8:H19")
seq_event1 = seq_event1 %>%
  mutate(timetype = "diagnosis") %>%
  rename(agerange = agegroup) %>%
  mutate(agerange = case_when(
    agerange == "youth" ~ "13-24",
    agerange == "adult" ~ "25-54",
    agerange == "olderadult" ~ "55-100")) %>%
  select(gender, risk, agerange, race, timetype, probability)

seq_event2 = read_combine_format_demo_breakdowns(filepath = file_loc_input,
                                                 rwhap = 0,
                                                 sheet = "Sequence_Probabilities",
                                                 "probability",
                                                 "L8:Q19")
seq_event2 = seq_event2 %>%
  mutate(timetype = "rebound") %>%
  rename(agerange = agegroup) %>%
  mutate(agerange = case_when(
    agerange == "youth" ~ "13-24",
    agerange == "adult" ~ "25-54",
    agerange == "olderadult" ~ "55-100")) %>%
  select(gender, risk, agerange, race, timetype, probability)

seq_event3 = read_combine_format_demo_breakdowns(filepath = file_loc_input,
                                                 rwhap = 0,
                                                 sheet = "Sequence_Probabilities",
                                                 "probability",
                                                 "U8:Z19")
seq_event3 = seq_event3 %>%
  mutate(timetype = "reengage") %>%
  rename(agerange = agegroup) %>%
  mutate(agerange = case_when(
    agerange == "youth" ~ "13-24",
    agerange == "adult" ~ "25-54",
    agerange == "olderadult" ~ "55-100")) %>%
  select(gender, risk, agerange, race, timetype, probability)

seq_event4 = read_combine_format_demo_breakdowns(filepath = file_loc_input,
                                                 rwhap = 0,
                                                 sheet = "Sequence_Probabilities",
                                                 "probability",
                                                 "C27:H38")
seq_event4 = seq_event4 %>%
  mutate(timetype = "initial") %>%
  rename(agerange = agegroup) %>%
  mutate(agerange = case_when(
    agerange == "youth" ~ "13-24",
    agerange == "adult" ~ "25-54",
    agerange == "olderadult" ~ "55-100")) %>%
  select(gender, risk, agerange, race, timetype, probability)

seq_events = bind_rows(
  bind_rows(
    bind_rows(seq_event1, seq_event2), seq_event3),
  seq_event4)

write_csv(seq_events,
          file = "../results/time_probs.csv",
          col_names = TRUE)
}
