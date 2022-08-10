
create_plots <-function() {

  library(tidyverse)
  library(plotly)
  library(ggplot2)

  file_loc = "C:/Users/ravij/Dropbox/Academic/Research/Projects/ABM_HIV/results/"
  file_name = "SIM_diag_time_7_26_22.csv"
  sim_diag_times <- read_csv(paste(file_loc, file_name, sep = ""))

  #Data from:
  #https://ahead.hiv.gov/locations/san-diego-county
  #https://aidsvu.org/local-data/united-states/west/california/san-diego-san-diego-county/#new-hiv-diagnoses
  #https://www.sandiegocounty.gov/content/dam/sdc/hhsa/programs/phs/documents/Monthly_CD_Report_September2019.pdf

  CSD_diag_per_year <- tibble(Year = c(2012:2029),
                              `New Diagnoses` = c(460, 462, 482, 483, 496, 414, 383, 383, 296, rep(NA, 9)),
                              Scenario = "Reported",
                              Prediction_type = "Preliminary")
  time_plot(sim_diag_times, CSD_diag_per_year)
}

time_plot <- function(time_dat.df) {


  # Make helper function for plotly settings
  plotly_thm <- . %>%
    config(modeBarButtonsToRemove = c('sendDataToCloud',
                                      'zoom2d',
                                      'pan2d',
                                      'select2d',
                                      'lasso2d',
                                      'zoomIn2d',
                                      'zoomOut2d',
                                      'autoScale2d',
                                      'resetScale2d',
                                      'hoverClosestCartesian',
                                      'hoverCompareCartesian',
                                      'toggleSpikelines'),
           displaylogo = F) %>%
    layout(xaxis=list(fixedrange=TRUE))

  ggplot_thm <- theme_bw() +
    theme(text=element_text(family="Zilla Slab"),
          legend.position = "bottom",
          axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold"),
          strip.text.x = element_text(size = 16, colour = "black", angle = 0),
          legend.text=element_text(size=16))

  graph_colors <- c('#0B2949', '#F1B51C','#046B5C','#D02B27', '#17A673', '#5B6771','#E0D4B5',  '#189394', '#7fa29a', '#72748b', '#eee7d6', '#e39378', '#9fa2a9', '#96bec1', '#9ccab0')

  make_count_pretty_whole <- function(val){
    return(formatC(val, digits = 0, format = 'f', big.mark = ','))
  }


  #################################################

  time_dat.df = sim_diag_times

  time_dat_plot.df = time_dat.df %>%
    mutate(Year = trunc(month/12) + 2012) %>%
      group_by(Year) %>%
      summarise(`New Diagnoses` = n()) %>%
      ungroup() %>%
    mutate(Scenario = "Preliminary") %>%
    mutate(Prediction_type = case_when(Year <= 2020 ~ "Preliminary",
                                Year > 2020 ~ "Prediction"))

  time_dat_plot.df = bind_rows(time_dat_plot.df,
                               time_dat_plot.df %>%
                                 filter(Year == 2020) %>%
                                 mutate(Prediction_type = "Prediction"))

  time_dat_plot.df = bind_rows(time_dat_plot.df, CSD_diag_per_year)

  time_dat_plot.df = time_dat_plot.df %>%
    mutate(Prediction_type = as.factor(Prediction_type))

  time_dat_plot.df$Prediction_type <- factor(time_dat_plot.df$Prediction_type, levels=c('Preliminary', 'Prediction'))

  time_fig <- time_dat_plot.df %>%
    ggplot(aes(x = Year, y = `New Diagnoses`, group = 1, text = paste0("Year: ", Year, "\nNew Diagnoses: ", make_count_pretty_whole(`New Diagnoses`)))) +
    geom_line(aes(col = Scenario,
                  linetype = Prediction_type)) +
    scale_color_manual(values = graph_colors) +
    scale_x_continuous(limits = c(2012, 2029)) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    guides(col=guide_legend("Scenario"),
           linetype = guide_legend("")) +
    labs(y = 'Number of New HIV Diagnoses') +
    ggplot_thm

  ggplotly(time_fig, tooltip = "text") %>%
    plotly_thm %>%
    layout(yaxis=list(fixedrange=TRUE))



}
