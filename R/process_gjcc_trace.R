#' @title Process GJCC trace
#'
#' @description Takes a gap junction measuring current clamp recording from `prep_abf()` and returns Coupling Coefficent, resistances, membrane time constant, and diagnostic plots. , and diagnostic plots.
#'
#' @param trace The prepared abf data.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples

process_gjcc_trace <- function(trace = trace_list$GJCC){

  ## prep ====
  temp <- trace

  temp_plt <- temp %>%
    # filter(Sweep == 4) %>%
    mutate(In4 = as.numeric(In4)) %>%
    mutate(In7 = as.numeric(In7))

  # Show what the protocol looks like
  plt1 <- downsample_data(temp_plt, len = 1000) %>%
    gather(key, value, c("In4", "In9")) %>%
    ggplot(aes(Time, value, color = key, group = interaction(Sweep, key)))+
    geom_path()+
    scale_color_manual(values = c("#F8766D", "#00BFC4"))

  plt2 <- downsample_data(temp_plt, len = 1000) %>%
    gather(key, value, c("In7", "In12")) %>%
    # mutate(key = factor(.$key, levels = c("In7", "In12"))) %>%
    ggplot(aes(Time, value, color = key, group = interaction(Sweep, key)))+
    geom_path()+
    scale_color_manual(values = c("#00BFC4", "#F8766D"))# Without flipping the factor level, get the right colors.

  plt_trace <- plt1/plt2


  # annotate
  temp <- temp %>%
    mutate(Segment = case_when(Time >= 0       & Time <= 0.32820 ~ "A", #"PreStepIn12",
                               Time >= 1.32836 & Time <= 1.82836 ~ "B", #"StepIn12",
                               Time >= 2.57744 & Time <= 3.07744 ~ "C", #"PreStepIn7",
                               Time >= 4.07760 & Time <= 4.57760 ~ "D", #"StepIn7"
    ))

  ## resistances/ coupling ====
  # $$r_{11}=\frac{v_1}{i_1}$$
  # $$r_{12}=\frac{v_2}{i_1}$$
  # $$r_1= \frac{r_{11}*r_{22} - r_{12}^2}{r_{22} - r_{12}}$$
  # $$r_c = \frac{r_{11}*r_{22} - r_{12}^2}{r_{12}}$$
  temp_resist <- temp %>%
    group_by(Sweep, Segment) %>%
    filter(!is.na(Segment)) %>%
    summarise(MeanIn4 = mean(In4, na.rm = T),
              MeanIn7 = mean(In7, na.rm = T),
              MeanIn9 = mean(In9, na.rm = T),
              MeanIn12 = mean(In12, na.rm = T)) %>%
    ungroup() %>%
    pivot_wider(names_from = Segment, values_from = c("MeanIn4", "MeanIn7", "MeanIn9", "MeanIn12")) %>%
    mutate(In4_CC = MeanIn9_D/MeanIn4_D, #inj in In4
           In4_R11 = MeanIn4_D/MeanIn7_D,
           In4_R12 = MeanIn9_D/MeanIn7_D,

           In9_CC = MeanIn4_B/MeanIn9_B, #inj in In9
           In9_R11 = MeanIn9_B/MeanIn12_B,
           In9_R12 = MeanIn4_B/MeanIn12_B
    ) %>%
    mutate(mean_R12 = (In4_R12+In9_R12)/2) %>%
    mutate(In4_R1 = ((In4_R11*In9_R11) - (mean_R12^2)) / (In9_R11-mean_R12),
           In4_Rc = ((In4_R11*In9_R11) - (mean_R12^2)) / (mean_R12),

           In9_R1 = ((In9_R11*In4_R11) - (mean_R12^2)) / (In4_R11-mean_R12),
           In9_Rc = ((In9_R11*In4_R11) - (mean_R12^2)) / (mean_R12)
    ) %>% select(starts_with("In"), "Sweep")

  ## V_rest ====
  temp_rmp <- temp %>%
    filter(Segment == "C") %>%
    group_by(Sweep) %>%
    summarise(In4_Vrest = min(In4, na.rm = T),
              In9_Vrest = min(In9, na.rm = T))


  ## Tau ====
  temp <- temp %>%
    mutate(Segment = case_when(Time >= 3.07744 & Time <= 4.47760 ~ "In4Step", #"PreStepIn7",
                               Time >= 0.32820 & Time <= 1.72836 ~ "In9Step", #"StepIn7"
    ))

  plt1 <-
    downsample_data(temp, len = 1000) %>%
    gather(Key, Value, c("In4", "In9")) %>%
    ggplot(aes(Time, Value, color = Segment, group = interaction(Key, Sweep)))+
    geom_path()

  plt2 <-
    downsample_data(temp, len = 1000) %>%
    ungroup() %>%
    gather(key, value, c("In7", "In12")) %>%
    mutate(key = factor(.$key, levels = c("In7", "In12"))) %>%
    ggplot(aes(Time, value, color = Segment, group = interaction(key, Sweep)))+
    geom_path()

  plt_segments <- plt1/plt2


  ### in4
  temp_in4 <- temp %>%
    filter(Segment == "In4Step") %>%
    mutate(Time = Time - min(Time, na.rm = T))

  test_fits <- map(unique(temp_in4$Sweep), function(i){
    temp_fit <- try(fit_tau_1term_exp(df = filter(temp_in4, Sweep == i),
                                      IV = "Time",
                                      DV = "In4"))
    if(is.list(temp_fit)){
      return(temp_fit)
    } else if (is.character(temp_fit)){
      warning("Error occured in fitting. Returning nothing.")
      return(list(fit = NA,
                  check_fit = NA))
    } else {
      warning("Unclear what error has occured in fitting tau for In9.")
      return()
    }
  })

  fits_in4 <- map(seq_along(transpose(test_fits)[[1]]), function(i){
    df <- transpose(test_fits)[[1]][i][[1]]
    if (is.na(df)){

    } else {
      df <- df[df$term == "b", c("estimate", "std.error")]
      df$Sweep <- i
      return(df)
    }
  }) %>%
    do.call(rbind, .) %>%
    rename(In4_Tau_est = estimate, In4_Tau_std.err = std.error) %>%
    mutate(Sweep = as.factor(Sweep))

  test_fits_plts <- map(seq_along(test_fits), function(i){
    if(!is.na(test_fits[[i]]$fit)){
    ggplot()+
      geom_line(data = as.data.frame(test_fits[[i]]$check_fit), aes(x = Time, y = Fit), color = "firebrick")+
      geom_line(data = as.data.frame(test_fits[[i]]$check_fit), aes(x = Time, y = In4), alpha = 0.8)+
      labs(subtitle = paste("tau =", as.character(
        round(test_fits[[i]]$fit[2, 2], digits = 3)
      )))
    }
  })

  plt_tau_in4 <- cowplot::plot_grid(plotlist = test_fits_plts)


  ### in9
  temp_in9 <- temp %>%
    filter(Segment == "In9Step") %>%
    mutate(Time = Time - min(Time, na.rm = T))

  test_fits <- map(unique(temp_in9$Sweep), function(i){
    temp_fit <- try(fit_tau_1term_exp(df = filter(temp_in9, Sweep == i),
                      IV = "Time",
                      DV = "In9"))
    if(is.list(temp_fit)){
      return(temp_fit)
    } else if (is.character(temp_fit)){
      warning("Error occured in fitting. Returning nothing.")
      return(list(fit = NA,
                  check_fit = NA))
    } else {
      warning("Unclear what error has occured in fitting tau for In9.")
      return()
    }

  })

  fits_in9 <- map(seq_along(transpose(test_fits)[[1]]), function(i){
    df <- transpose(test_fits)[[1]][i][[1]]
    if (is.na(df)){

    } else {
      df <- df[df$term == "b", c("estimate", "std.error")]
      df$Sweep <- i
      return(df)
    }
  }) %>%
    do.call(rbind, .) %>%
    rename(In9_Tau_est = estimate, In9_Tau_std.err = std.error) %>%
    mutate(Sweep = as.factor(Sweep))

  test_fits_plts <- map(seq_along(test_fits), function(i){
    if(!is.na(test_fits[[i]]$fit)){
      ggplot()+
      geom_line(data = as.data.frame(test_fits[[i]]$check_fit), aes(x = Time, y = Fit), color = "firebrick")+
      geom_line(data = as.data.frame(test_fits[[i]]$check_fit), aes(x = Time, y = In9), alpha = 0.8)+
      labs(subtitle = paste("tau =", as.character(
        round(test_fits[[i]]$fit[2, 2], digits = 3)
      )))
    }
  })

  plt_tau_in9 <- cowplot::plot_grid(plotlist = test_fits_plts)


  ## Condense ====

  summary_df <- full_join(temp_resist, temp_rmp) %>% full_join(., fits_in4) %>% full_join(., fits_in9)

  return(list(
    diagnostic_plots = list(
      trace = plt_trace,
      segment = plt_segments,
      fit_In4 = plt_tau_in4,
      fit_In9 = plt_tau_in9
    ),
    df = summary_df
  ))
}

