#' @title Process GJVC trace
#'
#' @description Takes a gap junction measuring voltage clamp recording from `prep_abf()` and returns I_leak, I_g, and diagnostic plots.
#'
#' @param trace The prepared abf data.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples

process_gjvc_trace <- function(trace = trace_list$GJVC){
  ### Setup
  i_leak <- trace %>%
    # filter(Time > 0.75) %>%
    mutate(In4 = as.numeric(In4),
           In7 = as.numeric(In7),
           In9 = as.numeric(In9),
           In12 = as.numeric(In12))

  i_leak <- i_leak %>%
    #When are the step start times?
    # 1. In9: ~0.27575
    # 2. In4: ~1.02532
    mutate(Inj_in = ifelse(Time > 0.8, "In4", "In9"),
           Segment = case_when(
             Time >= (0.07575 - 0.02532) & Time <= (0.27575 - 0.02532) ~ "PreStep", # Inj In9
             Time >= (0.27575+ 0.17468) & Time <= (0.27575+ 0.22468) ~ "Step",

             Time >= 0.8 & Time <= 1.0 ~ "PreStep", # Inj In4
             Time >= 1.2 & Time <= 1.25 ~ "Step"
           ))

  plt_tagged_regions <- downsample_data(i_leak, len = 10000) %>%
    gather("key", "value", c("In4", "In7", "In9", "In12")) %>%
    mutate(Cell   = case_when(key %in% c("In4", "In7")  ~ "Cell1",
                              key %in% c("In9", "In12") ~ "Cell2"),
           Aspect = case_when(key %in% c("In4", "In9")  ~ "mV",
                              key %in% c("In7", "In12") ~ "nA")
    ) %>%
    ggplot(aes(Time, value, color = Segment, group = Sweep))+
    geom_path()+
    # facet_wrap(key~., scales = "free")
    facet_grid(Aspect~Cell, scales = "free")


  i_leak_for_In4_to_In9 <- i_leak %>%
    filter(Inj_in == "In4") %>%
    group_by(Sweep, Segment, Inj_in) %>%
    mutate(In4Mean = mean(In4, na.rm = T),
           In7Mean = mean(In7, na.rm = T),
           In9Mean = mean(In9, na.rm = T),
           In12Mean =mean(In12, na.rm = T)) %>%
    ungroup()

  i_leak_for_In9_to_In4 <- i_leak %>%
    filter(Inj_in == "In9") %>%
    group_by(Sweep, Segment, Inj_in) %>%
    mutate(In4Mean = mean(In4, na.rm = T),
           In7Mean = mean(In7, na.rm = T),
           In9Mean = mean(In9, na.rm = T),
           In12Mean =mean(In12, na.rm = T)) %>%
    ungroup()



  ### Ig ####
  i_leak_for_In4_to_In9 <- i_leak_for_In4_to_In9 %>%
    select(Sweep, Segment, In4Mean, In7Mean, In9Mean, In12Mean) %>%
    filter(Segment != "NA") %>%
    distinct() %>%
    pivot_wider(names_from = "Segment",
                values_from = c("In4Mean", "In12Mean",
                                "In9Mean", "In7Mean" )) %>%
    mutate(In4Mean_Step  = In4Mean_Step - In4Mean_PreStep,
           In12Mean_Step = In12Mean_Step - In12Mean_PreStep,
           In9Mean_Step  = In9Mean_Step - In9Mean_PreStep,
           In7Mean_Step  = In7Mean_Step - In7Mean_PreStep
    ) %>%
    select(Sweep, In4Mean_Step, In12Mean_Step,
           In9Mean_Step, In7Mean_Step) %>%
    rename(In4Mean = In4Mean_Step, In12Mean = In12Mean_Step,
           In9Mean = In9Mean_Step, In7Mean = In7Mean_Step) %>%
    filter(abs(In4Mean) >= 4 | abs(In9Mean) >= 4 )# exclude no step condition


  i_leak_for_In9_to_In4 <- i_leak_for_In9_to_In4 %>%
    select(Sweep, Segment, In4Mean, In7Mean, In9Mean, In12Mean) %>%
    filter(Segment != "NA") %>%
    distinct() %>%
    pivot_wider(names_from = "Segment",
                values_from = c("In4Mean", "In12Mean",
                                "In9Mean", "In7Mean" )) %>%
    mutate(In4Mean_Step  = In4Mean_Step - In4Mean_PreStep,
           In12Mean_Step = In12Mean_Step - In12Mean_PreStep,
           In9Mean_Step  = In9Mean_Step - In9Mean_PreStep,
           In7Mean_Step  = In7Mean_Step - In7Mean_PreStep
    ) %>%
    select(Sweep, In4Mean_Step, In12Mean_Step,
           In9Mean_Step, In7Mean_Step) %>%
    rename(In4Mean = In4Mean_Step, In12Mean = In12Mean_Step,
           In9Mean = In9Mean_Step, In7Mean = In7Mean_Step) %>%
    filter(abs(In4Mean) >= 4 | abs(In9Mean) >= 4 )# exclude no step condition


  plt_ig_slopes <- ggplot()+
    geom_smooth(data = i_leak_for_In4_to_In9, aes(In4Mean, In12Mean), method = lm, color = "red")+
    geom_point( data = i_leak_for_In4_to_In9, aes(In4Mean, In12Mean), color = "red")+
    geom_smooth(data = i_leak_for_In9_to_In4, aes(In9Mean, In7Mean), method = lm)+
    geom_point( data = i_leak_for_In9_to_In4, aes(In9Mean, In7Mean))+
    labs(x = "Difference in mV", y = "nA")


  In4_to_In9_Ig <- as.numeric(broom::tidy(lm(In12Mean ~ In4Mean, data = i_leak_for_In4_to_In9))[2, "estimate"])
  In9_to_In4_Ig <- as.numeric(broom::tidy(lm(In7Mean ~ In9Mean,  data = i_leak_for_In9_to_In4))[2, "estimate"])

  ### Ileak ####
  In4_R <- as.numeric(broom::tidy(lm(In7Mean ~ In4Mean,  data = i_leak_for_In4_to_In9))[2, "estimate"])
  In9_R <- as.numeric(broom::tidy(lm(In12Mean ~ In9Mean, data = i_leak_for_In9_to_In4))[2, "estimate"])
  In4_R <- In4_R^-1
  In9_R <- In9_R^-1

  return(
    list(diagnostic_plots = list(
      trace = plt_tagged_regions,
      fit = plt_ig_slopes),
      df = data.frame(
        In4_to_In9 = In4_to_In9_Ig,
        In9_to_In4 = In9_to_In4_Ig,
        In4_R1c = In4_R,
        In9_R1c = In9_R
      )
    )
  )

}
