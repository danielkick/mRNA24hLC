#' @title Subtract Leak from Dataframe
#'
#' @description This function takes current and voltage columns from a dataframe with current/voltage pairs from an I_{HTK} protocol and fits then subtracts the leak current from the measurements.
#'
#' @param input.df dataframe containing current/voltage pairs from a voltage clamp step protocol.
#' @param min.mV The lower bound for the passive region of the curve.
#' @param max.mV The upper bound for the passive region of the curve.
#' @param ch.i The column represnting the current measurements.
#' @param ch.v The column representing the voltage measurements.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples

## Subtract leak from ihtk protocol
subtract_leak_df <- function(input.df = temp,
                             min.mV = -81,
                             max.mV = -39,
                             ch.i = "S2Max",
                             ch.v = "S1Max"){

  # input.df = temp
  # min.mV = -81
  # max.mV = -39
  # ch.i = "S2Max"
  # ch.v = "S1Max"

  input.df <- as.data.frame(input.df)

  # input.df <- input.df[!is.na(input.df[[ch.v]]), ]

  if (nrow(input.df[!is.na(input.df[[ch.v]]), ]) > 1 ){
    input.df <- input.df %>% rename(CH.I = ch.i) %>% rename(CH.V = ch.v)

    fm <- lm(CH.I ~ CH.V, input.df[input.df$CH.V >= min.mV & input.df$CH.V <= max.mV, ], na.action = "na.omit")

    input.df <- input.df %>% mutate(leak = predict(object = fm, newdata = input.df), na.action = "na.omit")
    input.df <- input.df %>% mutate(leak.sub = CH.I - leak)

    # input.df <- input.df %>% rename(ch.i = CH.I) %>% rename(ch.v = CH.V)

    names(input.df)[names(input.df) == "CH.I"] <- ch.i
    names(input.df)[names(input.df) == "CH.V"] <- ch.v


    # input.df %>% select(S1Max, S2Max, leak, leak.sub) %>% gather(type, value, 2:4) %>% ggplot(aes(x = S1Max, y = value, color = type))+geom_point()


  }
  return(input.df)

}
