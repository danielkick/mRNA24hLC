#' @title Scatter plot with and without outliers
#'
#' @description Less flexible version of `scatter_w_wo_flag`. Returns an xy scatterplot with a linear regression and equation for all data and all data sans outliers based on 1.5xIQR.
#'
#' @param temp The input dataframe.
#' @param X The column name of the desired x axis.
#' @param Y The column name of the desired y axis.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples


scatter_w_wo_outliers <- function(temp = filter(M, Time == "Baseline"),
                                  X = "bkkca",
                                  Y = "Ihtk.0"){
  # flag outliers based on 1.5xIQR from median
  X_low  <- median(temp[[X]], na.rm = T) - (1.5 * IQR(temp[[X]], na.rm = T))
  X_high <- median(temp[[X]], na.rm = T) + (1.5 * IQR(temp[[X]], na.rm = T))
  Y_low  <- median(temp[[Y]], na.rm = T) - (1.5 * IQR(temp[[Y]], na.rm = T))
  Y_high <- median(temp[[Y]], na.rm = T) + (1.5 * IQR(temp[[Y]], na.rm = T))
  X_pass <- (temp[[X]] > X_low) * (temp[[X]] < X_high)
  Y_pass <- (temp[[Y]] > Y_low) * (temp[[Y]] < Y_high)
  temp$flag <- ifelse((X_pass * Y_pass) == 1, T, F)
  # Duplicate so we have dataset 1, 2 (introduces duplicates)
  temp <- rbind(temp[temp$flag == T, ], mutate(temp, flag = F))
  formula1 <- y ~ x
  plt <- ggplot(temp, aes_string(X, Y, color = "flag"))+
    geom_smooth(data = temp, method = lm, se = F, fullrange = T)+
    geom_point(data = temp)+
    geom_point(data = temp, color = "black", shape = 1)+
    geom_point(data = temp[temp$flag, ])+
    ggpmisc::stat_poly_eq(aes(label =  paste(stat(eq.label), "*\" with \"*",
                                             stat(rr.label), "*\", \"*",
                                             stat(f.value.label), "*\", and \"*",
                                             stat(p.value.label), "*\".\"",
                                             sep = "")),
                          formula = formula1, parse = TRUE, size = 4)+
    scale_color_manual(values = c("darkgray", "black"))+
    theme_bw()+
    theme(legend.position = "")
  return(plt)
}

