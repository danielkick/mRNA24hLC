#' @title Scatter plot with and without flagged points
#'
#' @description Returns an xy scatterplot with a linear regression and equation for all data and without flagged points. Useful for showing the effects of outliers.
#'
#' @param temp The input dataframe. Must contain a logical column named "flag".
#' @param X The column name of the desired x axis.
#' @param Y The column name of the desired y axis.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples

scatter_w_wo_flag <- function(temp = mutate(filter(M, Time == "Baseline"), flag = ifelse(Ia.0 < 75 & shal < 300, T, F)),
                              X = "shal",
                              Y = "Ia.0"){
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
