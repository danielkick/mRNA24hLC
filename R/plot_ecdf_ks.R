#' @title Plot Empirical CDF and KS test
#'
#' @description Generate an ecdf with KS test statisitcs.
#'
#' @param df The input dataframe.
#' @param data.col The column to be used for the ecdf.
#' @param group.col The column containing group labels.
#' @param group1 The first group.
#' @param group2 The second group.
#' @param colors The colors to be used.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples

plot_ecdf_ks <- function(
  df = rbind(temp1, temp2),
  data.col = "Corr",
  group.col = "group",
  group1 = "Baseline",
  group2 = "Compensated",
  colors = c("#4d4d4d",
             #"#67a9cf",
             "#1c9099")) {

  # Adapted from:
  # https://rpubs.com/mharris/KSplot
  df <- filter(df, df[[group.col]] %in% c(group1, group2))

  df[df[[group.col]] == group1, data.col]


  data1 <- unlist(df[df[[group.col]] == group1, data.col])
  data2 <- unlist(df[df[[group.col]] == group2, data.col])

  ecdf1 <- ecdf(data1)
  ecdf2 <- ecdf(data2)

  # used to get the most extreme difference between the two samples
  MostExtremeDiff <- seq(min(data1, data2, na.rm = T), max(data1, data2, na.rm = T), length.out = length(data1))
  x0 <- MostExtremeDiff[which(abs(ecdf1(MostExtremeDiff) - ecdf2(MostExtremeDiff)) ==
                                max(abs(ecdf1(MostExtremeDiff) - ecdf2(MostExtremeDiff))))]
  y0 <- ecdf1(x0)
  y1 <- ecdf2(x0)

  graph.df <- data.frame(data1, data2) %>% gather(Condition, Value, 1:2)
  graph.df[graph.df$Condition == "data1", "Condition"] <- group1
  graph.df[graph.df$Condition == "data2", "Condition"] <- group2

  # Run two sided KS test on data
  test.res <- ks.test(data1, data2)

  plt <-
    ggplot(graph.df)+
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                 linetype = "dashed", color = "black", size = 1)+
    geom_point(aes(x = x0[1] , y= y0[1]), color="black", size=2) +
    geom_point(aes(x = x0[1] , y= y1[1]), color="black", size=2) +
    stat_ecdf(aes(x = Value, group = Condition, color = Condition))+
    labs(x = "Sample",
         y = "ECDF",
         subtitle = paste("K-S Test", as.character(group1), "vs", as.character(group2),
                       "\np-value:", as.character(round(test.res$p.value, digits = 4))))+
    theme_minimal()+
    theme(legend.position = "bottom")+
    scale_color_manual(values = colors)#+
  # theme(text=element_text(family="Calibri Light", size=14))

  return(plt)

}
