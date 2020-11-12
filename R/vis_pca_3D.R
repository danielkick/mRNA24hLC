#' @title Visualize PCA in 3D
#'
#' @description Plot data in top 3 dimensions of PC space
#'
#' @param input.df The input dataframe. Should contain only numerics.
#' @param color.by A vector to color the points by.
#' @param turn.x.times Number of times the plot should rotate.
#' @param use.colors A vector of the colors to use.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples

vis_pca_3D <- function(input.df = dplyr::filter(M, pass == T) %>% dplyr::select(-lc, -net, -pass),
                       color.by = M[M$pass == T, "lc"],
                       turn.x.times = 2,
                       use.colors = RColorBrewer::brewer.pal(8, "Set1")){

  res.pca <- PCA(input.df,  graph = FALSE)

  indivduals <- get_pca_ind(res.pca)
  indivduals <- as.data.frame(indivduals$coord)
  indivduals$cell.num <- as.factor(as.character(color.by))

  car::scatter3d(x = indivduals$Dim.1,
                 z = indivduals$Dim.3,
                 y = indivduals$Dim.2,
                 groups = indivduals$cell.num,



                 surface=F,
                 # fit = "smooth",
                 # surface.alpha = 0.5,
                 grid = F,
                 # residuals = F,
                 bg.col = "white", # white black
                 surface.col = use.colors,
                 point.col = use.colors,

                 xlab = "Dim. 1",
                 ylab = "Dim. 2",
                 zlab = "Dim. 3",
                 revolutions=turn.x.times,
                 # revolutions = 1,

                 # surface = F,
                 ellipsoid = TRUE
  )
}
