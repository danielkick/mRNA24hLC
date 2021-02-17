#' @title Visualize PCA in 3D
#'
#' @description Plot data in top 3 dimensions of PC space
#'
#' @param input.df The input dataframe. Should contain only numerics.
#' @param color.by A vector to color the points by.
#' @param use.colors A vector of the colors to use.
#' @param ellipse.opacity Denotes opacity of the ellipse.
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples

vis_pca_3D_plotly <- function(input.df = dplyr::filter(M, pass == T) %>% dplyr::select(-lc, -net, -pass),
                              color.by = M[M$pass == T, "lc"],
                              # turn.x.times = 2,
                              use.colors = RColorBrewer::brewer.pal(8, "Set1"),
                              point.size = 7,
                              point.opacity = 0.04,
                              dropdowns = T,
                              dropdown.width = 1.5,
                              dropdown.opacity = 1,
                              ellipse.opacity = 0.05){

  res.pca <- PCA(input.df,  graph = FALSE)

  indivduals <- get_pca_ind(res.pca)
  indivduals <- as.data.frame(indivduals$coord)
  indivduals$cell.num <- as.factor(as.character(color.by))

  # manually set so that we can apply a dropdown that goes to the "floor"
  zaxis_pad <- 0.05 *(max(indivduals$Dim.3) - min(indivduals$Dim.3))

  pca_plt <- plot_ly() %>%
    add_markers(x = indivduals$Dim.1,
                y = indivduals$Dim.2,
                z = indivduals$Dim.3,
                color = indivduals$cell.num,
                colors = use.colors,
                marker = list(
                  size = point.size, #5, #3.5,
                  opacity= point.opacity)) %>%
    layout(scene = list(xaxis = list(title = 'Dim. 1'),
                        yaxis = list(title = 'Dim. 2'),
                        zaxis = list(title = 'Dim. 3',
                                     range = c(min(indivduals$Dim.3)-zaxis_pad,
                                               max(indivduals$Dim.3)+zaxis_pad
                                     )
                        )))

  # add drop down lines
  if (dropdowns == T){
    for (i in 1:nrow(indivduals)){
      plt_floor <- min(indivduals$Dim.3)-zaxis_pad
      indivduals_dropdown <- indivduals[i, ]
      indivduals_dropdown <- rbind(indivduals_dropdown, indivduals_dropdown)

      indivduals_dropdown[1, "Dim.3"] <- plt_floor#0

      pca_plt <- pca_plt %>%
        add_trace(
          x = indivduals_dropdown$Dim.1,
          y = indivduals_dropdown$Dim.2,
          z = indivduals_dropdown$Dim.3,
          color = indivduals_dropdown$cell.num,
          mode = "lines",
          line = list(
            width = dropdown.width,
            opacity = dropdown.opacity
          #   color = use.colors[as.numeric(indivduals[i, ]$cell.num)]
          )
        )
    }
  }


  # add ellipses
  if (ellipse.opacity > 0){
    # inspired by:
    # https://stackoverflow.com/questions/50412858/plotting-ellipse3d-in-r-plotly-with-surface-ellipse
    ellipse_list <- map(unique(indivduals$cell.num), function(ellipse.group){
      rgl::ellipse3d(cov(
        indivduals[
          indivduals$cell.num == ellipse.group,
          c("Dim.1", "Dim.2", "Dim.3")]
      ))
    })

    for(i in seq_along(unique(indivduals$cell.num))){
      # add ellipses one at a time so that we can accommodate an arbitrary number of them
      pca_plt <- pca_plt %>%
        add_trace(x=ellipse_list[[i]]$vb [1,],
                  y=ellipse_list[[i]]$vb [2,],
                  z=ellipse_list[[i]]$vb [3,],
                  color = unique(indivduals$cell.num)[i],
                  type='mesh3d',
                  alphahull = 0,
                  opacity = ellipse.opacity)
    }
  }

  return(pca_plt)
}



