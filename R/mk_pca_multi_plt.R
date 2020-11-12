#' @title Make PCA Multiplot
#'
#' @description Return a list with a screeplot, top 20 contribibuitons for the first 3 PCs and a biplot.
#'
#' @param input.df The input dataframe. Should contain only numerics.
#' @param scree.max.y The maximum y for screept plot
#'
#' @author Daniel Kick (\email{daniel.r.kick@@gmail.com})
#'
#' @export
#'
#' @examples

mk_pca_multi_plt <- function(input.df = t_pass,
                             scree.max.y = 100){

  res.pca <- PCA(input.df,  graph = FALSE)
  # Extract eigenvalues/variances
  # get_eig(res.pca)

  # Visualize eigenvalues/variances
  p0 <- fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, scree.max.y))

  p1 <- fviz_contrib(res.pca, choice = "var", axes = 1, top = 20)
  p2 <- fviz_contrib(res.pca, choice = "var", axes = 2, top = 20)
  p3 <- fviz_contrib(res.pca, choice = "var", axes = 3, top = 20)

  # Control variable colors using their contributions
  p4 <- fviz_pca_var(res.pca, col.var="contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE # Avoid text overlapping
  )

  # pca.multi <- {p4|p0}/{p1+p2+p3}
  # return(pca.multi)
  return(list(scree = p0,
              cont1 = p1,
              cont2 = p2,
              cont3 = p3,
              biplt = p4))
}
