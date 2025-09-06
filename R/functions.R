#' DistCalc
#'
#' @param df data frame
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{DistCalc(df = df)}
DistCalc <- function(df){
  df <- as.matrix(df)
  S <- cov(df)
  mu <- colMeans(df)
  mumat <- matrix(mu, ncol = 1)
  distance <- matrix(nrow = length(df[,1]))
  for (i in 1:length(df[,1])){
    distance[i,] <- t(df[i,] - mumat) %*% solve(S) %*% (df[i,] - mumat)
  }
  list(distance = distance)
}


#' NormCheck
#'
#' @param df data frame
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot, geom_point, stat_smooth, ggtitle, xlab, ylab
#'
#' @examples
#' \dontrun{NormCheck(df = df)}
NormCheck <- function(df){
  df < as.matrix(df)
  n <- dim(df)[1]
  p <- dim(df)[2]
  d <- df[,p]
  j <- 1:n
  q <- qchisq((j - 0.5)/n, p)
  qq <- data.frame(y = sort(d),
                   x = q)
  QQ <- qq |>
    ggplot(aes(x = x, y= y)) +
    geom_point() +
    stat_smooth(method = "lm", formula = y ~ x) +
    ggtitle("Theoretical to Tested Quantiles") +
    xlab("Theoretical Quantiles") +
    ylab("Experimental Quantiles")

  limit <- qchisq(0.5, df = p)
  check <- length(which(d <= limit))/length(d)

  zcheck <- qchisq(p = 0.995,df = p)
  outliers <- which(d > zcheck)
  new <- d[-c(outliers)]

  n <- length(new)
  d <- new
  j <- 1:n
  q <- qchisq((j - 0.5)/n, p)
  qq <- data.frame(y = sort(d),
                   x = q)
  QQNew <- qq |>
    ggplot(aes(x = x, y= y)) +
    geom_point() +
    stat_smooth(method = "lm", formula = y ~ x) +
    ggtitle("Theoretical to Tested Quantiles: Outliers Removed") +
    xlab("Theoretical Quantiles") +
    ylab("Experimental Quantiles")

  list(QQ_Plot = QQ,
       Chi_Squared_50 = check,
       Distance = d,
       Z_Scale_Outliers = d[outliers],
       Outliers_Removed = new,
       New_QQ = QQNew)
}



#' myPrinComp
#'
#' @param df data frame
#' @param mat R or S matrix
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot, geom_line, geom_point, xlab, ylab
#'
#' @examples
#' \dontrun{myPrinComp(df = df, mat = "S")}
myPrinComp <- function(df, mat){
  ifelse(mat == "S",
         data <- cov(df),
         data <- cor(df))

  e <- eigen(data)
  lambda <- e$values
  vec <- e$vectors
  pr <- prcomp(data, center = FALSE)
  prin <- princomp(data)
  scree <- data.frame(
    Comp = 1:length(pr$sdev),
    Var = pr$sdev^2 / sum(pr$sdev^2)
  )

  screeplt <- ggplot(scree, aes(x = Comp, y = Var)) +
    geom_line() +
    geom_point() +
    xlab("Principal Components") +
    ylab("Proportion of Variance")

  list(eigen_values = lambda,
       eigen_vectors = vec,
       prcomp = pr,
       principle_components = prin,
       Cumulative_Proportion = cumsum(lambda)/sum(lambda),
       Scree_Plot = screeplt)
}


#' myFA
#'
#' @param df data frame
#' @param factors number of factors
#' @param rotation type of rotation
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{myFA(df = df, factors = 1, rotation = "none")}
myFA <- function(df, factors = 1, rotation = "none"){
  df <- as.matrix(df)
  R <- cor(df)
  e <- eigen(R)
  lambda <- e$values
  cp <- cumsum(lambda)/sum(lambda)
  fa <- factanal(covmat = R,
                 factors = factors,
                 rotation = rotation)
  resid <- R - fa$loadings %*% t(fa$loadings) - fa$uniquenesses

  list(R = R,
       Eigen_Vectors = e$vectors,
       Eigen_Values = e$values,
       Cumulative_Proportion = cp,
       Uniqueness = fa$uniquenesses,
       Loadings = fa$loadings,
       Residual_Matrix = resid)
}


#' Clust
#'
#' @param df data frame
#' @param G mixture components
#' @param k number of clusters
#' @param what type of plot from mclust
#'
#' @return
#' @export
#' @importFrom mclust mclust
#'
#' @examples
#' \dontrun{Clust(df = df, G = 1, k = 1, what = "uncertainty")}
Clust <- function(df, G = NULL, k = 1, what = "uncertainty"){
  pc <- princomp(x = df,
                 cor = TRUE,
                 scores = TRUE)

  sc <- pc$scores

  clus <- kmeans(x = sc[,1:2],
                 centers = k)

  km <- plot(sc[,1:2],
             pch = 21,
             cex = 1.5,
             bg = clus$cluster,
             main = paste0("K = ", k))

  mc <- Mclust(df, G = G)

  mcplot <- plot(mc, what = what)

  list(Principle_Components = pc,
       Summary = summary(mc, parameters = TRUE),
       kmeans_plot = km,
       MClust_Plot = mcplot)

}
