#' multi trait multi method matrix
#' 
#' renders a MTMM using ggplot2. This function will split the variable names in a correlation matrix, or a data.frame. The first part will be used as the trait, the second as the method. Correlations are displayed as text, with the font size corresponding to absolute size.
#' You can optionally supply a data frame of reliabilites to show in the diagonal.
#'
#' @param variables data frame of variables that are supposed to be correlated
#' @param reliabilities data frame of reliabilties: column 1: scale, column 2: rel. coefficient
#' @param split_regex regular expression to separate construct and method from the variable name, splits on '.' by default
#' @param cors you can also supply a (named) correlation matrix
#' 
#' @import ggplot2
#' 
#' @export
#' @examples
#' data.mtmm = data.frame(
#' `Ach_self_report` = rnorm(200), `Pow_self_report` = rnorm(200), `Aff_self_report`= rnorm(200),
#' `Ach_peer_report` = rnorm(200),`Pow_peer_report`= rnorm(200),`Aff_peer_report` = rnorm(200),
#' `Ach_diary` = rnorm(200), `Pow_diary` = rnorm(200),`Aff_diary` = rnorm(200))
#' reliabilities = data.frame(scale = names(data.mtmm), rel = stats::runif(length(names(data.mtmm))))
#' mtmm(data.mtmm, reliabilities = reliabilities)
#' 
mtmm = function(variables = NULL, reliabilities = NULL, split_regex = "_", 
  cors = NULL) {
  if (is.null(cors) & is.null(variables)) {
    stop("You have to provide either cors or variables.")
  }
  if (is.null(cors) & !is.null(variables)) 
    cors = stats::cor(variables, use = "pairwise.complete.obs")  # select variables
  
  var.names = colnames(cors)
  
  corm = reshape2::melt(cors)
  names(corm) = c("Var1", "Var2", "value")
  # substitute the 1s with the scale reliabilities here
  corm = corm[corm[, "Var1"] != corm[, "Var2"], ]
  if (!is.null(reliabilities)) {
    rel = reliabilities
    names(rel) = c("Var1", "value")
    rel$Var2 = rel$Var1
    rel = rel[which(rel$Var1 %in% var.names), c("Var1", "Var2", 
      "value")]
    corm = rbind(corm, rel)
  }
  
  if (any(is.na(stringr::str_split_fixed(corm$Var1, split_regex, 
    n = 2)))) {
    print(unique(stringr::str_split_fixed(corm$Var1, split_regex, 
      n = 2)))
    stop("regex broken")
  }
  # regex matching our column naming schema to extract trait
  # and method
  corm[, c("trait_X", "method_X")] = stringr::str_split_fixed(corm$Var1, 
    split_regex, n = 2)
  corm[, c("trait_Y", "method_Y")] = stringr::str_split_fixed(corm$Var2, 
    split_regex, n = 2)
  corm[, c("trait_Y", "method_Y", "trait_X", "method_X")] = sapply(corm[, 
    c("trait_Y", "method_Y", "trait_X", "method_X")], FUN = function(x) stringr::str_replace_all(x, 
    "(\\.|_)", " "))
  # sort pairs to find dupes
  corm[, c("var1.s", "var2.s")] <- t(apply(corm[, c("Var1", 
    "Var2")], 1, sort))
  corm[which(corm[, "trait_X"] == corm[, "trait_Y"] & corm[, 
    "method_X"] != corm[, "method_Y"]), "type"] = "validity"
  corm[which(corm[, "trait_X"] != corm[, "trait_Y"] & corm[, 
    "method_X"] == corm[, "method_Y"]), "type"] = "heterotrait-monomethod"
  corm[which(corm[, "trait_X"] != corm[, "trait_Y"] & corm[, 
    "method_X"] != corm[, "method_Y"]), "type"] = "heterotrait-heteromethod"
  corm[which(corm[, "trait_X"] == corm[, "trait_Y"] & corm[, 
    "method_X"] == corm[, "method_Y"]), "type"] = "reliability"
  
  # would be nice to have the facet_grid labels in the same
  # palce as the tick marks
  corm$trait_X = factor(corm$trait_X)
  corm$trait_Y = factor(corm$trait_Y, levels = rev(levels(corm$trait_X)))
  corm$method_X = factor(corm$method_X)
  corm$method_Y = factor(corm$method_Y, levels = levels(corm$method_X))
  corm = corm[order(corm$method_X, corm$trait_X), ]
  corm = corm[!duplicated(corm[, c("var1.s", "var2.s")]), ]  # remove dupe pairs
  corm$rvalue = stringr::str_replace(round(corm$value, 2), 
    "0\\.", ".")
  corm$logvalue = log(corm$value^2)
  # building ggplot the melted correlation matrix
  mtmm_plot <- ggplot(data = corm) + geom_tile(aes_string(x = "trait_X", 
    y = "trait_Y", fill = "type")) + geom_text(aes_string(x = "trait_X", 
    y = "trait_Y", label = "rvalue", size = "logvalue")) + 
    facet_grid(method_Y ~ method_X) + ylab("") + xlab("") + 
    theme_bw(base_size = 18) + theme(panel.background = element_rect(colour = NA), 
    panel.grid.minor = element_blank(), axis.line = element_blank(), 
    panel.border = element_blank(), strip.background = element_blank(), 
    panel.grid = element_blank(), legend.position = c(1, 
      1), legend.justification = c(1, 1)) + scale_fill_brewer("Type") + 
    scale_size("Absolute size", guide = F) + scale_colour_gradient(guide = F)
  
  mtmm_plot
}