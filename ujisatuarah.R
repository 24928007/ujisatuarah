# File: one_way_anova.R
one_way_anova <- function(data, response, group) {
  # Check input
  if (!is.data.frame(data)) stop("Data must be a data frame.")
  if (!response %in% names(data)) stop("Response variable not found in data.")
  if (!group %in% names(data)) stop("Group variable not found in data.")
  
  # Perform one-way ANOVA
  formula <- as.formula(paste(response, "~", group))
  result <- aov(formula, data = data)
  
  return(summary(result))
}
