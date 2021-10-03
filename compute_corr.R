#' Computes biometrical parameters
#'
#' @param data input data set
#' @param var1 name of variable1 (unquoted)
#' @param var2 name of variable2 (unquoted)
#'
#' @return A tibble with the Pearson correlation and p-value
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' compute_corr(data = faithful, var1 = eruptions, var2 = waiting)


compute_corr<-function(data, var1,var2){

  #Compute correlation
   stats::cor.test(
    x = data %>% dplyr::pull({{var1}}),
    y = data %>% dplyr::pull({{var2}})
    )%>%

  #tidy up results-----
    broom::tidy() %>%

  # retain and relevant bits -----

    dplyr::select(
      correlation = .data$estimate,
      pval = .data$p.value
    )


  }
