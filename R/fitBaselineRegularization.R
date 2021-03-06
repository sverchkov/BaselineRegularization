#' Fit baseline regularization
#'
#' Fit Baseline Regularization with the given parameters
#'
#' @param parameters parameters object produced by [defineBRParameters]
#' @param br_data data object produced by [prepareBRData]
#' @return An object representing the fit model, with elements:
#' * beta, the model coefficients
#' * t
#' * res
#' * parameters, (the `parameters` passed to this function)
#' * drug_concept_id, vector of drug concept IDs, for matching back with beta coefficients
#' @export
#' @import futile.logger
#' @author Zhaobin Kuang
fitBaselineRegularization <- function( br_data, parameters = defineBRParameters() ){

  if ( is.null( br_data ) )
    stop ( flog.fatal("Attempted to run fitBaselineRegularization with bad input.") )
  if ( is.null( br_data$X ) && is.list( br_data ) )
    return ( lapply( br_data, fitBaselineRegularization, parameters ) )

  result <- fitBR(
    interval_baseline_parameter = br_data$interval_baseline_parameter,
    baseline_parameter_obs_period = br_data$baseline_parameter_obs_period,
    X = br_data$X,
    l = br_data$l,
    n = br_data$n,
    lambda1 = parameters$lambda1,
    lambda2 = parameters$lambda2,
    lambda3 = parameters$lambda3,
    max_outer_loop_iterations = parameters$max_outer_loop_iterations,
    max_inner_loop_iterations = parameters$max_inner_loop_iterations,
    save_trajectory = parameters$save_trajectory )

  c( result, list(parameters = parameters, drug_concept_id = br_data$drug_concept_id, response_event = br_data$response_event ) )
}
