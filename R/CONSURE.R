#' CONSURE: Spatially continuous
#' survival, use of space and recovery probability estimates
#'
#' The CONSURE package provides functions to perform the continuous and the
#' combined approach from the dissertation of Saskia Schirmer. It can work with
#' both, simulated and real-world data.
#'
#' Nearly all functions are based on a \code{\link{mark_recapture_object}}.
#'
#' A spatial point pattern can be simulated by given survival, migratory
#' connectivity and recovery probability using \code{\link{sim_contin}}.
#'
#' @section Visualizing raw data:
#'
#' Spatial point patterns of raw recoveries can be visualized by
#' \code{\link{plot_raw_recoveries}}. The age distribution can be visualized by
#' \code{\link{plot_age_distribution}}.
#'
#' @section Continuous functions:
#'
#' The continuous estimation approach first estimates the density of the point
#' pattern by \code{\link{est_kde}}. Then, survival can be estimated by
#' \code{\link{est_s}} and finally migratory connectivity by \code{\link{est_m}}
#' and a constant recovery probability by \code{\link{est_r}}. The function
#' \code{\link{est_parameters}} is a wrapper-function performing kernel density
#' estimation and parameter estimation at once.
#'
#' All estimates can be plotted by the appropriate function:
#' \code{\link{plot_kde}}, \code{\link{plot_s}}, \code{\link{plot_m}}. The R^2
#' values of the linear model used to fit survival, migratory connectivity and
#' recovery probability can be plotted by \code{\link{plot_gof_of_lm}}.
#'
#' @section Combined functions:
#'
#' If recovery probability is not constant over space, an extra
#' \code{\link{optimizationObject}} is needed. The prop argument must be either
#' specified, e.g., by a discrete estimate of migratory connectivity for
#' real-world data or simulated data (not included in this package at the
#' moment), or the true discrete migratory connectivity values can be calculated
#' by \code{\link{calcDiscreteM}} and forwarded by the markRecaptureObject.
#'
#' Migratory connectivity and recovery probability can then be estimated
#' spatially continuously by \code{\link{combEstimate}}.
#'
#' The combined estimates can be plotted by \code{\link{plotCombM}} and
#' \code{\link{plotCombR}}.
#'
#' @section Real-world data:
#'
#' Some functions help processing real-world data: column names of data frames
#' containing dead recoveries can be adjusted by \code{\link{renameData}}.
#' If a shape file of the considered continuous non-breeding area is available,
#' it can be transformed to an owin object by \code{\link{createOwinFromShp}}.
#'
#' @section Additional functions:
#'
#' All other functions should not be relevant to the user. For a short overview:
#'
#' A markRecaptureObject consists of a breedingAreaObject and a
#' winteringAreaObject.
#' The constructors \code{\link{new_markRecaptureObject}},
#' \code{\link{new_breedingArea}}, \code{\link{new_winteringArea}} are accessed
#' via their helper functions \code{\link{markRecaptureObject}},
#' \code{\link{breedingArea}}, \code{\link{winteringArea}}. Also the constructor
#' of the optimizationObject \code{\link{new_optimizationObject}} is accessed
#' via the helper function \code{\link{optimizationObject}}.
#'
#' To define the density \code{\link{f_f}} of the point pattern correctly, the
#' subdensity of found \code{\link{f_f_sub}} individuals is needed. The
#' probability to be not seen \code{\link{p_nf}} is integrated over the
#' subdensity of not found \code{\link{f_nf_sub}} individuals.
#'
#' Estimating survival and migratory connectivity in the continuous approach is
#' based on a linear model which is estimated by \code{\link{estLM}}.
#'
#' The optimization procedure initializes B-splines via \code{\link{initSpline}}
#' and defines B-splines by \code{\link{defineBspline}}. The penalizing function
#' \code{\link{pen}} integrates the distance to the continuous estimate via
#' \code{\link{integrateDist2Continuous}} and the distance to the discrete
#' estimate via \code{\link{integrateDist2Discrete}}. The smoothness penalty is
#' implemented in \code{\link{Lh}}.
#'
#' The number of recovered individuals per area of origin can be summarized by
#' \code{\link{rec_inds_func}}.
#'
#' \code{\link{par_grid}} creates a grid containing the values of a specific
#' function on the grid.
#'
#' @section Uncertainty estimation:
#'
#' The uncertainty of the parameter estimates can be assessed by bootstrapping
#' with the function \code{\link{est_uncertainty}}. Optionally, data can be
#' bootstrapped before starting the estimation process using the function
#' \code{\link{init_bootstrapped_datasets}}. \code{\link{bootstrap_quantiles}}
#' calculates the 0.025- and 0.975-bootstrap quantiles. These quantiles can be
#' visualized in 3D with \code{\link{plotly_param}} or alternatively, as a
#' 2D surface with a bootstrap quantile along a profile line with
#' \code{\link{plot_profile}}.
#'
#' The following functions are used by the functions above:
#' \code{\link{bootstrap_marking_data}} performs the actual bootstrapping of
#' the marking data. \code{\link{get_bootstrap_parameters}} extracs the
#' parameters of the bootstrapped data sets from the mark_recapture_object as a
#' data frame.
#'
#' The profile line is created using \code{\link{profile_of_parameter}},
#' \code{\link{raster_param}}, \code{\link{wrap_profile_of_param}},
#' \code{\link{profile_line}} and \code{\link{profile_points}}.
#'
#' \code{\link{CONSURE}} needs projected data for some functions. Therefore,
#' the data will be projected from longitude/latitude (EPSG:4326) to Mollweide
#' projection (ESRI:54009), unless other projections are specified.
#' \code{\link{project_mark_recapture}}, \code{\link{project_df}} ,
#' \code{\link{project_window}} perform the projection.
#'
#'
#'
#'
#'
#' @docType package
#' @name CONSURE
NULL
#> NULL
