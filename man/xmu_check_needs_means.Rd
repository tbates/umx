% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xmu.R
\name{xmu_check_needs_means}
\alias{xmu_check_needs_means}
\title{Check data to see if model needs means.}
\usage{
xmu_check_needs_means(
  data,
  type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"),
  allContinuousMethod = c("cumulants", "marginals")
)
}
\arguments{
\item{data}{\code{\link[OpenMx:mxData]{OpenMx::mxData()}} to check.}

\item{type}{of the data requested by the model.}

\item{allContinuousMethod}{How data will be processed if used for WLS.}
}
\value{
\itemize{
\item T/F
}
}
\description{
Check data to see if model needs means.
}
\examples{
xmu_check_needs_means(mtcars, type = "Auto")
xmu_check_needs_means(mtcars, type = "FIML")
# xmu_check_needs_means(mtcars, type = "cov")
# xmu_check_needs_means(mtcars, type = "cor")

# TRUE - marginals means means
xmu_check_needs_means(mtcars, type = "WLS", allContinuousMethod= "marginals")
xmu_check_needs_means(mtcars, type = "ULS", allContinuousMethod= "marginals")
xmu_check_needs_means(mtcars, type = "DWLS", allContinuousMethod= "marginals")

# ================================
# = Provided as an mxData object =
# ================================
tmp = mxData(mtcars, type="raw")
xmu_check_needs_means(tmp, type = "FIML") # TRUE
xmu_check_needs_means(tmp, type = "ULS", allContinuousMethod= "cumulants") #FALSE
# TRUE - means with marginals
xmu_check_needs_means(tmp, type = "WLS", allContinuousMethod= "marginals")
tmp = mxData(cov(mtcars), type="cov", numObs= 100)
# Should catch this can't be type FIML
xmu_check_needs_means(tmp) # FALSE
tmp = mxData(cov(mtcars), means = umx_means(mtcars), type="cov", numObs= 100)
xmu_check_needs_means(tmp) # TRUE

# =======================
# = One var is a factor =
# =======================
tmp = mtcars
tmp$cyl = factor(tmp$cyl)
xmu_check_needs_means(tmp, allContinuousMethod= "cumulants") # TRUE
xmu_check_needs_means(tmp, allContinuousMethod= "marginals") # TRUE - always has means
}
\seealso{
\itemize{
\item \code{\link[=xmu_make_mxData]{xmu_make_mxData()}}
}

Other xmu internal not for end user: 
\code{\link{umxModel}()},
\code{\link{umxRenameMatrix}()},
\code{\link{umx_APA_pval}()},
\code{\link{umx_fun_mean_sd}()},
\code{\link{umx_get_bracket_addresses}()},
\code{\link{umx_make}()},
\code{\link{umx_standardize}()},
\code{\link{umx_string_to_algebra}()},
\code{\link{xmuHasSquareBrackets}()},
\code{\link{xmuLabel_MATRIX_Model}()},
\code{\link{xmuLabel_Matrix}()},
\code{\link{xmuLabel_RAM_Model}()},
\code{\link{xmuMI}()},
\code{\link{xmuMakeDeviationThresholdsMatrices}()},
\code{\link{xmuMakeOneHeadedPathsFromPathList}()},
\code{\link{xmuMakeTwoHeadedPathsFromPathList}()},
\code{\link{xmuMaxLevels}()},
\code{\link{xmuMinLevels}()},
\code{\link{xmuPropagateLabels}()},
\code{\link{xmuRAM2Ordinal}()},
\code{\link{xmuTwinSuper_Continuous}()},
\code{\link{xmuTwinSuper_NoBinary}()},
\code{\link{xmuTwinUpgradeMeansToCovariateModel}()},
\code{\link{xmu_CI_merge}()},
\code{\link{xmu_CI_stash}()},
\code{\link{xmu_DF_to_mxData_TypeCov}()},
\code{\link{xmu_PadAndPruneForDefVars}()},
\code{\link{xmu_bracket_address2rclabel}()},
\code{\link{xmu_cell_is_on}()},
\code{\link{xmu_check_levels_identical}()},
\code{\link{xmu_check_variance}()},
\code{\link{xmu_clean_label}()},
\code{\link{xmu_data_missing}()},
\code{\link{xmu_data_swap_a_block}()},
\code{\link{xmu_describe_data_WLS}()},
\code{\link{xmu_dot_make_paths}()},
\code{\link{xmu_dot_make_residuals}()},
\code{\link{xmu_dot_maker}()},
\code{\link{xmu_dot_move_ranks}()},
\code{\link{xmu_dot_rank_str}()},
\code{\link{xmu_extract_column}()},
\code{\link{xmu_get_CI}()},
\code{\link{xmu_lavaan_process_group}()},
\code{\link{xmu_make_TwinSuperModel}()},
\code{\link{xmu_make_bin_cont_pair_data}()},
\code{\link{xmu_make_mxData}()},
\code{\link{xmu_match.arg}()},
\code{\link{xmu_name_from_lavaan_str}()},
\code{\link{xmu_path2twin}()},
\code{\link{xmu_path_regex}()},
\code{\link{xmu_print_algebras}()},
\code{\link{xmu_rclabel_2_bracket_address}()},
\code{\link{xmu_relevel_factors}()},
\code{\link{xmu_safe_run_summary}()},
\code{\link{xmu_set_sep_from_suffix}()},
\code{\link{xmu_show_fit_or_comparison}()},
\code{\link{xmu_simplex_corner}()},
\code{\link{xmu_standardize_ACE}()},
\code{\link{xmu_standardize_ACEcov}()},
\code{\link{xmu_standardize_ACEv}()},
\code{\link{xmu_standardize_CP}()},
\code{\link{xmu_standardize_IP}()},
\code{\link{xmu_standardize_RAM}()},
\code{\link{xmu_standardize_SexLim}()},
\code{\link{xmu_standardize_Simplex}()},
\code{\link{xmu_start_value_list}()},
\code{\link{xmu_starts}()},
\code{\link{xmu_summary_RAM_group_parameters}()},
\code{\link{xmu_twin_add_WeightMatrices}()},
\code{\link{xmu_twin_check}()},
\code{\link{xmu_twin_get_var_names}()},
\code{\link{xmu_twin_make_def_means_mats_and_alg}()},
\code{\link{xmu_twin_upgrade_selDvs2SelVars}()},
\code{\link{xmu_update_covar}()}
}
\concept{xmu internal not for end user}
