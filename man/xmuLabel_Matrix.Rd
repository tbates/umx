% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xmu.R
\name{xmuLabel_Matrix}
\alias{xmuLabel_Matrix}
\title{xmuLabel_Matrix (not a user function)}
\usage{
xmuLabel_Matrix(
  mx_matrix = NA,
  baseName = NA,
  setfree = FALSE,
  drop = 0,
  jiggle = NA,
  boundDiag = NA,
  suffix = "",
  verbose = TRUE,
  labelFixedCells = FALSE,
  overRideExisting = FALSE
)
}
\arguments{
\item{mx_matrix}{an mxMatrix}

\item{baseName}{A base name for the labels NA}

\item{setfree}{Whether to set free cells FALSE}

\item{drop}{What values to drop 0}

\item{jiggle}{= whether to jiggle start values}

\item{boundDiag}{set diagonal element lbounds to this numeric value (default = NA = ignore)}

\item{suffix}{a string to append to each label}

\item{verbose}{how much feedback to give}

\item{labelFixedCells}{= FALSE}

\item{overRideExisting}{Whether to overRideExisting (Default FALSE)}
}
\value{
\itemize{
\item The labeled \code{\link[OpenMx:mxMatrix]{OpenMx::mxMatrix()}}
}
}
\description{
This function will label all the free parameters in an \code{\link[OpenMx:mxMatrix]{OpenMx::mxMatrix()}}
}
\details{
Model developers should just call \code{\link[=xmuLabel]{xmuLabel()}}

Purpose: label the cells of an mxMatrix
Detail: Defaults to the handy "name_r1c1" where name is the matrix name, and r1c1 = row 1 col 1.
Use case: You should not use this: call xmuLabel
umx:::xmuLabel_Matrix(mxMatrix("Lower", 3, 3, values = 1, name = "a", byrow = TRUE), jiggle = .05, boundDiag = NA);
umx:::xmuLabel_Matrix(mxMatrix("Full" , 3, 3, values = 1, name = "a", byrow = TRUE));
umx:::xmuLabel_Matrix(mxMatrix("Symm" , 3, 3, values = 1, name = "a", byrow = TRUE), jiggle = .05, boundDiag = NA);
umx:::xmuLabel_Matrix(mxMatrix("Full" , 1, 1, values = 1, name = "a", labels= "data.a"));
umx:::xmuLabel_Matrix(mxMatrix("Full" , 1, 1, values = 1, name = "a", labels= "data.a"), overRideExisting = TRUE);
umx:::xmuLabel_Matrix(mxMatrix("Full" , 1, 1, values = 1, name = "a", labels= "test"), overRideExisting = TRUE);
See also: fit2 = omxSetParameters(fit1, labels = "a_r1c1", free = FALSE, value = 0, name = "drop_a_row1_c1")
}
\seealso{
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
\code{\link{xmu_check_needs_means}()},
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
