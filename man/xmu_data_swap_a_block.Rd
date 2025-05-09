% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xmu.R
\name{xmu_data_swap_a_block}
\alias{xmu_data_swap_a_block}
\title{Data helper function to swap blocks of data from one set of columns to another.}
\usage{
xmu_data_swap_a_block(theData, rowSelector, T1Names, T2Names)
}
\arguments{
\item{theData}{A data frame to swap within.}

\item{rowSelector}{Rows to swap between first and second set of columns.}

\item{T1Names}{The first set of columns.}

\item{T2Names}{The second set of columns.}
}
\value{
\itemize{
\item dataframe
}
}
\description{
Swap a block of rows of a dataset between two sets of variables (typically twin 1 and twin 2)
}
\examples{
test = data.frame(
a = paste0("a", 1:10),
b = paste0("b", 1:10),
c = paste0("c", 1:10),
d = paste0("d", 1:10), stringsAsFactors = FALSE)
xmu_data_swap_a_block(test, rowSelector = c(1,2,3,6), T1Names = "b", T2Names = "c")
xmu_data_swap_a_block(test, rowSelector = c(1,2,3,6), T1Names = c("a","c"), T2Names = c("b","d"))

}
\seealso{
\itemize{
\item \code{\link[=subset]{subset()}}
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
\code{\link{xmu_check_needs_means}()},
\code{\link{xmu_check_variance}()},
\code{\link{xmu_clean_label}()},
\code{\link{xmu_data_missing}()},
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
