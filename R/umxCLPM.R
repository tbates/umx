#' Runs cross-lagged panel models 
#'
#' @description
#' One way of assessing causal relationships is by introducing time into the analyses. 
#' `umxCLPM` implements three cross-lagged panel models (CLPM) from the literature. 
#' The first is the classic CLPM from Heise (1969), the second is the CLPM from Hamaker et al. (2015), and
#' the third is the CLPM from STARTS (1995).
#' You simply pass the number of waves and the data set along with the model you wish to run.
#' 
#' Sketch mode is available; if you pass column names to data, a model object is returned for manipulation later. 
#' 
#' @param waves Number of waves of data.
#' @param name The name of the model (defaults to "Heise1969", "Hamaker2015", "STARTS1995" or "IV_RI_CLPM").
#' @param model Model type ("Hamaker2015", "Heise1969", "STARTS1995", or "IV_RI_CLPM").
#' @param data Data frame for the analysis.
#' @param counts Optional vector of count data columns.
#' @param summary Logical indicating whether to show a summary (default: TRUE if silent is not set).
#' @param autoRun Logical indicating whether to run the model (default to getOption("umx_auto_run")).
#' @param tryHard Method for fitting the model ("no", "yes", "ordinal", "search").
#' @param verbose Logical to control verbose output (default: FALSE).
#' @param batteries A character vector of pre-processing options ("scale", "ordinaloptim", "thresholds").
#' @param std Logical indicating whether to standardize the output (default: FALSE).
#' @param ivs Optional vector of instrumental variable column names.
#' @param defn Optional definition variable.
#' @param defto Optional variable to which to define.
#' @param type The method for handling missing data ("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS").
#' @param allContinuousMethod Method for handling continuous data ("cumulants", "marginals").
#' 
#' @return An OpenMx model object.
#' @export
#' @family CLPM Functions
#' @references 
#' Kenny, D.A., & Zautra, A. (1995). The trait-state-error model for multiwave data. *Journal of Consulting and Clinical Psychology*, **63**, 52–59. \doi{10.1037/0022-006X.63.1.52}
#' Hamaker E.L., Kuiper R.M., & Grasman R. (2015). A critique of the cross-lagged panel model. *Psychological Methods*, **20**, 102–116. \doi{10.1037/a0038889}
#' Heise D. R. (1970). Causal inference from panel data. *Sociological Methodology*, 2, 3–27. \doi{10.2307/270780}
#' @md
#' @examples
#' \dontrun{
#' 
#' # ================
#' # = 1. Load Data =
#' # ================
#' data(docData)
#' dt <- docData[2:9]
#'
#' # ============================
#' # = 2. Make a CLPM model     =
#' # ============================
#' hamaker <- umxCLPM(waves = 4, name = "mymodel", model = "Hamaker2015", data = dt)
#' }

umxCLPM <- function(data = NULL,waves, name = NULL, model = c("Hamaker2015", "Heise1969", "STARTS1995", "IV_RI_CLPM"),  counts = NULL,summary = !umx_set_silent(silent = TRUE), autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), verbose = FALSE, batteries = c("scale", "ordinaloptim"), std = FALSE, ivs = NULL, defn = NULL, defto = NULL, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"),  allContinuousMethod = c("cumulants", "marginals") ) {

		if (!(model %in% c("Hamaker2015", "Heise1969", "STARTS1995", "IV_RI_CLPM"))) stop("Model must be one of Hamaker2015, Heise1969, IV_RI_CLPM, or STARTS1995")

			sketch = FALSE
		xs = sprintf("X%03d", 1:waves)
		ys = sprintf("Y%03d", 1:waves)
		ps = sprintf("p%03d", 1:waves)
		qs = sprintf("q%03d", 1:waves)
		us = sprintf("u%03d", 1:waves)
		vs = sprintf("v%03d", 1:waves)

		if(missing(data)){
			data = c(xs, ys)
			mean_names = data 
			sketch = TRUE
		} else {
			if (missing(defn)) {
				colnames(data) = c(ivs, xs, ys)
				mean_names = c(ivs, xs, ys)
			} else {
				colnames(data) = c(ivs,xs, ys, defn)
				mean_names = c(ivs, xs, ys)
			}
		}

		if (!missing(counts)) {
			tryHard = "yes"
			if (all(c("X","Y") %in% counts)){
				counts_names = c(xs,ys)
				if (verbose) message("Both X and Y are count data")
			}   else if ("Y" %in% counts){
				counts_names = ys
				if (verbose) message("Y are count data")
			} else if ("X" %in% counts) {
				counts_names = xs
				if (verbose) message("X are count data")
			} 

		max_counts = sapply(data[,counts_names], max, na.rm = TRUE)
		mu_counts = sapply(data[,counts_names], function(x) sum(x > 0, na.rm = TRUE))
		zi_counts = sapply(data[,counts_names], function(x) mean(x == 0, na.rm = TRUE))  
		size_counts = sapply(data[,counts_names], function(x) sd(x == 0, na.rm = TRUE))  

		mxMNB = mxMarginalNegativeBinomial( # here you specify the distribution
			vars = counts_names, 
			maxCount = max_counts,
			size = size_counts, 
			mu = mu_counts,
			zeroInf = zi_counts)
	} 

	# Process first half of the columns
	if ("scale" %in% batteries && !sketch) {
				# Exclude columns defined in defn
		cols_to_process <- setdiff(colnames(data), c(ivs,defn))

		first_half <- cols_to_process[1:(length(cols_to_process)/2)]
		if (all(sapply(data[, first_half], is.numeric))) {
			data <- umx_scale_wide_twin_data(data=data, varsToScale = gsub(".$", "", first_half),sep="", twins = 1:waves)
			if (verbose) message("Scaling X data")
		} 
			 # Process second half of the columns
	second_half <- cols_to_process[((length(cols_to_process)/2) + 1):length(cols_to_process)]
	if (all(sapply(data[, second_half], is.numeric))) {
	 data <- umx_scale_wide_twin_data(data=data,varsToScale = gsub(".$", "", second_half), sep="", twins = 1:waves)
	 if (verbose) message("Scaling Y data")
 }
}

if ("dump" %in% batteries) return(data)

	if ("ordinaloptim" %in% batteries && !sketch) {
		# Exclude columns defined in defn
		cols_to_process <- setdiff(colnames(data), c(ivs,defn))

		first_half <- cols_to_process[1:(length(cols_to_process)/2)]
		if (all(sapply(data[, first_half], is.factor))) {
			data[, first_half] <- xmu_relevel_factors(data[, first_half], cols = first_half, min = 7)
			if (missing(tryHard)) tryHard = "ordinal"
			if (verbose) message("Releveling X data. Levels: ", length(levels(data[,xs[1]])))
		}

			 # Process second half of the columns
	second_half <- cols_to_process[((length(cols_to_process)/2) + 1):length(cols_to_process)]
	if (all(sapply(data[, second_half], is.factor))) {
	 data[, second_half] <- xmu_relevel_factors(data[, second_half], cols = second_half, min = 8)
	 if (missing(tryHard)) tryHard = "ordinal"
	 if (verbose) message("Releveling Y data. Levels: ", length(levels(data[,ys[1]])))
 }
}

if ("dump" %in% batteries) return(data)

	if (model == "Heise1969") {
		if (missing(name)) name = model 

		m1 = umxRAM(name,
			data = data,
			autoRun = FALSE,
			type = type,
			allContinuousMethod = allContinuousMethod,
											# WITHIN MODEL
			umxPath(ps, xs, fixedAt = 1),
			umxPath(var = ps),# labels = "us" ),
			umxPath(var = qs),# labels = "vs" ), 
			umxPath(qs, ys, fixedAt = 1),
											# CAUSAL
			umxPath(ps[1:waves - 1], qs[2:waves], values = 0.05),
			umxPath(qs[1:waves - 1], ps[2:waves], values = 0.05),
											# AR
			umxPath(ps[1:waves - 1], ps[2:waves], values = 1, lbound = 0.00001),
			umxPath(qs[1:waves - 1], qs[2:waves], values = 1, lbound = 0.00001),
											# CORRELATIONS
			umxPath(ps, with = qs, values = 0.02),
											# MEANS
			umxPath(means = mean_names)
		)

		if (!missing(counts)) m1 = mxModel(m1, mxMNB)

	}  else if (model == "Hamaker2015") {

		if (waves < 3) stop("Hamaker2015 requires at least 3 waves")
			paths = c(
				# RANDOM INTERCEPTS
				umxPath("xir", xs, fixedAt = 1),
				umxPath("yir", ys, fixedAt = 1),
				# WITHIN MODEL
				umxPath(ps, xs, fixedAt = 1),
				umxPath(us[2:waves], ps[2:waves], fixedAt=1),
				umxPath(vs[2:waves], qs[2:waves], fixedAt=1),
				umxPath(qs, ys, fixedAt = 1),
				# CAUSAL
				umxPath(ps[1:waves - 1], qs[2:waves], values = 0.2),
				umxPath(qs[1:waves-1], ps[2:waves], values = 0.05),
				# AR
				umxPath(ps[1:waves - 1], ps[2:waves], values = 1, lbound = 0.00001),
				umxPath(qs[1:waves - 1], qs[2:waves], values = 1, lbound = 0.00001),
				# CORRELATIONS
				umxPath(us[2:waves], with = vs[2:waves], values = 0.02),
				umxPath(ps[1], with = qs[1], values = 0.08),
				umxPath("xir", with = "yir", values = 0.08),
				# UNCORRELATED VARIANCES
				umxPath(var = c("xir", "yir"), values = 1),
				umxPath(var = c(ps[1], qs[1]), values = 1),
				umxPath(var= vs[2:waves] , values = 1),
				umxPath(var= us [2:waves], values = 1),
				# MEANS
				umxPath(means = mean_names)
			)

		if (!missing(defn)) {
			paths <- c(paths,
			 umxPath("one", paste0("def_",defn), fixedAt = 1, labels = paste0("data.", defn)),
			 umxPath(fromEach = paste0("def_", defn), to = defto, values = 0.1))
		}

		m1 = umxRAM(name,
			paths,
			data = data,
			autoRun = FALSE,
			type = type,
			allContinuousMethod = allContinuousMethod)


		if (!missing(counts)) m1 = mxModel(m1, mxMNB)
	} else if (model == "STARTS1995") {

		if (waves < 4) stop("STARTS1995 requires at least 4 waves")

			paths <- c(
											# RANDOM INTERCEPTS
				umxPath("xir", xs, fixedAt = 1),
				umxPath("yir", ys, fixedAt = 1),
											# WITHIN MODEL
				umxPath(ps, xs, fixedAt = 1),
				umxPath(us[2:waves], ps[2:waves], fixedAt=1),
				umxPath(vs[2:waves], qs[2:waves], fixedAt=1),
				umxPath(qs, ys, fixedAt = 1),
											# CAUSAL
				umxPath(ps[1:waves - 1], qs[2:waves], values = 0.05),
				umxPath(qs[1:waves-1], ps[2:waves], values = 0.05),
											# AR
				umxPath(ps[1:waves - 1], ps[2:waves], values = .1, lbound = 0.00001),
				umxPath(qs[1:waves - 1], qs[2:waves], values = .1, lbound = 0.00001),
											# CORRELATIONS
				umxPath(us[2:waves], with = vs[2:waves], values = 0.02),
				umxPath(ps[1], with = qs[1], values = 0.02),
				umxPath("xir", with = "yir", values = 0.02),
											# MEANS
				umxPath(means = mean_names),
											# UNCORRELATED VARIANCES
				umxPath(var = xs, values = 0.5),
				umxPath(var = ys, values = 0.5),
				umxPath(var = c("xir", "yir"), values = 1),
				umxPath(var = c(ps[1], qs[1]), values = 1)
			)

		if (!missing(defn)) {
			paths <- c(paths,
			 umxPath("one", paste0("def_",defn), fixedAt = 1, labels = paste0("data.", defn)),
			 umxPath(fromEach = paste0("def_", defn), to = defto, values = 0.1))
		}

		m1 = umxRAM(name,
			paths,
			data = data,
			autoRun = FALSE,
			type = type,
			allContinuousMethod = allContinuousMethod)

		if (!missing(counts)) m1 = mxModel(m1, mxMNB)

	}   else if (model == "IV_RI_CLPM") {

		paths <- list(
						# RANDOM INTERCEPTS
			umxPath("xir", xs, fixedAt = 1),
			umxPath("yir", ys, fixedAt = 1),
						# INSTRUMENTS
			umxPath("PSx", ps, values = 0.05), #labels = "psx"
			umxPath("PSy", qs, values = 0.05), #labels = "psy"
						# WITHIN MODEL
			umxPath(ps, xs, fixedAt = 1),
			umxPath(us[2:waves], ps[2:waves], fixedAt=1),
			umxPath(vs[2:waves], qs[2:waves], fixedAt=1),
			umxPath(qs, ys, fixedAt = 1),
						# IMMEDIATE
			umxPath(ps[2:waves], qs[2:waves], values = 0.05), # labels = "immedx"),
			umxPath(qs[2:waves], ps[2:waves], values = 0.05), # labels = "immedy"),
						# CAUSAL
			umxPath(ps[1:waves - 1], qs[2:waves], values = 0.05),
			umxPath(qs[1:waves - 1], ps[2:waves], values = 0.05),
						# AR
			umxPath(ps[1:waves - 1], ps[2:waves], values = 0.05, lbound = 0.00001), #, labels = "aux"),
			umxPath(qs[1:waves - 1], qs[2:waves], values = 0.05, lbound = 0.00001),#, labels = "auy"),
						# CORRELATIONS
			umxPath(us[2:waves], with = vs[2:waves], values = 0.02, labels = "uv"),
			umxPath(ps[1], with = qs[1], values = 0.5),
			umxPath("xir", with = "yir", values = 0.5),
						# MEANS
			umxPath(means = mean_names),
						# UNCORRELATED VARIANCES
			umxPath(var = c("xir", "yir"), values = 1),
			umxPath(var =  vs[2:waves], values = 1,  labels = "vs"),
			umxPath(var =  us[2:waves], values = 1,  labels = "us"),
			umxPath(v1m0= c("PSx", "PSy")),
			umxPath("PSx", with = "PSy", values = 0.5),
			umxPath(var = c(ps[1], qs[1]), values = 1)
		)

		if (!missing(defn)) {
			paths <- c(paths,
			 umxPath("one", paste0("def_",defn), fixedAt = 1, labels = paste0("data.", defn)),
			 umxPath(fromEach = paste0("def_", defn), to = defto, values = 0.1))
		}

					# Create a RAM model with the vectors
		m1 <- umxRAM("IV_RI_CLPM",
		 data = data,
		 type = type,
		 allContinuousMethod = allContinuousMethod,
		 autoRun = FALSE,
		 paths)

		if ("yes" %in% tryHard ) m1 <- mxOption(m1, 'mvnRelEps', mxOption(m1, 'mvnRelEps')/5)

			if (!missing(counts)) m1 = mxModel(m1, mxMNB)
		} 

	if ("thresholds" %in% batteries) {
		cols_to_process <- setdiff(colnames(data), c(ivs,defn))

		first_half <- cols_to_process[1:(length(cols_to_process)/2)]
		second_half <- cols_to_process[((length(cols_to_process)/2) + 1):length(cols_to_process)]

					# if first half is ordinal, then set thresholds with the equate_threshold_values function below
		if (all(sapply(data[,first_half], is.factor))) {
			m1 = xmu_equate_threshold_values(m1, x_cols = colnames(data[,first_half]))
			if (verbose) message("Equate thresholds for X data")
		}
					# now for the second half
	if (all(sapply(data[,second_half], is.factor))) {
		m1 = xmu_equate_threshold_values(m1, x_cols = colnames(data[,second_half]))
		if (verbose) message("Equate thresholds for Y data")
	}
}

if (!sketch) m1  = xmu_safe_run_summary(m1, autoRun = autoRun,  summary = summary, tryHard =  tryHard, std = std)

	return(m1)
}

#' Relabel Factor Columns in a Data Frame
#'
#' @description
#' This function modifies the levels of specified factor columns in a data.frame where the specified factor columns 
#' have potentially collapsed levels based on the criteria provided.
#' 
#' Levels that make up less than a specified proportion of total observations are collapsed into the previous level,
#' providing that a minimum number of levels remains. 
#' 
#' The levels of the remaining factor columns are synchronized with the updated levels of the first specified column.
#' Variables named in `cols` must be factors. Note too that prop uses e.g., .1 to stand for 10 percent.
#'
#' @param df A data frame containing the factor columns to be modified.
#' @param cols A character vector specifying the names of the factor columns to relabel.
#' @param prop A numeric value indicating the minimum proportion of observations for a level (default = .1)
#' @param min Integer bounding the minimum remaining number of levels (Default 8).
#' 
#' @family xmu internal not for end user
#' @return data.frame with the same structure as the input
#' 
#' @export
#' @examples 
#' df = data.frame(
#'     group = factor(c("A", "B", "B", "C", "D", "E", "E", "E")),
#'     score = c(10, 15, 15, 20, 25, 30, 30, 30)
#' )
#' 
#' # Relabel factor columns
#' df_releveled = xmu_relevel_factors(df, cols = c("group"), prop = 0.2, min=2)
#' df_releveled
#'
xmu_relevel_factors <- function(df, cols, prop = .1, min = 8) {

		# Check if all specified columns are factors
	if (!all(sapply(df[cols], is.factor))) {
		stop("All specified columns must be factors.")
	}

		# Check if the number of levels is greater than or equal to min
	first_col_levels_count <- length(levels(df[[cols[1]]]))
	if (first_col_levels_count < min) {
		message("Number of levels in the first column is less than ", min, ". No changes made.")
		return(df)
	}

		# Check if all columns have the same number of levels
	if (length(unique(sapply(df[cols], function(x) length(levels(x))))) > 1) {
		stop("All columns must have the same number of levels")
	}

		# Get the levels of the first column
	first_col_levels <- levels(df[[cols[1]]])

		# Calculate the percentage of total observations for each level
	lev_perc <- table(df[[cols[1]]]) / length(df[[cols[1]]])

		# Get the levels in order
	ordered_levels <- levels(df[[cols[1]]])

		# Initialize a new vector to hold the collapsed levels
	new_vector <- df[[cols[1]]]

		# Loop over the levels
	for (i in length(ordered_levels):2) {
		lev <- ordered_levels[i]
				# If the level makes up less than prop of the observations
		if (lev_perc[lev] < prop) {
						# Find the previous level
			prev_lev <- ordered_levels[i - 1]

						# Collapse the level into the previous level
			new_vector[new_vector == lev] <- prev_lev
		}

				# Ensure there are at least `min` levels left
		if (length(unique(new_vector)) <= min) break
	}

		# Relevel the factor with the original order
	df[[cols[1]]] <- factor(new_vector, levels = ordered_levels)

		# Remove empty levels
	df[[cols[1]]] <- droplevels(df[[cols[1]]])

		# Apply the same levels to the remaining columns
	for (col in cols[-1]) {
		if (is.factor(df[[col]])) {
			df[[col]] <- factor(df[[col]], levels = levels(df[[cols[1]]]))
		}
	}

	return(df)
}


#' Equate Threshold Values Across Columns in a Model
#'
#' @description
#' This function sets the threshold values for multiple columns in a model to be equal to the threshold values of the first specified column. 
#' It is useful in contexts where consistent threshold values are needed across different variables for statistical modeling.
#'
#' @param model A model object that contains threshold values in its `deviations_for_thresh` slot. 
#' @param x_cols A character vector specifying the names of the columns whose thresholds will be equated.
#' 
#' @return The modified model object with equated threshold values across the specified columns.
#' @export
#' @examples
#' \dontrun{
#' # Assumes `my_model` is a previously defined threshold model
#' # and has columns "var1", "var2", and "var3" in deviations_for_thresh$values
#' updated_model = xmu_equate_threshold_values(my_model, x_cols = c("var1", "var2", "var3"))
#' }
#' 
xmu_equate_threshold_values <-  function(model, x_cols) {
	# Set the thresholds for the first wave
	first_wave <- model$deviations_for_thresh$values[, x_cols[1]]
	# Apply the thresholds to all specified columns
	model$deviations_for_thresh$values[, x_cols] <- first_wave
	return(model)
}

