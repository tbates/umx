#' Run a model, choosing mxRun or mxTryHard variants automatically (experimental)
#'
#' @description
#' **Experimental**: the interface and defaults may change.
#'
#' `umx_run` picks the run function to suit the model: [OpenMx::mxTryHardOrdinal()] for
#' models with thresholds, [OpenMx::mxTryHard()] for models with definition variables,
#' and [OpenMx::mxRun()] otherwise. If the run returns status code 6 (non-zero gradient),
#' it retries once with [OpenMx::mxTryHardOrdinal()].
#'
#' @details
#' Run settings:
#' * `"auto"` (default): thresholds -> `mxTryHardOrdinal`; else definition variables -> `mxTryHard`; else `mxRun`.
#' * `"mxRun"`, `"mxTryHard"`, `"mxTryHardOrdinal"`: use that function.
#' * `"off"`: always `mxRun`.
#'
#' A model that has already been run is not re-run, but a status 6 result still triggers the retry.
#' Warnings from the optimizer are reported as messages, and the run time, fit, and status are reported.
#'
#' @param model An [OpenMx::mxModel()] to run.
#' @param ... Further arguments passed to the run function.
#' @param silent Passed to the run function to quiet the backend (default TRUE).
#' @param intervals Whether to compute CIs (default TRUE).
#' @param smart Which run function to use: "auto" (default), "mxRun", "mxTryHard", "mxTryHardOrdinal", or "off".
#' @return The run [OpenMx::mxModel()].
#' @export
#' @family Advanced Model Building Functions
#' @seealso - [umxRun()], [OpenMx::mxRun()], [OpenMx::mxTryHard()], [OpenMx::mxTryHardOrdinal()]
#' @examples
#' \dontrun{
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("One Factor", data = demoOneFactor, autoRun = FALSE,
#' 	umxPath("G", to = manifests),
#' 	umxPath(v.m. = manifests),
#' 	umxPath(v1m0 = "G")
#' )
#' m1 = umx_run(m1)
#' m1 = umx_run(m1, smart = "mxTryHard")
#' }
umx_run <- function(model, ..., silent = TRUE, intervals = TRUE, smart = c("auto", "mxRun", "mxTryHard", "mxTryHardOrdinal", "off")) {
	smart = match.arg(smart)
	mdl = as(model, "MxModel")
	status = mdl$output$status$code

	# Decide which run function to use
	chosen = "mxRun"
	if(smart == "auto") {
		hasOrd = tryCatch(imxHasThresholds(mdl, submodels = TRUE), error = function(e) FALSE)
		hasDef = tryCatch(imxHasDefinitionVariable(mdl), error = function(e) FALSE)
		chosen = if(isTRUE(hasOrd)) "mxTryHardOrdinal" else if(isTRUE(hasDef)) "mxTryHard" else "mxRun"
	} else if(smart != "off") {
		chosen = smart
	}

	# Report warnings as messages
	reportWarning = function(w) {
		message(conditionMessage(w))
		invokeRestart("muffleWarning")
	}

	if(is.null(status)) {
		message(sprintf("Running %s with %s ...", mdl@name, chosen))
		t0 = Sys.time()
		mdl = withCallingHandlers({
			if(chosen == "mxTryHard") {
				mxTryHard(mdl, ..., silent = silent, intervals = intervals)
			} else if(chosen == "mxTryHardOrdinal") {
				mxTryHardOrdinal(mdl, ..., silent = silent, intervals = intervals)
			} else {
				mxRun(mdl, ..., silent = silent, intervals = intervals)
			}
		}, warning = reportWarning)
		dt = as.numeric(difftime(Sys.time(), t0, units = "secs"))
		status = mdl@output$status$code
		message(sprintf("Done in %.2fs | LL=%.6g | status=%s", dt, mdl@output$fit, status))
	}

	# Status code 6 (non-zero gradient): retry once with mxTryHardOrdinal
	if(isTRUE(!is.na(status)) && status == 6L) {
		message("Status code 6 detected (non-zero gradient). Retrying with mxTryHardOrdinal() ...")
		t1 = Sys.time()
		mdl = withCallingHandlers(
			mxTryHardOrdinal(mdl, ..., silent = silent, intervals = intervals),
			warning = reportWarning
		)
		dt = as.numeric(difftime(Sys.time(), t1, units = "secs"))
		status = mdl@output$status$code
		message(sprintf("Retry finished in %.2fs | LL=%.6g | status=%s", dt, mdl@output$fit, status))
	}
	mdl
}
