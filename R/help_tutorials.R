#' Easily open vignettes (tutorials) for a package
#'
#' @description
#' Present the user with a list of available vignettes (tutorials) for a package,
#' prompt them to select one by number, and open the selected vignette.
#'
#' @param package The name of the package. Can be a character string or an unquoted name (default = "umx").
#' @return None
#' @export
#' @family Utility Functions
#' @seealso - [vignette()]
#' @examples
#' \dontrun{
#' # Open a tutorial from umx
#' umx_tutorials()
#' 
#' # Open a tutorial from another package, e.g. ggplot2
#' umx_tutorials(ggplot2)
#' }
umx_tutorials <- function(package = "umx") {
	if (missing(package)) {
		pkg_name <- "umx"
	} else {
		pkg_name <- tryCatch({
			val <- eval(substitute(package), parent.frame())
			if (is.character(val)) val else deparse(substitute(package))
		}, error = function(e) {
			deparse(substitute(package))
		})
	}

	# Get the list of vignettes
	vinst <- utils::vignette(package = pkg_name)
	
	if (nrow(vinst$results) == 0) {
		message("No vignettes (tutorials) found for package '", pkg_name, "'.")
		return(invisible(NULL))
	}
	
	# Present menu
	choices <- vinst$results[, "Title"]
	choice <- utils::menu(choices, title = paste0("\nTutorials in '", pkg_name, "':"))
	
	if (choice > 0) {
		# Open selected vignette
		topic <- vinst$results[choice, "Item"]
		message("Opening vignette '", topic, "'...")
		utils::vignette(topic, package = pkg_name)
	} else {
		message("No tutorial selected.")
	}
	return(invisible(NULL))
}


#' xmu_parse_completion_context
#'
#' Parse the current console line buffer to extract the function name,
#' argument name, and argument value prefix under completion.
#'
#' @param line The current command line buffer string.
#' @param end The integer position of the cursor in the line buffer.
#' @return A list containing the parsed context (`funName`, `argName`, `valPrefix`, `hasQuote`, `quoteChar`), or `NULL` if not in an argument value completion context.
xmu_parse_completion_context <- function(line, end) {
	utils_ns = asNamespace("utils")
	
	inside_quotes = utils_ns$isInsideQuotes()
	
	if (inside_quotes) {
		fullToken = utils_ns$.guessTokenFromLine(update = FALSE)
		clean_line = substr(line, 1L, fullToken$start - 1L)
		val_prefix = fullToken$token
		has_quote = TRUE
		quote_char = substr(line, fullToken$start, fullToken$start)
	} else {
		clean_line = substr(line, 1L, end)
		val_prefix = utils_ns$.CompletionEnv[["token"]]
		has_quote = FALSE
		quote_char = NULL
	}
	
	# walk backwards to find matching open parenthesis '('
	nesting = 0
	open_paren_idx = NULL
	
	n_clean = nchar(clean_line)
	if (n_clean < 1) return(NULL)
	
	for (i in seq(n_clean, 1, by = -1)) {
		char = substr(clean_line, i, i)
		if (char == ")") {
			nesting = nesting + 1
		} else if (char == "(") {
			if (nesting == 0) {
				open_paren_idx = i
				break
			} else {
				nesting = nesting - 1
			}
		}
	}
	
	if (is.null(open_paren_idx)) return(NULL)
	
	prefix_part = substr(clean_line, 1, open_paren_idx - 1)
	fun_match = regexpr("[a-zA-Z0-9_.:]+$", prefix_part)
	if (fun_match > 0) {
		fun_name = substr(prefix_part, fun_match, nchar(prefix_part))
	} else {
		return(NULL)
	}
	
	inside = substr(clean_line, open_paren_idx + 1, n_clean)
	
	# Walk backwards to find the last '='
	eq_idx = NULL
	nesting = 0
	if (nchar(inside) > 0) {
		for (i in seq(nchar(inside), 1, by = -1)) {
			char = substr(inside, i, i)
			if (char == ")") {
				nesting = nesting + 1
			} else if (char == "(") {
				nesting = nesting - 1
			} else if (char == "=") {
				if (nesting == 0) {
					eq_idx = i
					break
				}
			} else if (char == ",") {
				if (nesting == 0) {
					break
				}
			}
		}
	}
	
	if (is.null(eq_idx)) return(NULL)
	
	arg_part = substr(inside, 1, eq_idx - 1)
	arg_match = regexpr("[a-zA-Z0-9_.]+$", trimws(arg_part))
	if (arg_match > 0) {
		arg_name = substr(trimws(arg_part), arg_match, nchar(trimws(arg_part)))
	} else {
		return(NULL)
	}
	
	return(list(
		funName = fun_name,
		argName = arg_name,
		valPrefix = val_prefix,
		hasQuote = has_quote,
		quoteChar = quote_char
	))
}

#' Fallback to utils internal token completer (no triple-colon in package code)
#' @keywords internal
xmu_utils_completeToken <- function(custom = FALSE) {
	fun = get0(".completeToken", envir = asNamespace("utils"), inherits = FALSE, ifnotfound = NULL)
	if (is.function(fun)) {
		return(fun(custom = custom))
	}
	character(0)
}

#' xmu_custom_completer
#'
#' A custom autocompletion function for R console. It intercepts the tab-completion
#' process and suggests default values/options (from `c(...)` or logical vectors)
#' for function arguments when completing an argument value.
#'
#' @param env The completion environment (utils internal CompletionEnv).
#' @return NULL (called for its side effects of modifying `env[["comps"]]`).
xmu_custom_completer <- function(env) {
	line = env[["linebuffer"]]
	end = env[["end"]]
	
	ctx = xmu_parse_completion_context(line, end)
	
	# We also need to get the old_completer to fall back if needed
	old_completer = utils::rc.getOption("custom.completer")
	
	# Check if we should fall back
	# Note: prevent infinite recursion if old_completer points to this function
	is_recursive = !is.null(old_completer) && (
		identical(old_completer, xmu_custom_completer) || 
		identical(old_completer, get("xmu_custom_completer", envir = asNamespace("umx")))
	)
	
	if (is.null(ctx)) {
		if (is.function(old_completer) && !is_recursive) {
			return(old_completer(env))
		} else {
			return(xmu_utils_completeToken())
		}
	}
	
	# Try to find function object
	funObj = tryCatch(eval(parse(text = ctx$funName), envir = .GlobalEnv), error = function(e) {
		parts = strsplit(ctx$funName, "::")[[1]]
		if (length(parts) == 2) {
			tryCatch(getExportedValue(parts[1], parts[2]), error = function(e) NULL)
		} else {
			tryCatch(get(ctx$funName, mode = "function"), error = function(e) NULL)
		}
	})
	
	if (!is.function(funObj)) {
		if (is.function(old_completer) && !is_recursive) {
			return(old_completer(env))
		} else {
			return(xmu_utils_completeToken())
		}
	}
	
	frms = formals(funObj)
	if (!(ctx$argName %in% names(frms))) {
		if (is.function(old_completer) && !is_recursive) {
			return(old_completer(env))
		} else {
			return(xmu_utils_completeToken())
		}
	}
	
	arg_val = frms[[ctx$argName]]
	options = NULL
	should_quote = FALSE
	
	if (is.call(arg_val) && as.character(arg_val[[1]]) == "c") {
		options = tryCatch(eval(arg_val), error = function(e) NULL)
		if (is.character(options)) should_quote = TRUE
	} else if (is.character(arg_val)) {
		options = arg_val
		should_quote = TRUE
	} else if (is.logical(arg_val)) {
		options = c("TRUE", "FALSE")
		should_quote = FALSE
	} else if (is.numeric(arg_val)) {
		options = arg_val
		should_quote = FALSE
	}
	
	if (is.null(options)) {
		if (is.function(old_completer) && !is_recursive) {
			return(old_completer(env))
		} else {
			return(xmu_utils_completeToken())
		}
	}
	
	options = as.character(options)
	matched = options[startsWith(options, ctx$valPrefix)]
	if (length(matched) == 0) {
		env[["comps"]] = character()
		return()
	}
	
	if (should_quote) {
		if (ctx$hasQuote) {
			formatted = paste0(matched, ctx$quoteChar)
		} else {
			formatted = paste0('"', matched, '"')
		}
	} else {
		formatted = matched
	}
	
	env[["comps"]] = formatted
}

#' umx_complete_dollar
#'
#' Modifies the macOS RGUI (R.app) to extend auto-completion.
#' It appends a "$" and attempts completion again when no stem matches are found.
#' Implemented by Simon Urbanek.
#'
#' @export
#' @family Miscellaneous Functions
umx_complete_dollar <- function() {
	# Register custom completer for RStudio and other general R consoles
	utils::rc.options(custom.completer = xmu_custom_completer)

	if (Sys.info()["sysname"] == "Darwin" && "tools:RGUI" %in% search()) {
		tryCatch({
			RGUI = as.environment("tools:RGUI")
			RGUI$rcompgen.completion <- function(x) {
				comp <- function(x) {
					utils_ns = asNamespace("utils")
					utils_ns$.assignLinebuffer(x)
					utils_ns$.assignEnd(nchar(x))
					utils_ns$.guessTokenFromLine()
					utils_ns$.completeToken()
					utils_ns$.CompletionEnv[["comps"]]
				}
				res <- unique(comp(x))
				if (nzchar(x) && identical(res, x) && !identical(substr(x, nchar(x), nchar(x)), "$")) {
					rc <- comp(paste0(x, "$"))
					if (!identical(substr(rc, nchar(rc), nchar(rc)), "$")) res <- rc
				}
				# If we have a unique function name, append "(" to place the cursor inside
				if (length(res) == 1 && is.character(res) && nzchar(res) && !grepl("\\($", res)) {
					is_func <- FALSE
					if (exists(res, mode = "function")) {
						is_func <- TRUE
					} else if (grepl("::", res)) {
						tryCatch({
							obj <- eval(parse(text = res))
							if (is.function(obj)) is_func <- TRUE
						}, error = function(e) {})
					}
					if (is_func) {
						res <- paste0(res, "(")
					}
				}
				res
			}
		}, error = function(e) {
			# Silent fallback if environment modification fails
		})
	}
}
