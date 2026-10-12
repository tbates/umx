#' Easily plot functions in R
#'
#' @description
#' A wrapper for [ggplot2::stat_function()] that handles single or multiple functions.
#'
#' @details Easily plot functions-like sin or x^2-using ggplot. Accepts bare functions, 
#' strings, or lists of strings/functions. Automatically generates a legend when 
#' multiple functions are provided.
#'
#' Strings may use arbitrary variable names: the sole right-hand-side symbol is
#' plotted on the horizontal axis, e.g. `"100 - Q"` plots against `Q`. An equation
#' left-hand side names the vertical axis, e.g. `"P = 100 - Q"` labels the y axis `P`.
#' When the right-hand side has several symbols and no `x`, name the horizontal
#' variable with `x`, e.g. `umxPlotFun("P = a*Q + b", x = "Q")`; remaining symbols
#' must have values where the plot is drawn.
#' Bind leftover symbols to values as arguments, e.g.
#' `umxPlotFun("alice = bob^2+c", x = "bob", c = 5)`.
#'
#' @param fun Function(s) to plot. Takes strings like c("3 + sin(x)^2", "cos(x)"), equations like "P = 100 - Q", or function objects.
#' @param min x-range min.
#' @param max x-range max.
#' @param xlab Optional x axis label.
#' @param ylab Optional y axis label.
#' @param title Optional title for the plot.
#' @param logX Set to "log" or "log10" to transform x coordinate.
#' @param logY Set to "log" or "log10" to transform y coordinate.
#' @param p Optional plot onto which to draw the function(s).
#' @param x Optional name of the horizontal-axis variable when a string has several right-hand-side symbols and no `x`, e.g. `x = "Q"`.
#' @param ... Named single-number values for remaining right-hand-side symbols, e.g. `c = 5`. Names matching no plotted symbol warn.
#' @return A ggplot graph object
#' @export
#' @family Plotting functions
#' @seealso [ggplot2::stat_function()]
#' @examples
#' \dontrun{
#' # Plotting multiple strings
#' p = umxPlotFun(c("sin(x)", "cos(x)"), max = 2*pi)
#' 
#' # Providing a named list to control legend labels
#' umxPlotFun(list(Sine = sin, Cosine = cos), max = 2*pi)
#'
#' # Arbitrary symbols: Q plots on x, P labels y
#' umxPlotFun("P = 100 - Q")
#'
#' # Several right-hand-side symbols: name the x variable
#' a = 2; b = 10
#' umxPlotFun("P = a*Q + b", x = "Q")
#'
#' # Binding leftover symbols to values
#' umxPlotFun("alice = bob^2+c", x = "bob", c = 5)
#' }
#'
umxPlotFun <- function(fun = c("sin(x)", "cos(x)"), min = -1, max = 5, xlab = NULL, ylab = NULL, title = NULL, logY = c("no", "log", "log10"), logX = c("no", "log", "log10"), p = NULL, x = NULL, ...) {
    logY = xmu_match.arg(logY, c("no", "log", "log10"), check = FALSE)
    logX = xmu_match.arg(logX, c("no", "log", "log10"), check = FALSE)

    if (!is.null(x)) {
        if (!is.character(x) || length(x) != 1L || is.na(x) || trimws(x) == "") {
            stop("umxPlotFun: 'x' must be one variable name as a string, e.g. x = 'Q'.")
        }
        x = trimws(x)
    }

    dotVals = list(...)
    if (length(dotVals) >= 1L) {
        if (is.null(names(dotVals))) {
            stop("umxPlotFun: bindings in '...' must be named single numbers, e.g. c = 5.")
        }
        for (i in seq_along(dotVals)) {
            if (is.na(names(dotVals)[i]) || names(dotVals)[i] == "" || !is.numeric(dotVals[[i]]) || length(dotVals[[i]]) != 1L || is.na(dotVals[[i]])) {
                stop("umxPlotFun: bindings in '...' must be named single numbers, e.g. c = 5.")
            }
        }
    }

    if (inherits(fun, "numeric")) {
        stop("If you write a function symbolically, you need to put it in quotes, e.g. 'x^2'")
    }

    # 1. Normalize 'fun' into a standard named list of functions
    funcList = list()
    funcNames = names(fun)
    if (is.null(funcNames)) { funcNames = character(length(fun)) }
    singleXVar = ""
    singleLhsVar = ""
    allRhsVars = character()

    if (is.character(fun)) {
        for (i in seq_along(fun)) {
            if (funcNames[i] == "") funcNames[i] = fun[i]
            rawString = fun[i]
            splitEq = strsplit(rawString, "=", fixed = TRUE)[[1]]
            lhsVar = ""
            rhsString = trimws(rawString)
            if (length(splitEq) >= 2L && grepl("^([A-Za-z]|[.][A-Za-z])[A-Za-z0-9._]*$", trimws(splitEq[1]))) {
                lhsVar = trimws(splitEq[1])
                rhsString = trimws(paste(splitEq[-1], collapse = "="))
            }
            if (rhsString == "") {
                stop(paste0("umxPlotFun: '", rawString, "' has nothing to plot after '='."))
            }
            rhsVars = all.vars(parse(text = rhsString))
            xVar = "x"
            evalString = rhsString
            if (!("x" %in% rhsVars) && length(rhsVars) >= 1L) {
                if (length(rhsVars) == 1L) {
                    xVar = rhsVars
                    if (!is.null(x) && x != xVar) {
                        stop(paste0("umxPlotFun: x = '", x, "' is not in '", rawString, "'. The variable there is '", xVar, "'."))
                    }
                } else {
                    if (is.null(x) || !(x %in% rhsVars)) {
                        stop(paste0("umxPlotFun: '", rawString, "' contains several variables (", paste(rhsVars, collapse = ", "), ") and no 'x', so I can't tell which goes on the horizontal axis. Either write the horizontal variable as 'x', or name it with x, e.g. umxPlotFun('P = a*Q + b', x = 'Q'). Any other symbols must have values where the plot is drawn."))
                    }
                    xVar = x
                }
                evalString = paste(deparse(do.call("substitute", list(parse(text = rhsString)[[1]], setNames(list(quote(x)), xVar)))), collapse = "\n")
            }
            if ("x" %in% rhsVars) {
                allRhsVars = union(allRhsVars, rhsVars)
            } else if (length(rhsVars) >= 1L) {
                allRhsVars = union(allRhsVars, setdiff(rhsVars, xVar))
            }
            if (length(fun) == 1L) {
                singleLhsVar = lhsVar
                if (xVar != "x") {
                    singleXVar = xVar
                }
            }
            thisFun = function(x) {}
            body(thisFun) = parse(text = evalString)
            funcList[[i]] = thisFun
        }
    } else if (is.function(fun)) {
        funcList[[1]] = fun
        if (!is.null(body(fun))) {
            allRhsVars = union(allRhsVars, all.vars(parse(text = paste(deparse(body(fun)), collapse = "\n"))[[1]]))
        }
        if (funcNames[1] == "") funcNames[1] = "Function"
    } else if (is.list(fun)) {
        for (i in seq_along(fun)) {
            if (is.character(fun[[i]])) {
                if (funcNames[i] == "") funcNames[i] = fun[[i]]
                rawString = fun[[i]]
                splitEq = strsplit(rawString, "=", fixed = TRUE)[[1]]
                lhsVar = ""
                rhsString = trimws(rawString)
                if (length(splitEq) >= 2L && grepl("^([A-Za-z]|[.][A-Za-z])[A-Za-z0-9._]*$", trimws(splitEq[1]))) {
                    lhsVar = trimws(splitEq[1])
                    rhsString = trimws(paste(splitEq[-1], collapse = "="))
                }
                if (rhsString == "") {
                    stop(paste0("umxPlotFun: '", rawString, "' has nothing to plot after '='."))
                }
                rhsVars = all.vars(parse(text = rhsString))
                xVar = "x"
                evalString = rhsString
                if (!("x" %in% rhsVars) && length(rhsVars) >= 1L) {
                    if (length(rhsVars) == 1L) {
                        xVar = rhsVars
                        if (!is.null(x) && x != xVar) {
                            stop(paste0("umxPlotFun: x = '", x, "' is not in '", rawString, "'. The variable there is '", xVar, "'."))
                        }
                    } else {
                        if (is.null(x) || !(x %in% rhsVars)) {
                            stop(paste0("umxPlotFun: '", rawString, "' contains several variables (", paste(rhsVars, collapse = ", "), ") and no 'x', so I can't tell which goes on the horizontal axis. Either write the horizontal variable as 'x', or name it with x, e.g. umxPlotFun('P = a*Q + b', x = 'Q'). Any other symbols must have values where the plot is drawn."))
                        }
                        xVar = x
                    }
                    evalString = paste(deparse(do.call("substitute", list(parse(text = rhsString)[[1]], setNames(list(quote(x)), xVar)))), collapse = "\n")
                }
                if ("x" %in% rhsVars) {
                    allRhsVars = union(allRhsVars, rhsVars)
                } else if (length(rhsVars) >= 1L) {
                    allRhsVars = union(allRhsVars, setdiff(rhsVars, xVar))
                }
                if (length(fun) == 1L) {
                    singleLhsVar = lhsVar
                    if (xVar != "x") {
                        singleXVar = xVar
                    }
                }
                thisFun = function(x) {}
                body(thisFun) = parse(text = evalString)
                funcList[[i]] = thisFun
            } else if (is.function(fun[[i]])) {
                funcList[[i]] = fun[[i]]
                if (!is.null(body(fun[[i]]))) {
                    allRhsVars = union(allRhsVars, all.vars(parse(text = paste(deparse(body(fun[[i]])), collapse = "\n"))[[1]]))
                }
                if (funcNames[i] == "") funcNames[i] = paste("Function", i)
            }
        }
    } else {
        stop("fun must be a function, string, or list/vector of these.")
    }
    names(funcList) = funcNames

    bindEnv = NULL
    if (length(dotVals) >= 1L) {
        if (length(setdiff(names(dotVals), allRhsVars)) >= 1L) {
            warning(paste0("umxPlotFun: binding(s) not used by any plotted string: ", paste(setdiff(names(dotVals), allRhsVars), collapse = ", "), "."))
        }
        bindEnv = list2env(dotVals, parent = environment())
        for (i in seq_along(funcList)) {
            environment(funcList[[i]]) = bindEnv
        }
    }

    # 2. Setup the base plot canvas
    if (is.null(p)) {
        p = ggplot2::ggplot(data.frame(x = c(min, max)), ggplot2::aes(x = x))
        if (logY != "no") {
            p = p + ggplot2::coord_trans(y = logY)
        }
        if (logX != "no") {
            p = p + ggplot2::coord_trans(x = logX)
        }
    }
    
    # 3. Layer the functions
    for (i in seq_along(funcList)) {
        thisFun = funcList[[i]]
        thisName = names(funcList)[i]
        
        # Inject the evaluated name into the aesthetic mapping to build the legend
        if (is.na(max) && !is.null(p)) {
            p = p + ggplot2::stat_function(fun = thisFun, ggplot2::aes(color = !!thisName))
        } else {
            p = p + ggplot2::stat_function(fun = thisFun, xlim = c(min, max), ggplot2::aes(color = !!thisName))
        }
    }
    
    # 4. Handle Labels and Theming
    if (is.null(xlab) && length(funcList) == 1L && singleXVar != "") {
        xlab = singleXVar
    } else {
        xlab = ifelse(!is.null(xlab), xlab, "X value")
    }

    if (is.null(ylab)) {
        if (length(funcList) == 1L && singleLhsVar != "") {
            ylab = singleLhsVar
        } else {
            ylab = ifelse(length(funcList) == 1, paste0(names(funcList)[1], " of x"), "Functions of X")
        }
    }
    
    if (is.null(title)) {
        pref = ifelse(length(funcList) == 1, "Plot of function: ", "Plot of Functions: ")
        # Truncate title if there are too many functions to display cleanly
        if (length(funcList) > 3) {
            title = paste0(pref, paste(names(funcList)[1:3], collapse = ", "), ", ...")
        } else {
            title = paste0(pref, paste(names(funcList), collapse = ", "))
        }
    }
    
    # Apply labels; set the legend title for 'color'
    p = p + ggplot2::labs(x = xlab, y = ylab, title = title, color = "Legend")
    p = p + cowplot::theme_cowplot(font_family = "Times", font_size = 12)
    
    # Remove legend entirely if only one function is plotted (cleaner UI)
    if (length(funcList) == 1) {
        p = p + ggplot2::theme(legend.position = "none")
    }
    
    p
}

