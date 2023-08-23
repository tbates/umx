#' Functions for Structural Equation Modeling in OpenMx
#'
#' @description
#' `umx` allows you to more easily build, run, modify, and report structural models, 
#' building on the OpenMx package.
#' All core functions are organized into families, so they are easier to find 
#' (so if you know a function similar to what you are looking for, look at other members of its "family" at the bottom of its help file.
#' 
#' Please cite as: Bates, T. C., Neale, M. C., & Maes, H. H. (2019). 
#' umx: A library for Structural Equation and Twin Modelling in R. 
#' *Twin Research and Human Genetics*, **22**, 27-41. \doi{10.1017/thg.2019.2}.
#'
#' All the functions have full-featured and well commented examples, some even have *figures*, 
#' so use the help, even if you think it won't help :-)
#' Have a look, for example at [umxRAM()]
#' 
#' Check out NEWS about new features at `news(package = "umx")`
#' 
#' @details
#' Introductory working examples are below. You can run all demos with demo(umx)
#' When I have a vignette, it will be: vignette("umx", package = "umx")
#' 
#' There is a helpful blog at <https://tbates.github.io>
#' 
#' (Only) if you want the bleeding-edge version:
#' 
#' devtools::install_github("tbates/umx")
#'
#' @name umx
#' @family Core Model Building Functions
#' @family Model Summary and Comparison
#' @family Reporting Functions
#' @family Super-easy helpers
#' @family Twin Modeling Functions
#' @family Twin Data functions
#' @family Miscellaneous Stats Functions
#' @family Teaching and testing Functions
#' @family Get and set
#' @family Check or test
#' @family Plotting functions
#' @family Data Functions
#' @family File Functions
#' @family String Functions
#' @family Miscellaneous Utility Functions
#' @family datasets
#' @family Advanced Model Building Functions
#' @family zAdvanced Helpers
#'
#' @references - Bates, T. C., Neale, M. C., & Maes, H. H. (2019). umx: A library for Structural 
#' Equation and Twin Modelling in R. *Twin Research and Human Genetics*, **22**, 27-41. \doi{10.1017/thg.2019.2}, 
#' <https://github.com/tbates/umx>, tutorial: <https://tbates.github.io>
#'
#' @md
#' @examples
#' \dontrun{
#' require("umx")
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("One Factor", data = demoOneFactor, type="cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G"  , fixedAt= 1)
#' )
#' 
#' # umx added informative labels, created starting values, 
#' # Ran your model (if autoRun is on), and displayed a brief summary
#' # including a comparison if you modified a model...!
#' 
#' # umxSummary generates journal-ready fit information.
#' # We can choose std=T for standardized parameters and can also
#' # filter out some types of parameter (e.g. means or residuals)
#'
#' umxSummary(m1, std = TRUE, residuals=FALSE)
#' 
#' # parameters() flexibly retrieves model coefficients. 
#' # For example just G-loadings greater than |.3| and rounded to 2-digits.
#' parameters(m1, thresh="above", b=.3, pattern = "G_to.*", digits = 2)
#'
#' # (The built-in coef works as for lm etc.)
#' coef(m1)
#' 
#' # ==================
#' # = Model updating =
#' # ==================
#' # umxModify modifies, renames, re-runs, and compares a model
#' # Can we set the loading of x1 on G to zero? (nope...)
#' m2 = umxModify(m1, "G_to_x1", name = "no_effect_of_g_on_X1", comparison = TRUE)
#'
#' # note1: umxSetParameters can do this with some additional flexibility
#' # note2 "comparison = TRUE" above is the same as calling 
#' # umxCompare, like this
#' umxCompare(m1, m2)
#' 
#' 
#' # ========================
#' # = Confidence intervals =
#' # ========================
#' 
#' # umxSummary() will show these, but you can also use the confint() function
#' confint(m1) # OpenMx's SE-based confidence intervals
#' 
#' 
#' # umxConfint formats everything you need nicely, and allows adding CIs (with parm=)
#' umxConfint(m1, parm = 'all', run = TRUE) # likelihood-based CIs
#' 
#' # And make a Figure and open in browser
#' plot(m1, std = TRUE)
#' 
#' # If you just want the .dot code returned set file = NA
#' plot(m1, std = TRUE, file = NA)
#' }
#'
'_PACKAGE'
NULL
