# ==============
# = Deprecated =
# ==============

umxTryHard = function(model, n=3, calc_SE=F){ stop("Use umxRun() in place of umxTryHard") }

umxLabels = function(from=NA, to=NA, connect="single", prefix="", suffix="") {stop("please use umxPath in place of umxLabels. To label models or matrices, use umxLabel")}

genEpi_Jiggle = function(matrixIn, mean = 0, sd = .1, dontTouch = 0) {stop("please use umxJiggle in place of genEpi_Jiggle")}
