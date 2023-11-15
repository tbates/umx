# library(testthat)
# library(umx)
# test_package("umx")
# test_active_file("~/bin/umx/tests/testthat/test_umx_DOC_etc.r") 

test_that("testing umxDiffMZ", {
	require(umx)
	data(twinData)
	df = umx_scale_wide_twin_data(varsToScale= c("ht", "wt"), sep = "", data= twinData, twins = 1:2)
	df$xDiff = df[, "ht1"] - df[, "ht2"]
	df$yDiff = df[, "wt1"] - df[, "wt2"]
	p = umxDiffMZ(x="ht", y="wt", labxy = c(x=-.4, y=1), data = subset(df, abs(yDiff - xDiff) < .09), sep = "")
	p = p + labs(
		# title = "MZ twin intra-pair differences",
		x     = "Difference in exposure to rain (T1 - T2)",
		y     = "Difference in mud (T1 - T2)"
	)
	p = p + annotate("text", x=  .4, y = -.35, label = "Twin 1 goes out in the rain more,\nand has more mud on his shoes", family="Times", size=4)
	p = p + geom_curve(data = data.frame(x1 = .5, y1 = -.22, x2 = .85, y2 = .85), aes(x = x1, y = y1, xend = x2, yend = y2), arrow = arrow(length = unit(0.4, "cm")) )
	p = p + annotate("text", x= -.5, y =  .50, label = "Twin 1 seldom walks in rain,\nand has less mud on his shoes", family="Times", size=4)
	p = p + geom_curve(data = data.frame(x1 = -.75, y1 = .35, x2 = -.55, y2 = -.65), aes(x = x1, y = y1, xend = x2, yend = y2), arrow = arrow(length = unit(0.4, "cm")) )
	print(p)
})
