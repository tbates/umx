context("umxMeasurementInvariance_Functions")

test_that("umxMeasurementInvariance works", {
	require(umx)
	data("HS.ability.data", package = "OpenMx")
	df = umx_scale(HS.ability.data[, c("visual", "cubes", "flags", "school")])
	model = "spatial =~ visual + cubes + flags"
	
	# Run measurement invariance sequence with default compare = "both"
	expect_error(res <- umxMeasurementInvariance(model, data = df, group = "school", silent = TRUE), NA)
	
	# Verify that the returned table contains rows for each model level
	expect_s3_class(res, "data.frame")
	expect_true("Configural" %in% res$Model)
	expect_true("Metric" %in% res$Model)
	expect_true("Scalar" %in% res$Model)
	expect_true("Strict" %in% res$Model)
	
	# Verify sequential comparisons structure
	# Metric compared to Configural
	expect_equal(res$`Compare with Model`[res$Model == "Metric"], "Configural")
	# Scalar compared to Metric
	expect_equal(res$`Compare with Model`[res$Model == "Scalar"], "Metric")
	# Strict compared to Scalar
	expect_equal(res$`Compare with Model`[res$Model == "Strict"], "Scalar")
	
	# Verify models attribute is attached
	models = attr(res, "models")
	expect_length(models, 4)
	expect_named(models, c("Configural", "Metric", "Scalar", "Strict"))
	
	# Verify configural attribute is attached
	config_attr = attr(res, "configural")
	expect_s3_class(config_attr, "data.frame")
	# All in config_attr should be compared to Configural
	expect_equal(config_attr$`Compare with Model`[config_attr$Model == "Metric"], "Configural")
	expect_equal(config_attr$`Compare with Model`[config_attr$Model == "Scalar"], "Configural")
	expect_equal(config_attr$`Compare with Model`[config_attr$Model == "Strict"], "Configural")

	# Test compare = "sequential"
	expect_error(res_seq <- umxMeasurementInvariance(model, data = df, group = "school", compare = "sequential", silent = TRUE), NA)
	expect_s3_class(res_seq, "data.frame")
	expect_null(attr(res_seq, "configural"))
	expect_equal(res_seq$`Compare with Model`[res_seq$Model == "Scalar"], "Metric")

	# Test compare = "configural"
	expect_error(res_conf <- umxMeasurementInvariance(model, data = df, group = "school", compare = "configural", silent = TRUE), NA)
	expect_s3_class(res_conf, "data.frame")
	expect_null(attr(res_conf, "configural"))
	expect_equal(res_conf$`Compare with Model`[res_conf$Model == "Scalar"], "Configural")
})
