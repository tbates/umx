if (Sys.getenv("USER") == "timothybates") {
	# Code to run only on the user's machine
	library("testthat")
	library("umx")
	test_check("umx")
} else {
	# Code to run during CRAN testing
	print("Tests not run during CRAN testing.")
}
