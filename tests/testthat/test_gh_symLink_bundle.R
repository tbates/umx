library(testthat)
library(umx)

test_that("gh_symLink_bundle handles existing destinations", {
	skip_on_os("windows")
	base = tempfile("ghlink")
	dir.create(file.path(base, "src"), recursive = TRUE)
	dir.create(file.path(base, "dest"), recursive = TRUE)
	on.exit(unlink(base, recursive = TRUE), add = TRUE)
	writeLines("x", file.path(base, "src", "f.txt"))
	src = file.path(base, "src")
	dest = file.path(base, "dest")
	# fresh link then the already-exists branch (message, no error)
	gh_symLink_bundle(which = "f.txt", localPath = src, destination = dest)
	expect_message(gh_symLink_bundle(which = "f.txt", localPath = src, destination = dest), "already correctly configured")
	# symlink pointing elsewhere is refused
	unlink(file.path(dest, "f.txt"))
	writeLines("z", file.path(base, "src", "other.txt"))
	file.symlink(file.path(base, "src", "other.txt"), file.path(dest, "f.txt"))
	expect_error(gh_symLink_bundle(which = "f.txt", localPath = src, destination = dest), "points elsewhere")
	# non-symlink occupant is refused
	unlink(file.path(dest, "f.txt"))
	writeLines("y", file.path(dest, "f.txt"))
	expect_error(gh_symLink_bundle(which = "f.txt", localPath = src, destination = dest), "non-symlink")
	# missing source is refused
	expect_error(gh_symLink_bundle(which = "nope.txt", localPath = src, destination = dest), "does not exist")
})
