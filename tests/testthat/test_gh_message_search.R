test_that("gh_message_search resolves a commit id and still greps messages", {
	repo = tempfile("ghmsg")
	dir.create(repo)
	on.exit(unlink(repo, recursive = TRUE), add = TRUE)
	writeLines("a", file.path(repo, "f.txt"))
	expect_equal(system2("git", c("-C", repo, "init", "-q", "-b", "main")), 0L)
	expect_equal(system2("git", c("-C", repo, "add", "f.txt")), 0L)
	expect_equal(system2("git", c("-C", repo, "-c", "user.email=gh@example.com", "-c", "user.name=gh", "commit", "-q", "-m", "hello_subject")), 0L)
	full = system2("git", c("-C", repo, "rev-parse", "HEAD"), stdout = TRUE)
	expect_equal(length(full), 1L)
	short = substr(full, 1L, 8L)

	hits = gh_message_search(short, repo = repo)
	expect_equal(nrow(hits), 1L)
	expect_equal(hits$n, 1L)
	expect_equal(hits$sha, full)
	expect_equal(hits$subject, "hello_subject")
	expect_true(startsWith(hits$sha, hits$short))
	expect_true(is.na(hits$url))

	upperHits = gh_message_search(toupper(short), repo = repo)
	expect_equal(upperHits$sha, full)

	messageHits = gh_message_search("hello", repo = repo)
	expect_equal(messageHits$sha, full)
	expect_equal(messageHits$subject, "hello_subject")

	fake = "0123456789abcdef0123456789abcdef01234567"
	expect_false(identical(fake, full))
	writeLines("b", file.path(repo, "g.txt"))
	expect_equal(system2("git", c("-C", repo, "add", "g.txt")), 0L)
	expect_equal(system2("git", c("-C", repo, "-c", "user.email=gh@example.com", "-c", "user.name=gh", "commit", "-q", "-m", paste0("mentions_", fake))), 0L)
	mentioned = gh_message_search(fake, repo = repo)
	expect_equal(nrow(mentioned), 1L)
	expect_equal(mentioned$subject, paste0("mentions_", fake))

	missingId = paste(rep("a", 40), collapse = "")
	if (identical(missingId, full) || identical(missingId, mentioned$sha)) {
		missingId = paste(rep("b", 40), collapse = "")
	}
	missingHits = expect_message(gh_message_search(missingId, repo = repo), "no commits match")
	expect_equal(nrow(missingHits), 0L)

	star = "* IMPROVED: `gh_open_app_support` working"
	expect_equal(system2("git", c("-C", repo, "-c", "user.email=gh@example.com", "-c", "user.name=gh", "commit", "-q", "--allow-empty", "-m", shQuote(star))), 0L)
	starHits = gh_message_search(star, repo = repo)
	expect_equal(nrow(starHits), 1L)
	expect_equal(starHits$subject, star)

	fixedDot = expect_message(gh_message_search("hello.subject", repo = repo), "no commits match")
	expect_equal(nrow(fixedDot), 0L)
	regexHits = gh_message_search("hello.subject", repo = repo, regex = TRUE)
	expect_true(any(regexHits$subject == "hello_subject"))
})
