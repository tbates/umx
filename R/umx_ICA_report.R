# Editorial clocks for a Scholastica export of Intelligence & Cognitive Abilities.

#' Journal processing report for an ICA Scholastica export
#'
#' @description
#' Read a Scholastica CSV export and open an HTML report of editorial
#' efficiency. Journal time on each version runs from that version hitting the
#' desk to the decision on it. Time the author spends between a decision and
#' the next version is counted on its own. The page gives means and medians
#' for triage, review, the editor after the reviews are in, author revision,
#' and acceptance of a revision, with the ggplot figures embedded in the page.
#'
#' @param dataDir Directory of the Scholastica CSV export. A single string.
#'   Default is `~/bin/ica/ICAdata`. The files used are `manuscripts.csv`,
#'   `decisions.csv`, `reviewer-invitations.csv`, and `reviews.csv`.
#' @param asOf Date used to age manuscripts that are still in review or still
#'   with the author. A `Date`, or something [base::as.Date()] can parse. Default is
#'   [Sys.Date()].
#'
#' @details
#' ## Files and keys
#'
#' The export is a set of tables joined by id, one Scholastica submission
#' version per manuscript row.
#'
#' * `manuscripts.csv` has one row per version. `manuscript_id` is unique to
#'   that version. `previous_version_id` is the previous version's
#'   `manuscript_id` (empty on the first version). `submission_id` is shared by
#'   every version of the same paper, so it identifies the paper, but it is not
#'   reliably the first or the latest `manuscript_id`. `version_number` is 1,
#'   2, or 3. `created_at` is when that version hit the desk. `workflow_state`
#'   is the current state of that version. `decision_created_at` is when the
#'   decision on that version was made.
#' * `decisions.csv` has one row per decided version, joined on `manuscript_id`.
#'   `decision_type` is `accept`, `reject`, or `revise_and_resubmit`. Its
#'   `created_at` matches `manuscripts.csv` `decision_created_at`.
#' * `reviewer-invitations.csv` is joined on the version `manuscript_id`.
#'   `created_at` is when the invitation was sent. `accepted_at` is when the
#'   reviewer accepted. `workflow_state` is `submitted` (review filed),
#'   `accepted` (agreed, review not in), `declined`, `expired`, `revoked`, or
#'   `hard_bounced`. `manuscript_version_number` repeats the version.
#' * `reviews.csv` has no `manuscript_id`. It joins to an invitation through
#'   `reviewer_invitation_id`. `created_at` is when the review was filed.
#'   `publication_recommendation` is Accept, Revise and resubmit, or Reject.
#' * `authors.csv` joins on `manuscript_id`. `articles.csv` is the published
#'   article, joined on `manuscript_id`, with `published_at`. `activities.csv`
#'   is a prose log of the same events (submitted, invited, accepted an
#'   invitation, submitted a review, made a decision). The clocks below use the
#'   structured timestamps, not the prose log. `issues.csv` is journal issues.
#'   `posts.csv` is empty in the export this function was written against.
#'
#' Trial rows are dropped: `title` beginning with `[EXAMPLE]`, or `editor_tags`
#' containing `trial entries to ignore`.
#'
#' A paper is the chain walked from a version with no `previous_version_id`
#' along `previous_version_id`. Outcomes come from the latest version's
#' `workflow_state`: `ready_to_publish` accepted, `rejected` or `desk_rejected`
#' rejected, `revise_and_resubmit` with no later version still with the author,
#' `reviewers_confirmed` or `reviews_submitted` still in review, and
#' `withdrawn_before_decision` or `withdrawn_after_non_terminal_decision`
#' withdrawn.
#'
#' ## Seconds columns already on manuscripts.csv
#'
#' Scholastica also stores durations. They are not what the report plots,
#' because the total mixes the two clocks, and the editor buckets sometimes
#' overlap.
#'
#' * `time_to_decision_in_seconds` is journal time on that version only
#'   (`created_at` to `decision_created_at`). It does not include the author gap.
#' * `time_spent_with_editor_pre_review_in_seconds` is meant to be desk time
#'   before review. On many versions it matches the wait until the first
#'   reviewer accepts. On others it keeps running after reviews have arrived.
#' * `time_spent_with_editor_post_review_in_seconds` matches last review filed
#'   to the decision.
#' * On two versions, pre-review plus post-review exceeds time-to-decision, so
#'   those two buckets are not a partition.
#' * `time_spent_with_author_in_seconds` is stored on the new version and
#'   matches the gap from the previous decision to this version's `created_at`.
#' * `total_time_to_decision_across_versions_in_seconds` sums version
#'   time-to-decision and the author gaps. It is filled on the latest decided
#'   version. A paper reviewed in two days, held by the author for a year, and
#'   then accepted the next day has a total near a year.
#'
#' ## Clocks the report uses
#'
#' Timestamps are minute resolution (`2015-01-13 10:18PM` style). Durations are
#' in days.
#'
#' * Hits the desk: that version's `created_at`.
#' * Goes out to a reviewer: invitation `created_at`.
#' * Reviewer takes it on: invitation `accepted_at`.
#' * Review comes back: `reviews.csv` `created_at`.
#' * Decision: `decision_created_at`.
#' * Triage, until a reviewer has it: version `created_at` to the first
#'   `accepted_at`. Time until the first invitation is sent is reported beside it.
#' * Review window: first `accepted_at` to the last review filed on that version.
#' * Editor after the reviews are in: last review to the decision.
#' * Round-1 journal time: first version `created_at` to its decision. Desk
#'   rejects and editorial accepts with no invitation are in this total, and
#'   are also reported on their own.
#' * Author revision time: decision on version N to `created_at` of version N+1.
#'   Each return is one observation. An open revise-and-resubmit is aged to
#'   `asOf` and kept out of the completed mean.
#' * Time to accept a revision: `created_at` to the accept decision on a version
#'   whose `decision_type` is `accept` and whose `version_number` is greater
#'   than 1.
#' * Journal time for a finished paper: sum over its versions of
#'   (decision - arrived). Author gaps are not in that sum.
#' * Calendar time: first version `created_at` to the final decision. This is
#'   journal time plus author time.
#'
#' Versions that have not yet been decided are left out of the completed
#' journal means. Their elapsed time is listed under papers still open.
#'
#' @return Invisibly, a list. `htmlFile` is the temporary HTML page that was
#'   opened. `papers` is one row per paper. `versions` is one row per version.
#'   `authorGaps` is one row per completed author return. `reviewTurns` is one
#'   row per filed review. `revisionRounds` is one row per decided version
#'   after the first.
#' @export
#' @family Miscellaneous Utility Functions
#' @md
#' @examples
#' \dontrun{
#' umx_ICA_report()
#' umx_ICA_report(dataDir = "~/bin/ica/ICAdata")
#' }
umx_ICA_report <- function(dataDir = "~/bin/ica/ICAdata", asOf = Sys.Date()) {
	dataDir = path.expand(dataDir)
	asOf = as.Date(asOf)
	if (length(dataDir) != 1 || !dir.exists(dataDir)) {
		stop("dataDir is not a directory: ", dataDir)
	}
	asOfClock = as.POSIXct(paste(asOf, "00:00:00"), tz = "UTC")

	oldLocale = Sys.getlocale("LC_TIME")
	Sys.setlocale("LC_TIME", "C")
	on.exit(Sys.setlocale("LC_TIME", oldLocale), add = TRUE)

	parseUtc = function(x) {
		x = gsub("[[:space:]]+", " ", trimws(as.character(x)))
		x[x == "" | x == "NA"] = NA_character_
		as.POSIXct(x, format = "%Y-%m-%d %I:%M%p", tz = "UTC")
	}
	readExport = function(fileName) {
		path = file.path(dataDir, fileName)
		if (!file.exists(path)) {
			stop("Missing export file: ", path)
		}
		raw = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
		if (ncol(raw) == 0) {
			return(raw)
		}
		names(raw)[1] = sub("^\ufeff", "", names(raw)[1])
		names(raw) = gsub(" \\(in UTC\\)", "", names(raw))
		names(raw) = gsub("[^A-Za-z0-9]+", "_", names(raw))
		names(raw) = gsub("^_|_$", "", names(raw))
		raw
	}
	idChar = function(x) {
		out = rep(NA_character_, length(x))
		ok = !is.na(x) & as.character(x) != ""
		out[ok] = as.character(x[ok])
		out
	}
	daysBetween = function(a, b) {
		as.numeric(difftime(b, a, units = "days"))
	}
	firstTime = function(x) {
		x = x[!is.na(x)]
		if (length(x) == 0) {
			return(as.POSIXct(NA, tz = "UTC"))
		}
		min(x)
	}
	lastTime = function(x) {
		x = x[!is.na(x)]
		if (length(x) == 0) {
			return(as.POSIXct(NA, tz = "UTC"))
		}
		max(x)
	}
	fmt = function(x, digits = 1) {
		if (length(x) == 0 || all(is.na(x))) {
			return("-")
		}
		format(round(x, digits), nsmall = digits, trim = TRUE)
	}
	htmlEscape = function(x) {
		x = gsub("&", "&amp;", x, fixed = TRUE)
		x = gsub("<", "&lt;", x, fixed = TRUE)
		x = gsub(">", "&gt;", x, fixed = TRUE)
		x = gsub("\"", "&quot;", x, fixed = TRUE)
		x
	}
	statRow = function(label, x) {
		x = x[!is.na(x)]
		cells = if (length(x) == 0) {
			c("0", "-", "-", "-", "-")
		} else {
			qs = stats::quantile(x, c(0.25, 0.75))
			c(as.character(length(x)), fmt(mean(x)), fmt(stats::median(x)), fmt(qs[1]), fmt(qs[2]))
		}
		paste0("<tr><th>", htmlEscape(label), "</th>", paste0("<td>", cells, "</td>", collapse = ""), "</tr>")
	}
	statTable = function(rows) {
		paste0(
			"<table><thead><tr><th></th><th>n</th><th>Mean (days)</th><th>Median</th><th>25th</th><th>75th</th></tr></thead><tbody>",
			paste(rows, collapse = ""),
			"</tbody></table>"
		)
	}
	rawToBase64 = function(x) {
		alphabet = c(LETTERS, letters, as.character(0:9), "+", "/")
		n = length(x)
		pad = (3 - n %% 3) %% 3
		if (pad > 0) {
			x = c(x, raw(pad))
		}
		bytes = as.integer(x)
		b1 = bytes[seq(1, length(bytes), by = 3)]
		b2 = bytes[seq(2, length(bytes), by = 3)]
		b3 = bytes[seq(3, length(bytes), by = 3)]
		enc = paste0(
			alphabet[bitwShiftR(b1, 2) + 1L],
			alphabet[bitwOr(bitwShiftL(bitwAnd(b1, 3L), 4L), bitwShiftR(b2, 4L)) + 1L],
			alphabet[bitwOr(bitwShiftL(bitwAnd(b2, 15L), 2L), bitwShiftR(b3, 6L)) + 1L],
			alphabet[bitwAnd(b3, 63L) + 1L],
			collapse = ""
		)
		if (pad > 0) {
			substr(enc, nchar(enc) - pad + 1, nchar(enc)) = strrep("=", pad)
		}
		enc
	}
	plotUri = list()
	embedPlot = function(name, plot, width, height) {
		tmpPng = tempfile(fileext = ".png")
		ggplot2::ggsave(tmpPng, plot, width = width, height = height, dpi = 120, bg = "white")
		raw = readBin(tmpPng, "raw", n = file.info(tmpPng)$size)
		unlink(tmpPng)
		plotUri[[name]] <<- paste0("data:image/png;base64,", rawToBase64(raw))
	}
	shortTitle = function(title, n = 52) {
		title = gsub("[[:space:]]+", " ", title)
		ifelse(nchar(title) <= n, title, paste0(substr(title, 1, n - 1), "..."))
	}

	ms = readExport("manuscripts.csv")
	dec = readExport("decisions.csv")
	inv = readExport("reviewer-invitations.csv")
	rev = readExport("reviews.csv")

	trial = startsWith(ms$title, "[EXAMPLE]") | grepl("trial entries to ignore", ms$editor_tags, fixed = TRUE)
	ms = ms[!trial, , drop = FALSE]

	versions = data.frame(
		submissionId = idChar(ms$submission_id),
		manuscriptId = idChar(ms$manuscript_id),
		versionNumber = as.integer(ms$version_number),
		title = ms$title,
		previousVersionId = idChar(ms$previous_version_id),
		workflowState = ms$workflow_state,
		createdAt = parseUtc(ms$created_at),
		decidedAt = parseUtc(ms$decision_created_at),
		stringsAsFactors = FALSE
	)
	decisionType = rep(NA_character_, nrow(versions))
	decMid = idChar(dec$manuscript_id)
	decWhen = parseUtc(dec$created_at)
	for (i in seq_len(nrow(versions))) {
		hit = which(decMid == versions$manuscriptId[i])
		if (length(hit) == 0) {
			next
		}
		pick = hit[which.max(decWhen[hit])]
		decisionType[i] = dec$decision_type[pick]
	}
	versions$decisionType = decisionType

	invMid = idChar(inv$manuscript_id)
	invId = idChar(inv$id)
	invSent = parseUtc(inv$created_at)
	invAccepted = parseUtc(inv$accepted_at)
	revInvId = idChar(rev$reviewer_invitation_id)
	revFiled = parseUtc(rev$created_at)

	nInvite = integer(nrow(versions))
	nAccept = integer(nrow(versions))
	nReview = integer(nrow(versions))
	firstInviteAt = rep(as.POSIXct(NA, tz = "UTC"), nrow(versions))
	firstAcceptAt = rep(as.POSIXct(NA, tz = "UTC"), nrow(versions))
	lastReviewAt = rep(as.POSIXct(NA, tz = "UTC"), nrow(versions))
	for (i in seq_len(nrow(versions))) {
		hit = which(invMid == versions$manuscriptId[i])
		nInvite[i] = length(hit)
		if (length(hit) == 0) {
			next
		}
		firstInviteAt[i] = firstTime(invSent[hit])
		firstAcceptAt[i] = firstTime(invAccepted[hit])
		nAccept[i] = sum(!is.na(invAccepted[hit]))
		filed = revFiled[revInvId %in% invId[hit]]
		nReview[i] = length(filed)
		lastReviewAt[i] = lastTime(filed)
	}
	versions$nInvite = nInvite
	versions$nAccept = nAccept
	versions$nReview = nReview
	versions$firstInviteAt = firstInviteAt
	versions$firstAcceptAt = firstAcceptAt
	versions$lastReviewAt = lastReviewAt
	versions$journalDays = daysBetween(versions$createdAt, versions$decidedAt)
	versions$triageToInviteDays = daysBetween(versions$createdAt, versions$firstInviteAt)
	versions$triageToAcceptDays = daysBetween(versions$createdAt, versions$firstAcceptAt)
	versions$reviewWindowDays = daysBetween(versions$firstAcceptAt, versions$lastReviewAt)
	versions$editorAfterDays = daysBetween(versions$lastReviewAt, versions$decidedAt)

	reviewTurns = data.frame(
		invitationId = character(0),
		manuscriptId = character(0),
		versionNumber = integer(0),
		fromInviteDays = numeric(0),
		fromAcceptDays = numeric(0),
		stringsAsFactors = FALSE
	)
	for (i in seq_len(nrow(inv))) {
		filedHit = which(revInvId == invId[i])
		if (length(filedHit) == 0) {
			next
		}
		verHit = which(versions$manuscriptId == invMid[i])
		versionNumber = NA_integer_
		if (length(verHit) == 1) {
			versionNumber = versions$versionNumber[verHit]
		}
		reviewTurns = rbind(reviewTurns, data.frame(
			invitationId = invId[i],
			manuscriptId = invMid[i],
			versionNumber = versionNumber,
			fromInviteDays = daysBetween(invSent[i], revFiled[filedHit[1]]),
			fromAcceptDays = daysBetween(invAccepted[i], revFiled[filedHit[1]]),
			stringsAsFactors = FALSE
		))
	}

	childOf = list()
	for (i in seq_len(nrow(versions))) {
		parent = versions$previousVersionId[i]
		if (!is.na(parent)) {
			childOf[[parent]] = c(childOf[[parent]], versions$manuscriptId[i])
		}
	}
	for (parent in names(childOf)) {
		if (length(childOf[[parent]]) > 1) {
			stop("Version ", parent, " has more than one resubmission: ", paste(childOf[[parent]], collapse = ", "))
		}
	}

	rootRows = which(is.na(versions$previousVersionId))
	papers = data.frame(
		submissionId = character(0),
		rootId = character(0),
		latestId = character(0),
		title = character(0),
		nRounds = integer(0),
		outcome = character(0),
		workflowState = character(0),
		firstCreated = as.POSIXct(character(0), tz = "UTC"),
		lastDecided = as.POSIXct(character(0), tz = "UTC"),
		round1Journal = numeric(0),
		laterJournal = numeric(0),
		journalDays = numeric(0),
		authorDays = numeric(0),
		calendarDays = numeric(0),
		round1Reviewed = logical(0),
		stringsAsFactors = FALSE
	)
	authorGaps = data.frame(
		rootId = character(0),
		title = character(0),
		fromVersion = integer(0),
		days = numeric(0),
		decidedAt = as.POSIXct(character(0), tz = "UTC"),
		returnedAt = as.POSIXct(character(0), tz = "UTC"),
		stringsAsFactors = FALSE
	)
	revisionRounds = data.frame(
		manuscriptId = character(0),
		title = character(0),
		versionNumber = integer(0),
		decisionType = character(0),
		journalDays = numeric(0),
		nReview = integer(0),
		stringsAsFactors = FALSE
	)
	openRows = list()

	for (r in rootRows) {
		chain = versions$manuscriptId[r]
		guard = 0
		repeat {
			kids = childOf[[chain[length(chain)]]]
			if (is.null(kids)) {
				break
			}
			chain = c(chain, kids)
			guard = guard + 1
			if (guard > 20) {
				stop("Version chain did not end, starting at ", versions$manuscriptId[r])
			}
		}
		idx = match(chain, versions$manuscriptId)
		latest = versions[idx[length(idx)], ]
		state = latest$workflowState
		if (state == "ready_to_publish") {
			outcome = "accepted"
		} else if (state %in% c("rejected", "desk_rejected")) {
			outcome = "rejected"
		} else if (startsWith(state, "withdrawn")) {
			outcome = "withdrawn"
		} else if (state == "revise_and_resubmit") {
			outcome = "with_author"
		} else {
			outcome = "in_review"
		}

		round1 = versions[idx[1], ]
		round1Journal = round1$journalDays
		laterJournal = 0
		authorSum = 0
		if (length(idx) > 1) {
			later = versions$journalDays[idx[-1]]
			later = later[!is.na(later)]
			if (length(later) > 0) {
				laterJournal = sum(later)
			}
		}
		for (k in seq_len(length(idx) - 1)) {
			gap = daysBetween(versions$decidedAt[idx[k]], versions$createdAt[idx[k + 1]])
			authorSum = authorSum + gap
			authorGaps = rbind(authorGaps, data.frame(
				rootId = versions$manuscriptId[idx[1]],
				title = versions$title[idx[length(idx)]],
				fromVersion = versions$versionNumber[idx[k]],
				days = gap,
				decidedAt = versions$decidedAt[idx[k]],
				returnedAt = versions$createdAt[idx[k + 1]],
				stringsAsFactors = FALSE
			))
		}
		for (k in idx[-1]) {
			if (is.na(versions$journalDays[k])) {
				next
			}
			revisionRounds = rbind(revisionRounds, data.frame(
				manuscriptId = versions$manuscriptId[k],
				title = versions$title[k],
				versionNumber = versions$versionNumber[k],
				decisionType = versions$decisionType[k],
				journalDays = versions$journalDays[k],
				nReview = versions$nReview[k],
				stringsAsFactors = FALSE
			))
		}

		journalDays = NA_real_
		calendarDays = NA_real_
		decidedIdx = idx[!is.na(versions$decidedAt[idx])]
		if (outcome %in% c("accepted", "rejected") && length(decidedIdx) == length(idx)) {
			journalDays = sum(versions$journalDays[idx])
			calendarDays = daysBetween(versions$createdAt[idx[1]], versions$decidedAt[idx[length(idx)]])
		}

		papers = rbind(papers, data.frame(
			submissionId = versions$submissionId[idx[1]],
			rootId = versions$manuscriptId[idx[1]],
			latestId = latest$manuscriptId,
			title = latest$title,
			nRounds = length(idx),
			outcome = outcome,
			workflowState = state,
			firstCreated = versions$createdAt[idx[1]],
			lastDecided = if (is.na(latest$decidedAt)) as.POSIXct(NA, tz = "UTC") else latest$decidedAt,
			round1Journal = round1Journal,
			laterJournal = laterJournal,
			journalDays = journalDays,
			authorDays = if (outcome %in% c("accepted", "rejected")) authorSum else NA_real_,
			calendarDays = calendarDays,
			round1Reviewed = round1$nReview > 0,
			stringsAsFactors = FALSE
		))

		if (outcome == "in_review") {
			openRows[[length(openRows) + 1]] = data.frame(
				title = latest$title,
				workflowState = state,
				versionNumber = latest$versionNumber,
				elapsedDays = daysBetween(latest$createdAt, asOfClock),
				stringsAsFactors = FALSE
			)
		}
		if (outcome == "with_author") {
			openRows[[length(openRows) + 1]] = data.frame(
				title = latest$title,
				workflowState = "with author since revise and resubmit",
				versionNumber = latest$versionNumber,
				elapsedDays = daysBetween(latest$decidedAt, asOfClock),
				stringsAsFactors = FALSE
			)
		}
	}

	accepted = papers[papers$outcome == "accepted", , drop = FALSE]
	round1 = versions[is.na(versions$previousVersionId), , drop = FALSE]
	round1Decided = round1[!is.na(round1$journalDays), , drop = FALSE]
	round1Reviewed = round1[round1$nReview > 0, , drop = FALSE]
	deskOnly = round1Decided[round1Decided$nInvite == 0, , drop = FALSE]
	acceptRounds = revisionRounds[!is.na(revisionRounds$decisionType) & revisionRounds$decisionType == "accept", , drop = FALSE]

	# Figures. ggplot objects are built and saved. They are not printed.
	journalBlue = "#1B4F72"
	journalLight = "#5DADE2"
	authorGold = "#B9770E"

	if (nrow(accepted) > 0) {
		stack = accepted
		stack$label = shortTitle(stack$title)
		if (any(duplicated(stack$label))) {
			stack$label = paste0(stack$label, " [", stack$latestId, "]")
		}
		stack = stack[order(stack$calendarDays, stack$label), , drop = FALSE]
		long = data.frame(
			label = rep(stack$label, 3),
			piece = rep(c("Round 1 with the journal", "Later rounds with the journal", "With the author"), each = nrow(stack)),
			days = c(stack$round1Journal, stack$laterJournal, stack$authorDays),
			stringsAsFactors = FALSE
		)
		long$label = factor(long$label, levels = stack$label)
		long$piece = factor(long$piece, levels = c("Round 1 with the journal", "Later rounds with the journal", "With the author"))
		pAccepted = ggplot2::ggplot(long, ggplot2::aes(x = .data$days, y = .data$label, fill = .data$piece))
		# reverse = TRUE puts the first factor level at the axis, so the bar reads left to right: round 1, later rounds, author.
		pAccepted = pAccepted + ggplot2::geom_col(width = 0.72, position = ggplot2::position_stack(reverse = TRUE))
		pAccepted = pAccepted + ggplot2::scale_fill_manual(values = c("Round 1 with the journal" = journalBlue, "Later rounds with the journal" = journalLight, "With the author" = authorGold))
		pAccepted = pAccepted + ggplot2::labs(x = "Days from first submission to acceptance", y = NULL, fill = NULL, title = "Accepted papers: journal time and author time")
		pAccepted = pAccepted + ggplot2::theme_bw(base_size = 11)
		pAccepted = pAccepted + ggplot2::theme(legend.position = "bottom", panel.grid.major.y = ggplot2::element_blank())
		embedPlot("accepted", pAccepted, width = 10, height = max(6, 0.32 * nrow(stack) + 1.6))
	}

	stage = data.frame(
		stage = c(
			rep("To first acceptance", sum(!is.na(round1$triageToAcceptDays))),
			rep("Review window", sum(!is.na(round1Reviewed$reviewWindowDays))),
			rep("Editor after reviews", sum(!is.na(round1Reviewed$editorAfterDays)))
		),
		days = c(
			round1$triageToAcceptDays[!is.na(round1$triageToAcceptDays)],
			round1Reviewed$reviewWindowDays[!is.na(round1Reviewed$reviewWindowDays)],
			round1Reviewed$editorAfterDays[!is.na(round1Reviewed$editorAfterDays)]
		),
		stringsAsFactors = FALSE
	)
	stage$stage = factor(stage$stage, levels = c("To first acceptance", "Review window", "Editor after reviews"))
	pStages = ggplot2::ggplot(stage, ggplot2::aes(x = .data$stage, y = .data$days))
	pStages = pStages + ggplot2::geom_boxplot(fill = journalBlue, alpha = 0.25, width = 0.55, outlier.shape = NA)
	pStages = pStages + ggplot2::geom_jitter(width = 0.12, height = 0, size = 1.6, alpha = 0.55, colour = journalBlue)
	pStages = pStages + ggplot2::labs(x = NULL, y = "Days", title = "First version, once a reviewer is involved", subtitle = "Triage, then the review window, then the editor.")
	pStages = pStages + ggplot2::theme_bw(base_size = 12)
	pStages = pStages + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
	embedPlot("stages", pStages, width = 8, height = 5.2)

	if (nrow(reviewTurns) > 0) {
		turnMed = stats::median(reviewTurns$fromAcceptDays, na.rm = TRUE)
		pReviews = ggplot2::ggplot(reviewTurns, ggplot2::aes(x = .data$fromAcceptDays))
		pReviews = pReviews + ggplot2::geom_histogram(binwidth = 5, fill = journalBlue, colour = "white", boundary = 0)
		pReviews = pReviews + ggplot2::geom_vline(xintercept = turnMed, linetype = 2, colour = authorGold, linewidth = 0.6)
		pReviews = pReviews + ggplot2::labs(x = "Days from accepting the invitation to filing the review", y = "Reviews", title = "Reviewer turnaround", subtitle = paste0("n = ", sum(!is.na(reviewTurns$fromAcceptDays)), ". Dashed line is the median, ", fmt(turnMed), " days."))
		pReviews = pReviews + ggplot2::theme_bw(base_size = 12)
		embedPlot("reviews", pReviews, width = 8, height = 4.6)
	}

	if (nrow(authorGaps) > 0) {
		authorMed = stats::median(authorGaps$days)
		pAuthor = ggplot2::ggplot(authorGaps, ggplot2::aes(x = .data$days))
		pAuthor = pAuthor + ggplot2::geom_histogram(binwidth = 14, fill = authorGold, colour = "white", boundary = 0)
		pAuthor = pAuthor + ggplot2::geom_vline(xintercept = authorMed, linetype = 2, colour = journalBlue, linewidth = 0.6)
		pAuthor = pAuthor + ggplot2::labs(x = "Days from the decision to the next version arriving", y = "Revision returns", title = "Time with the author", subtitle = paste0("n = ", nrow(authorGaps), " completed returns. Dashed line is the median, ", fmt(authorMed), " days."))
		pAuthor = pAuthor + ggplot2::theme_bw(base_size = 12)
		embedPlot("author", pAuthor, width = 8, height = 4.6)
	}

	if (nrow(revisionRounds) > 0) {
		revisionRounds$decisionLabel = revisionRounds$decisionType
		revisionRounds$decisionLabel[revisionRounds$decisionLabel == "accept"] = "Accept"
		revisionRounds$decisionLabel[revisionRounds$decisionLabel == "reject"] = "Reject"
		revisionRounds$decisionLabel[revisionRounds$decisionLabel == "revise_and_resubmit"] = "Revise and resubmit"
		revisionRounds$decisionLabel[is.na(revisionRounds$decisionLabel)] = "Undecided label"
		pRevision = ggplot2::ggplot(revisionRounds, ggplot2::aes(x = .data$decisionLabel, y = .data$journalDays))
		pRevision = pRevision + ggplot2::geom_boxplot(fill = journalLight, alpha = 0.35, width = 0.5, outlier.shape = NA)
		pRevision = pRevision + ggplot2::geom_jitter(width = 0.12, height = 0, size = 1.8, alpha = 0.65, colour = journalBlue)
		pRevision = pRevision + ggplot2::labs(x = NULL, y = "Days from resubmission to the decision", title = "Journal time on a revision", subtitle = "Version 2 and later. An accept with no new reviews sits near zero.")
		pRevision = pRevision + ggplot2::theme_bw(base_size = 12)
		pRevision = pRevision + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
		embedPlot("revision", pRevision, width = 8, height = 5)
	}

	createdSpan = range(versions$createdAt, na.rm = TRUE)
	nAccepted = sum(papers$outcome == "accepted")
	nRejected = sum(papers$outcome == "rejected")
	nWithdrawn = sum(papers$outcome == "withdrawn")
	nWithAuthor = sum(papers$outcome == "with_author")
	nInReview = sum(papers$outcome == "in_review")
	direct = accepted[accepted$nRounds == 1, , drop = FALSE]
	via = accepted[accepted$nRounds > 1, , drop = FALSE]
	fig = function(name, alt) {
		src = plotUri[[name]]
		if (is.null(src)) {
			return("")
		}
		paste0("<figure><img alt=\"", htmlEscape(alt), "\" src=\"", src, "\"></figure>")
	}

	page = c(
		"<!DOCTYPE html>",
		"<html lang=\"en\"><head><meta charset=\"utf-8\">",
		"<title>ICA editorial efficiency</title>",
		"<style>",
		"body { font: 16px/1.45 -apple-system, BlinkMacSystemFont, sans-serif; color: #1c2833; max-width: 920px; margin: 2rem auto; padding: 0 1.2rem 3rem; }",
		"h1 { font-size: 1.6rem; margin-bottom: 0.3rem; } h2 { font-size: 1.2rem; margin: 1.8rem 0 0.4rem; }",
		"p { margin: 0.45rem 0 0.7rem; } p.lead { margin: 0.15rem 0; } p.note { margin: 0.6rem 0 0.8rem; } figure { margin: 0.4rem 0 1rem; }",
		"img { width: 100%; height: auto; }",
		"table { border-collapse: collapse; width: 100%; margin: 0.4rem 0 0.8rem; }",
		"th, td { text-align: right; padding: 0.28rem 0.5rem; border-bottom: 1px solid #d5d8dc; vertical-align: top; }",
		"th:first-child, td:first-child { text-align: left; }",
		"thead th { border-bottom: 2px solid #1b4f72; }",
		"</style></head><body>",
		"<h1>ICA editorial efficiency</h1>",
		paste0("<p class=\"lead\">Report written ", htmlEscape(format(Sys.time(), "%Y-%m-%d %H:%M %Z")), "</p>"),
		paste0("<p class=\"lead\">We have handled ", nrow(papers), " papers, ", nrow(versions), " versions (trial submissions dropped).</p>"),
		paste0("<p class=\"lead\">Latest state: ", nAccepted, " accepted, ", nRejected, " rejected, ", nInReview, " in review, ", nWithAuthor, " with the author, ", nWithdrawn, " withdrawn.</p>"),
		"<h2>Accepted papers</h2>",
		statTable(c(
			statRow("Calendar, first submission to acceptance", accepted$calendarDays),
			statRow("With the journal", accepted$journalDays),
			statRow("With the author", accepted$authorDays),
			statRow("Accepted with no revision, journal time", direct$journalDays),
			statRow("Revised then accepted, journal time", via$journalDays),
			statRow("Revised then accepted, author time", via$authorDays),
			statRow("Revised then accepted, round 1 with the journal", via$round1Journal),
			statRow("Revised then accepted, later rounds with the journal", via$laterJournal)
		)),
		paste0("<p class=\"note\">Note: Calendar days run from the first submission to the final decision; Journal days sum, over each version, the time from that version arriving to its decision; Author days are the gaps between a decision and the next version. First submission ", format(createdSpan[1], "%Y-%m-%d"), ", most recent ", format(createdSpan[2], "%Y-%m-%d"), ".</p>"),
		fig("accepted", "Accepted papers, journal time and author time"),
		paste0("<p>", nrow(direct), " papers were accepted on the first version. ", nrow(via), " went out and came back at least once. On those, later rounds with the journal are much shorter than round 1, because most accepting rounds did not go out for a new review.</p>"),
		"<h2>First submission</h2>",
		statTable(c(
			statRow("Submit to first reviewer invited", round1$triageToInviteDays),
			statRow("Submit to first reviewer accepts", round1$triageToAcceptDays),
			statRow("First acceptance to last review", round1Reviewed$reviewWindowDays),
			statRow("Last review to the decision", round1Reviewed$editorAfterDays),
			statRow("Submit to first decision, all decided first versions", round1Decided$journalDays),
			statRow("No reviewer invited, submit to decision", deskOnly$journalDays)
		)),
		fig("stages", "First-version stage times"),
		"<p>The boxplots are first versions that reached a reviewer. Desk rejects and editorial accepts with no invitation are in the row with no reviewer invited, and inside the all-decided first-decision row.</p>",
		"<h2>Reviewers</h2>",
		statTable(c(
			statRow("Invitation sent to review filed", reviewTurns$fromInviteDays),
			statRow("Invitation accepted to review filed", reviewTurns$fromAcceptDays)
		)),
		fig("reviews", "Reviewer turnaround"),
		"<p>Each filed review is one count. The manuscript-level review window above runs from the first acceptance to the last review on that version, so it is longer than a typical single review when two reviewers are out together or the second is found late.</p>",
		"<h2>Author revision</h2>",
		statTable(statRow("Decision to the next version", authorGaps$days)),
		fig("author", "Time with the author")
	)
	if (nrow(authorGaps) > 0) {
		slow = authorGaps[which.max(authorGaps$days), ]
		page = c(page, paste0("<p>The longest completed return is ", fmt(slow$days), " days, on &ldquo;", htmlEscape(slow$title), "&rdquo;, decision ", format(slow$decidedAt, "%Y-%m-%d"), ", revision in on ", format(slow$returnedAt, "%Y-%m-%d"), ".</p>"))
	}
	page = c(page,
		"<h2>Accepting a revision</h2>",
		statTable(c(
			statRow("Any decided revision round", revisionRounds$journalDays),
			statRow("The round that accepted", acceptRounds$journalDays),
			statRow("Accepting round with a new review", acceptRounds$journalDays[acceptRounds$nReview > 0]),
			statRow("Accepting round with no new review", acceptRounds$journalDays[acceptRounds$nReview == 0])
		)),
		fig("revision", "Journal time on a revision"),
		"<h2>Still open</h2>"
	)
	if (length(openRows) == 0) {
		page = c(page, "<p>No manuscript is in review or waiting on an author.</p>")
	} else {
		openHtml = paste0("<p>Elapsed days use ", format(asOf), " as the clock. These papers are not in the means above.</p>")
		openHtml = paste0(openHtml, "<table><thead><tr><th>Manuscript</th><th>State</th><th>Version</th><th>Days elapsed</th></tr></thead><tbody>")
		for (i in seq_along(openRows)) {
			row = openRows[[i]]
			openHtml = paste0(openHtml, "<tr><td>", htmlEscape(row$title), "</td><td>", htmlEscape(row$workflowState), "</td><td>", row$versionNumber, "</td><td>", fmt(row$elapsedDays), "</td></tr>")
		}
		page = c(page, paste0(openHtml, "</tbody></table>"))
	}
	acceptedHtml = "<h2>Accepted papers, one row each</h2><table><thead><tr><th>Manuscript</th><th>Rounds</th><th>Journal</th><th>Author</th><th>Calendar</th></tr></thead><tbody>"
	if (nrow(accepted) > 0) {
		show = accepted[order(-accepted$calendarDays), , drop = FALSE]
		for (i in seq_len(nrow(show))) {
			acceptedHtml = paste0(acceptedHtml, "<tr><td>", htmlEscape(show$title[i]), "</td><td>", show$nRounds[i], "</td><td>", fmt(show$journalDays[i]), "</td><td>", fmt(show$authorDays[i]), "</td><td>", fmt(show$calendarDays[i]), "</td></tr>")
		}
	}
	page = c(page, paste0(acceptedHtml, "</tbody></table></body></html>"))

	htmlFile = tempfile(pattern = "ica_report_", fileext = ".html")
	writeLines(page, htmlFile)
	umx_open(htmlFile)

	invisible(list(
		htmlFile = htmlFile,
		papers = papers,
		versions = versions,
		authorGaps = authorGaps,
		reviewTurns = reviewTurns,
		revisionRounds = revisionRounds
	))
}
