# ====================
# = Could be in Rtim = #
# ====================
# gh_show_run_list
# 	"gh run list --limit 100 --json databaseId -q '.[].databaseId' | xargs -I{}"
# gh_remove_quarantine
# 	"xattr -dr com.apple.quarantine ~/Downloads/TextMate.app"
# gh_open_app_support
# 	system2("open ~/Library/Application\ Support/TextMate")

#= -TODO- =#
# cd /path/to/your/r.tmbundle   # wherever the symlinked clone lives
# git status                     # must be clean — nothing uncommitted
# git log --oneline -5           # confirm nothing un-PR'd that you care about
# git remote -v                  # check 'upstream' exists; if not:
# # git remote add upstream https://github.com/textmatelives/r.tmbundle.git
# git fetch upstream
# git checkout main
# git reset --hard upstream/main


#' Open TextMate Application Support Directory
#'
#' Opens the user's TextMate Application Support directory in Finder and 
#' brings the Finder application to the foreground.
#'
#' @details
#' Expands the `~/Library/Application Support/TextMate` path and opens it using
#' the macOS `open` utility. An AppleScript command is subsequently issued via
#' `osascript` to bring Finder into active focus.
#'
#' @return Invisible `NULL`. Called for side effects.
#' @family github
#' @export
gh_openAppSupport <- function() {
    targetPath = path.expand("~/Library/Application Support/TextMate")
    
    if (!dir.exists(targetPath)) {
        stop("Directory does not exist: ", targetPath)
    }
    
    system2("open", targetPath)
    system2("osascript", c("-e", 'tell application "Finder" to activate'))
}

#' Remove macOS Quarantine Attribute
#'
#' Recursively removes the `com.apple.quarantine` extended attribute from a 
#' specified file or application bundle.
#'
#' @param path Character string. Path to the file or app bundle. Tildes are expanded.
#' @return No return value, called for side effects.
#' @family github
#' @export
gh_removeQuarantine <- function(path = "~/Downloads/TextMate.app") {
    targetPath = path.expand(path)
    
    if (!file.exists(targetPath)) {
        stop("Target path does not exist: ", targetPath)
    }
    
    system2("xattr", c("-dr", "com.apple.quarantine", targetPath))
}

#' Symlink a Custom TextMate Bundle
#'
#' Creates a symbolic link from a local development bundle directory 
#' into TextMate's active bundle directory.
#'
#' @param which Character string. Name of the `.tmbundle` file or folder.
#' @param localPath Character string. Path to local bundles repository. 
#'   Tildes are expanded.
#' @param destination Character string. Path to TextMate's Bundles directory. 
#'   Tildes are expanded.
#' @return Invisible `NULL`. Called for side effects.
#' @family github
#' @export
gh_symLink_bundle <- function(which = "source.tmbundle", localPath = "~/bin/tm/bundles/", 
    destination = "~/Library/Application Support/TextMate/Bundles/") {
    fullPath = path.expand(file.path(localPath, which))
    destDir  = path.expand(destination)
    fullDest = file.path(destDir, which)
    # 1. Check if source bundle exists
    if (!file.exists(fullPath)) {
        stop("Source bundle does not exist: ", fullPath)
    }
    # 2. Ensure destination directory exists (create it if missing)
    if (!dir.exists(destDir)) {
        dir.create(destDir, recursive = TRUE, showWarnings = FALSE)
    }
    # 3. Check existing destination state
    if (file.exists(fullDest) || dir.exists(fullDest)) {
        if (Sys.islink(fullDest)) {
            currentLinkTarget = normalizePath(Sys.readlink(fullDest), mustWork = FALSE)
            if (currentLinkTarget == normalizePath(fullPath, mustWork = FALSE)) {
                message("Symlink already correctly configured for: ", which)
                return(invisible(NULL))
            } else {
                stop("A symlink already exists at destination, but points elsewhere: ", currentLinkTarget)
            }
        } else {
            stop("A non-symlink file or directory already occupies the destination path: ", fullDest)
        }
    }
        # 4. Create the symbolic link safely using system2 vector arguments
    res = system2("ln", c("-s", fullPath, fullDest))
    
    if (res == 0) {
        cat("Created symlink:\n  ", fullDest, "\n  -> ", fullPath, "\n", sep = "")
    } else {
        stop("Failed to create symbolic link (exit code ", res, ")")
    }
    
    invisible(NULL)
}

# git diff --name-only -z upstream/main main | xargs -0 git checkout main --

#' Teardown Local TextMate Bundle Fork and Symlink
#'
#' Removes the symlink from TextMate's Bundles directory, deletes the remote GitHub repository fork using the `gh` CLI, and optionally removes the local source directory.
#'
#' @param which Character string. Name of the `.tmbundle` directory.
#' @param localPath Character string. Path to local bundles repository. Tildes are expanded.
#' @param destination Character string. Path to TextMate's Bundles directory. Tildes are expanded.
#' @param deleteLocal Logical. Whether to delete the local source folder (`fullPath`) after cleaning up. Default is `FALSE`.
#' @param dryRun Logical. If `TRUE`, prints the actions that would be taken without deleting files or calling `gh repo delete`. Default is `FALSE`.
#' @return Invisible `NULL`. Called for side effects.
#' @family github
#' @export
#' @examples
#' \dontrun{
#' ghTeardownBundle(which = "source.tmbundle", deleteLocal = FALSE, dryRun = TRUE)
#' }
gh_teardown_bundle <- function(which = "source.tmbundle", localPath = "~/bin/tm/bundles/", 
    destination = "~/Library/Application Support/TextMate/Bundles/", 
	deleteLocal = FALSE, dryRun = TRUE) {

    fullPath = path.expand(file.path(localPath, which))
    destDir  = path.expand(destination)
    fullDest = file.path(destDir, which)
    
    if (dryRun) {
        cat("--- DRY RUN MODE ACTIVE ---\n")
    }
    
    # 1. Remove the symlink if it exists
    if (file.exists(fullDest) || nzchar(Sys.readlink(fullDest))) {
        if (nzchar(Sys.readlink(fullDest))) {
            if (dryRun) {
                cat("[DRY RUN] Would remove symlink: ", fullDest, "\n")
            } else {
                unlink(fullDest)
                cat("Removed symlink: ", fullDest, "\n")
            }
        } else {
            warning("Destination exists but is not a symlink (skipped unlink): ", fullDest)
        }
    } else {
        message("No symlink found at destination: ", fullDest)
    }
    
    # 2. Handle remote GitHub repo deletion if the local repo exists
    if (dir.exists(fullPath)) {
        owd = setwd(fullPath)
        on.exit(setwd(owd), add = TRUE)
        
        if (dir.exists(".git")) {
            remoteUrl = system2("git", c("config", "--get", "remote.origin.url"), stdout = TRUE)
            
            if (length(remoteUrl) > 0 && nzchar(remoteUrl)) {
                slug = sub(".*github\\.com[:/]([^/]+/[^\\.]+)(\\.git)?.*", "\\1", remoteUrl)
                
                if (nzchar(slug) && grepl("/", slug)) {
                    if (dryRun) {
                        cat("[DRY RUN] Would execute: gh repo delete ", slug, " --yes\n", sep = "")
                    } else {
                        cat("Deleting remote GitHub repository: ", slug, "\n")
                        res = system2("gh", c("repo", "delete", slug, "--yes"))
                        
                        if (res != 0) {
                            warning("Failed to delete remote repo via `gh`. Check your `gh` CLI auth status.")
                        }
                    }
                } else {
                    warning("Could not parse valid GitHub slug from remote URL: ", remoteUrl)
                }
            } else {
                message("No remote origin URL found in local git config.")
            }
        } else {
            message("Local directory is not a git repository: ", fullPath)
        }
        
        # 3. Optionally clean up local directory
        if (deleteLocal) {
            setwd(owd) # Restore working directory before removing
            if (dryRun) {
                cat("[DRY RUN] Would delete local directory: ", fullPath, "\n")
            } else {
                unlink(fullPath, recursive = TRUE, force = TRUE)
                cat("Removed local source directory: ", fullPath, "\n")
            }
        }
    } else {
        message("Local source path does not exist: ", fullPath)
    }    
    invisible(NULL)
}


#' Make a feature branch on a github fork.
#' 
#' @description
#' `gh_feature_branch` takes a feature name and creates a branch on the user's fork (createing this if necessary from an "owner/repo:branch".
#' 
#' It will
#' 1. Fork to your account (if not already existing)
#' 2. Clone to your machine (if not already cloned)
#' 3. Move to preferred location
#' 4. Create the requested fix/feature branch and switch to it
#' 
#' You can then 
#' 5. Edit, commit, push, repeat: success!
#' 6. Squash if necessary
#' 7. pull request from your/branch to upstream/main using `gh_open_PR_url` if you wish.
#'
#' @param feature    = "fix-piglet"
#' @param base       = "textmatelives/textmate:main"
#' @param head_owner = "tbates"
#' @param local_path = "~/bin/tm/bundle/"
#' @return - status
#' @export
#' @family github
#' @seealso - [gh_open_PR_url()]
#' @examples
#' \dontrun{
#' 	gh_feature_branch("fix/font_issue", base = "textmatelives/textmate:main")
#' }
gh_feature_branch <- function(feature = "fix-piglet", base = "textmatelives/textmate:main", head_owner = "tbates", local_path = "~/bin/tm/bundles/"){
	# TODO: could store head owner in a preference
	# TODO: could invent branch name from local_path + feature Dumb???

	# 1. reprocess "textmatelives/textmate:main"
	baseStar = xgh_check_base_name(base = "textmatelives/textmate:main")
	baseStar$owner  # e.g. "textmatelives"
	baseStar$repo   # e.g. "textmate"
	baseStar$branch # e.g. "main"

	# 1. Does head_owner have a fork yet?
	# 	* No: create it
	# 	* Yes: use it
	# 2. Has fork been cloned locally?
	# 	* No: clone to local_path
	# 	* Yes: use it
	# 3. Checkout new feature branch (or code in main?)

	# cd ~/bin/tm/bundles/GitHub-Markdown-Font-Settings.tmbundle
	
	# git fetch upstream
	# git checkout -b font-menlo-1em upstream/main
	# git checkout main -- "Preferences/Font Name and Size.tmPreferences" README.md
	# git push -u origin font-menlo-1em, git checkout main
	# git push -u origin font-menlo-1em
	# git checkout font-menlo-1em

	# 4. Switch to new feature branch

	return("created ", head_owner, "/", feature, at )
}

#' Build and open the compare URL GitHub renders as a PR page.
#'
#' @description
#' `gh_open_PR_url` takes the owner, repo and branch you want to open a PR on, along with your owner name, repo and
#' head branch you want to pull from, and opens github at the exact page you need.
#' 
#' @details
#' Before doing this, you want to
#' 1. Fork to your account
#' 2. Clone to your machine
#' 3. Move to preferred location
#' 4. Make a fix or feature branch and switch to it
#' 5. Edit, commit, push, repeat: success!
#' 6. Squash if necessary
#' 7. PR from your/branch to upstream/main
#' 
#' `gh_open_PR_url` solves #7: pull requesting
#'
#' @param head_branch Default `NULL`.
#' @param base_owner Default `"textmatelives"`.
#' @param base_repo Default `"textmate"`.
#' @param base_branch Default `"main"`.
#' @param head_owner Default `"tbates"`.
#' @param local_path Default `"."`.
#' @param browse Default `TRUE`.
#' @return - url
#' @export
#' @family github
#' @seealso - [gh_feature_branch()]
#' @references \url{https://tbates.github.io}, [tutorials](https://github.com/tbates/umx)
#' @md
#' @examples
#' \dontrun{
#' gh_open_PR_url(head_branch="fix", browse = FALSE)
#' }
gh_open_PR_url <- function(head_branch = NULL, base_owner = "textmatelives", base_repo = "textmate", base_branch = "main", head_owner = "tbates", local_path = ".", browse=TRUE) {
	if (is.null(head_branch) || is.na(head_branch) || head_branch == "") {
		head_branch = xgh_get_current_branch(local_path)
	}
	# Build the compare URL GitHub renders as a PR page. Pure: no browser.
	bits = list(head_branch = head_branch, base_owner = base_owner, base_repo = base_repo, base_branch = base_branch, head_owner = head_owner)
	bad = names(bits)[vapply(bits, function(x) length(x) != 1L || is.na(x) || x == "", logical(1))]
	if (length(bad)) {
		stop("empty pieces: ", paste(bad, collapse = ", "), call. = FALSE)
	}
	url = paste0("https://github.com/", base_owner, "/", base_repo, "/compare/", base_branch, "...", head_owner, ":", head_branch)
	if(browse){
		utils::browseURL(url)
	}
	invisible(url)
}

#' Search commit messages or a commit id, and link the hits on GitHub.
#'
#' @description
#' `gh_message_search` asks git for commits on the current branch whose
#' message contains `search`, and prints the newest `max` of them.
#' `sort = "asc"` flips that set so the oldest of those hits is first.
#'
#' A `search` of 4 to 64 hex digits is tried as a commit id first
#' (`git rev-parse`). That commit is printed as one row, including when
#' it is not on the current branch. If the id is not an object, the
#' search falls through to the message grep. An ambiguous short id
#' stops with git's message.
#'
#' The text is applied to the whole commit message. Each row has a
#' number, the date, a short sha, and the subject. The subject is only
#' the first line, so the match may be further down the message.
#' When `origin` is a GitHub remote, the row also has the commit page.
#' Pass `open` as that row number to show it in the browser.
#' The same page is `hits$url[n]` on the data.frame returned.
#'
#' `regex = FALSE` (the default) treats `search` as literal text (`git log -F --grep`). `*`, `+`, `()`, and `|` match themselves. `regex = TRUE` treats `search` as an extended regular expression (`git log -E --grep`). Matching ignores case unless `ignore_case = FALSE`.
#'
#' @param search Text to find in the commit message, or a commit id of 4 to 64 hex digits.
#' @param repo Local checkout. Default `"~/bin/umx"`.
#' @param max How many of the newest matches to keep. Default `10`.
#' @param sort `"desc"` (newest first) or `"asc"`. Default `"desc"`.
#' @param ignore_case Default `TRUE`.
#' @param open Row number to open on GitHub. Default `NULL` (print only).
#' @param regex `TRUE` for an extended regular expression. Default `FALSE` (literal text).
#' @return A data.frame (invisibly) with columns `n`, `date`, `sha`, `short`, `subject`, `url`.
#' @export
#' @family github
#' @md
#' @examples
#' \dontrun{
#' hits <- gh_message_search("double entry")
#' gh_message_search("double entry", open = 1)
#' gh_message_search("4d395296", "~/bin/tm/textmate/")
#' gh_message_search("open.app", regex = TRUE)
#' }
gh_message_search <- function(search, repo = "~/bin/umx", max = 10, sort = c("desc", "asc"), ignore_case = TRUE, open = NULL, regex = FALSE) {
	sort = match.arg(sort)
	if (length(search) != 1L || is.na(search) || !is.character(search) || !nzchar(search)) {
		stop("search must be one non-empty string.", call. = FALSE)
	}
	if (length(regex) != 1L || is.na(regex) || !is.logical(regex)) {
		stop("regex must be TRUE or FALSE.", call. = FALSE)
	}
	if (length(max) != 1L || is.na(max) || !is.numeric(max) || max < 1) {
		stop("max must be a positive number.", call. = FALSE)
	}
	max = as.integer(max)
	repo = path.expand(repo)
	if (!dir.exists(repo)) {
		stop("repo not found: ", repo, call. = FALSE)
	}
	inside = xgh_git(repo, c("rev-parse", "--is-inside-work-tree"))
	if (!is.null(attr(inside, "status")) || !length(inside) || inside[1] != "true") {
		stop("not a git repo: ", repo, call. = FALSE)
	}

	log = NULL
	commitId = grepl("^[0-9a-fA-F]{4,64}$", search)
	if (commitId) {
		resolved = xgh_git(repo, c("rev-parse", "--verify", "--end-of-options", paste0(search, "^{commit}")))
		resolvedStatus = attr(resolved, "status")
		if (is.null(resolvedStatus)) {
			fullSha = resolved[grepl("^[0-9a-f]{40}$|^[0-9a-f]{64}$", resolved)]
			if (length(fullSha) == 1L) {
				log = xgh_git(repo, c("log", "-1", "--date=short", "--pretty=format:%H%x09%h%x09%ad%x09%s", "--end-of-options", fullSha))
			}
		} else if (length(resolved) && any(grepl("ambiguous", resolved, ignore.case = TRUE))) {
			stop(paste(resolved, collapse = "\n"), call. = FALSE)
		}
	}
	if (is.null(log)) {
		args = c("log", if (isTRUE(regex)) "-E" else "-F", if (isTRUE(ignore_case)) "-i", paste0("--grep=", search), paste0("--max-count=", max), "--date=short", "--pretty=format:%H%x09%h%x09%ad%x09%s")
		log = xgh_git(repo, args)
	}
	status = attr(log, "status")
	if (!is.null(status) && status != 0) {
		stop(paste(log, collapse = "\n"), call. = FALSE)
	}
	blank = data.frame(n = integer(), date = character(), sha = character(), short = character(), subject = character(), url = character(), stringsAsFactors = FALSE)
	if (!length(log) || (length(log) == 1L && !nzchar(log))) {
		message("no commits match in ", repo)
		return(invisible(blank))
	}

	parts = strsplit(log, "\t", fixed = TRUE)
	bad = which(lengths(parts) < 4L)
	if (length(bad)) {
		stop("unexpected git log line: ", log[bad[1]], call. = FALSE)
	}
	sha = vapply(parts, `[`, "", 1L)
	short = vapply(parts, `[`, "", 2L)
	date = vapply(parts, `[`, "", 3L)
	subject = vapply(parts, function(x) paste(x[4:length(x)], collapse = "\t"), "")
	if (sort == "asc") {
		sha = rev(sha); short = rev(short); date = rev(date); subject = rev(subject)
	}

	base = xgh_github_origin(repo)
	url = if (is.na(base)) rep(NA_character_, length(sha)) else paste0(base, "/commit/", sha)
	hits = data.frame(n = seq_along(sha), date = date, sha = sha, short = short, subject = subject, url = url, stringsAsFactors = FALSE)

	cap = if (nrow(hits) < max) {
		paste0(nrow(hits), if (nrow(hits) == 1L) " match" else " matches")
	} else {
		paste0(max, " newest matches")
	}
	if (sort == "asc") {
		cap = paste0(cap, ", oldest first")
	}
	cat("\n", repo, "  (", cap, ")\n", sep = "")
	for (i in seq_len(nrow(hits))) {
		cat(sprintf("%2d  %s  %s  %s\n", hits$n[i], hits$date[i], hits$short[i], hits$subject[i]))
		if (!is.na(hits$url[i])) {
			cat("    ", hits$url[i], "\n", sep = "")
		}
	}
	if (anyNA(hits$url)) {
		cat("    origin is not a GitHub remote, so there is no commit page.\n")
	}
	if (!is.null(open)) {
		if (length(open) != 1L || is.na(open) || !is.numeric(open) || open != as.integer(open) || open < 1 || open > nrow(hits)) {
			stop("open must be a row number from 1 to ", nrow(hits), call. = FALSE)
		}
		if (is.na(hits$url[open])) {
			stop("no GitHub URL for row ", open, call. = FALSE)
		}
		utils::browseURL(hits$url[open])
	}
	invisible(hits)
}

# =======================
# = - xgh functions - = #
# =======================

#' Get the current git branch
#'
#' @description
#' `xgh_get_current_branch` takes a local_path to return a current branch
#'
#' @param local_path Default `"."`.
#' @return - current_branch name
#' @export
#' @family github
#' @seealso - [gh_open_PR_url()]
#' @md
#' @examples
#' \dontrun{
#' xgh_get_current_branch(local_path)
#' }
xgh_get_current_branch <- function(local_path = ".") {
	# Fill head_branch from the repo you are standing in.
	out = suppressWarnings(system2("git", c("-C", local_path, "branch", "--show-current"), stdout = TRUE, stderr = FALSE))
	if (!length(out) || out == "") {
		stop("no current branch in ", local_path, call. = FALSE)
	}
	out
}

#' Check branch address is valid.
#'
#' @description
#' `xgh_check_base_name` takes a full git branch address, validates it, and returns it
#' as a list. For example, "textmatelives/textmate:fix/browser_sorting" would return
#' `list(owner = "textmatelives", repo = "textmate", branchname = "fix/browser_sorting")`.
#'
#' @param base A three-component branch name. Default `"textmatelives/textmate:main"`
#' @return list of components
#' @export
#' @family github
#' @seealso [gh_feature_branch()]
#' @references \url{https://tbates.github.io}, [tutorials](https://github.com/tbates/umx)
#' @md
#' @examples
#' xgh_check_base_name("textmatelives/textmate:fix/browser_sorting")
xgh_check_base_name <- function(base = "textmatelives/textmate:main") {
	# Split "owner/repo:branch" into its three pieces.
	# The colon is the anchor: colons are illegal in git refs, so exactly one must exist.
	# Branch names may contain slashes, so only the owner/repo part is slash-checked.
	if (length(base) != 1L || is.na(base) || base == "") {
		stop("base must be one string like \"owner/repo:branch\".", call. = FALSE)
	}
	colon = gregexpr(":", base, fixed = TRUE)[[1]]
	if (length(colon) != 1L || colon == -1L) {
		stop("base must contain exactly one \":\", e.g. \"owner/repo:branch\".", call. = FALSE)
	}
	front = substr(base, 1L, colon - 1L)
	branch = substr(base, colon + 1L, nchar(base))
	if (branch == "") {
		stop("base needs a branch after the colon.", call. = FALSE)
	}
	parts = strsplit(front, "/", fixed = TRUE)[[1]]
	if (length(parts) != 2L || any(parts == "")) {
		stop("base must look like \"owner/repo:branch\".", call. = FALSE)
	}
	list(owner = parts[1L], repo = parts[2L], branch = branch)
}

#' GitHub base URL for a repo's origin remote.
#'
#' @param repo Local checkout.
#' @return `"https://github.com/owner/name"`, or `NA` when origin is not GitHub.
#' @keywords internal
xgh_github_origin <- function(repo) {
	remote = xgh_git(repo, c("remote", "get-url", "origin"))
	if (!is.null(attr(remote, "status")) || !length(remote) || remote[1] == "") {
		return(NA_character_)
	}
	u = sub("\\.git$", "", remote[1])
	if (grepl("^https://github.com/[^/]+/[^/]+$", u)) {
		return(u)
	}
	if (grepl("^git@github.com:[^/]+/[^/]+$", u)) {
		return(sub("^git@github.com:", "https://github.com/", u))
	}
	if (grepl("^ssh://git@github.com/[^/]+/[^/]+$", u)) {
		return(sub("^ssh://git@github.com/", "https://github.com/", u))
	}
	NA_character_
}

#' Run git in a repo. This R's system2 pastes args into a shell command.
#'
#' @param repo Local checkout, passed to `git -C`.
#' @param args Character vector of git arguments, quoted one by one.
#' @return Character vector of output, with a `status` attribute on failure.
#' @keywords internal
xgh_git <- function(repo, args) {
	suppressWarnings(system2("git", c(shQuote(c("-C", repo)), shQuote(args)), stdout = TRUE, stderr = TRUE))
}
