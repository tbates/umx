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

gh_open_app_support <- function(){
	system2("open ~/Library/Application\ Support/TextMate")
}

gh_remove_quarantine <- function(){
	system2("xattr -dr com.apple.quarantine ~/Downloads/TextMate.app")
}

gh_sym_link_bundle <- function(which = "source.tmbundle", local_path = "~/bin/tm/bundles/", destination = "$HOME/Library/Application\ Support/TextMate/Bundles/"){
	fullPath = paste0(local_path, which) # e.g., ~/bin/tm/bundles/GitHub-Markdown-Font-Settings.tmbundle
	fullDest = paste0(destination, which) # e.g.,"$HOME/Library/Application Support/TextMate/Bundles/GitHub-Markdown-Font-Settings.tmbundle"
	# todo checkExists(fullPath) exists
	# todo checkExists(destination)
	# todo check not already have bundle at dest
	# todo check ln not already set
	symlinkCommand = paste0("ln -s ", fullPath,  fullDest)
	system2(symlinkCommand) 
	cat("created:", symlinkCommand)
}

# git diff --name-only -z upstream/main main | xargs -0 git checkout main --

gh_teardown <- function(which = "source.tmbundle", local_path = "~/bin/tm/bundles/", destination = ""){
	fullPath = paste0(local_path, which)
	# check(fullPath) exists
 	# cd GitHub-Markdown-Font-Settings.tmbundle
	# git branch -r | grep font-menlo
	# gh repo delete tbates/GitHub-Markdown-Font-Settings.tmbundle
	# rm "$HOME/Library/Application Support/TextMate/Bundles/GitHub-Markdown-Font-Settings.tmbundle"
	system2()
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
#' @references - [tutorials](https://tbates.github.io), [tutorials](https://github.com/tbates/umx)
#' @md
#' @examples
#' gh_open_PR_url(head_branch, base_owner, base_repo, base_branch, head_owner, local_path, browse=FALSE)
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
#' @return - list of components
#' @export
#' @family github
#' @seealso - [gh_feature_branch()]
#' @references - [tutorials](https://tbates.github.io), [tutorials](https://github.com/tbates/umx)
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
