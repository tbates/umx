# TODO 

# gh_feature_branch(feature = "fix_em", base = "textmatelives/textmate/main", head_owner = "tbates", local_path = "~/bin/tm/bundle/")

# gh_open_PR_url <- function(head_branch = "textmate_fix_problem", base_owner = "textmatelives", base_repo = "textmate", base_branch = "main", head_owner = "tbates")

#' gh_open_PR_url TITLE
#'
#' @description
#' gh_open_PR_url is a function which 
#'
#' @details
#'
#' @param head_branch Default `"textmate_fix_problem"`.
#' @param base_owner Default `"textmatelives"`.
#' @param base_repo Default `"textmate"`.
#' @param base_branch Default `"main"`.
#' @param head_owner Default `"tbates"`.
#' @return - [gh_open_PR_url()]
#' @export
#' @family
#' @seealso - [umxLabel()]
#' @references - [tutorials](https://tbates.github.io), [tutorials](https://github.com/tbates/umx)
#' @md
#' @examples
#' gh_open_PR_url(head_branch, base_owner, base_repo, base_branch, head_owner)
#' \dontrun{
#' 
#' }
gh_open_PR_url <- function(head_branch = "textmate_fix_problem", base_owner= "textmatelives", base_repo= "textmate", base_branch= "main", head_owner= "tbates"){
	# once the feature is written, this function would let the user open github to the correct page without having to navigate their imperfect GUI
}

#' xgh_check_base_name TITLE
#'
#' @description
#' `xgh_check_base_name` takes a full git branch address, checks it, and returns it
#' as a `list(owner = , repo = , branchname = )`
#' "textmatelives/textmate:main"
#'
#' @details
#'
#' @param base A three-component branch name. Default `"textmatelives/textmate:main"`
#' @return - [xgh_check_base_name()]
#' @export
#' @family
#' @seealso - [gh_feature_branch()]
#' @references - [tutorials](https://tbates.github.io), [tutorials](https://github.com/tbates/umx)
#' @md
#' @examples
#' xgh_check_base_name(base)
#' \dontrun{
#' 
#' }
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
	# reprocess "textmatelives/textmate:main"
	# could invent branch name from local_path +  + feature
	# could store head owner in a preference
	# 1. Does fork at head_owner exist?
	# 	* No: create it
	# 	* Yes: use it
	# 2. Has fork been cloned locally?
	# 	* No: clone
	# 	* Yes: use it
	xgh_check_base_name(base = "textmatelives/textmate:main")
	head_branch = "textmate_fix_problem"
		
	return(result)
}


#' Open a PR on github
#' Maybe do this in umx where it can take parameters!!
#' 
#' @description
#' `gh_open_PR` takes the owner, repo and branch you want to push to, your owner name, repo and head branch you want to pull from, and opens github at the
#' exact page you need.
#' 
#' Before doing this, you want to
#' 1. Fork to your account
#' 2. Clone to your machine
#' 3. Move to preferred location
#' 4. Make a fix or feature branch and switch to it
#' 5. Edit, commit, push, repeat: success!
#' 6. Squash if necessary
#' 7. pull request from your/branch to upstream/main
#' 
#' This `gh_open_PR_url` solves #7: pull requesting
#'
#' Other functions might include:
#' * `gh_feature_branch`
#' 
#' 
#' @details
#'
#' @param head_branch = "textmate_fix_problem"
#' @param base_owner  = "textmatelives"
#' @param base_repo   = "textmate"
#' @param base_branch = "textmate/main"
#' @param head_owner  = "tbates"
#' @return - status
#' @export
#' @family xmu internal not for end user
#' @seealso - [gh_open_PR_url()]
#' @examples
#' \dontrun{
#' 	gh_open_PR_url(head_branch = "menlo-font")
#' }
gh_open_PR_url <- function(head_branch = "textmate_fix_problem", base_owner = "textmatelives", base_repo = "textmate", base_branch = "main", head_owner = "tbates") {
	# could we fill head from the current repo?
	# base-repo/compare/base-branch...head-owner:head-branch
	# snippet idea
	# ${1|textmatelives,base-repo|}/compare/${2|textmate,branch|}...tbates:${3|fix,feature|}-${4:$2}
	paste0("github.com/", base_owner, "/", base_repo, "/compare/", base_branch, "...,", head_owner, ":", head_branch)
}
