#' Read and optionally merge demographics file from prolific academic
#'
#' prolific academic provides a demographics file. This reads it and merges with your data
#' using PID and participant_id
#'
#' @param file Path to demographics file.
#' @param base Optional path to folder, in which case 'file' is just filename.
#' @param df Existing datafile to merge demographics into (optional)
#' @param by.df The ID name in existing df (default = "PROLIFIC_PID")
#' @param by.demog The ID name in the prolific demographics file (default = "Participant id" was `by.demog`)
#' @param age Name of age var in demographics file ("age")
#' @param sex Name of sex var in demographics file ("Sex")
#' @param vars Additional vars to keep from demographics file (WAS age & Sex)
#' @param all.df Whether to keep all lines of df (default = TRUE)
#' @param all.demog Whether to keep all lines (people) in the demographics file (default = FALSE)
#' @param verbose Print variable names found in the file.
#' @return - [[data.frame]]
#' @seealso - [prolific_check_ID()], [prolific_anonymize()], [umx_merge_randomized_columns()]
#' @export
#' @family Data Functions
#' @references <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @examples
#' \dontrun{
#' fp = "~/Desktop/prolific_export_5f20c3e662e3b6407dcd37a5.csv"
#' df = prolific_read_demog(fp, sex = "Gender", age = "Age", df = df)
#' tmp = prolific_read_demog(fp, by.df = "PROLIFIC_PID", vars=c("Ethnicity.simplified"))
#' }
prolific_read_demog <- function(file, base = "", df = NULL, by.df = "PROLIFIC_PID", by.demog = "Participant.id", age = "age", sex = "Gender", vars= NULL, all.df = TRUE, all.demog = FALSE, verbose = FALSE) {
	if(base != "") file = paste0(base, file)
	newdf = read.csv(file, header= TRUE, sep = ',', quote = "\"", dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE, na.strings = c("NA", "DATA_EXPIRED"))
	if(verbose) print(namez(newdf))
	allNames = umx_check_names(namesNeeded = c(vars,age, sex), data = newdf, message="Checking demographics col names", die=FALSE)
	if(!allNames){
		namesNeeded = c(vars,age, sex)
		missing = namesNeeded[!namesNeeded %in% colnames(newdf)]
		print(paste0("Asked for: ", omxQuotes(c(vars,age, sex))))
		print(namez(newdf))
		stop(length(missing), " columns missing: ", paste(omxQuotes(missing), collapse=", "))
	}

	# Replace "CONSENT_REVOKED" with NA
	# TODO check each var name?
	tmp = newdf[, allNames]
	bad = tmp == "CONSENT_REVOKED"
	tmp[bad] = NA
	newdf[, allNames] = tmp
	# repair age if needed
	newdf[, age] = as.numeric(newdf[, age])
	if(!is.null(df)){
		umx_check_names(namesNeeded = by.df, data = df)
		umx_check_names(namesNeeded = by.demog, data = newdf)
		newdf = merge(df, newdf[, c(by.demog, age, sex, vars)], by.x = by.df, by.y = by.demog, all.x = all.df, all.y = all.demog, suffixes = c("", ".y"))
	}else{
		newdf = newdf[, c(by.demog, age, sex, vars)]
	}

	if(age %in% namez(df)){
		age = paste0(age, ".y")
	}
	if(sex %in% namez(df)){
		sex = paste0(sex, ".y")
	}

	# May as well print out a nice subjects section...
	# "Man (including Trans Male/Trans Man)"

	jnk = umx_aggregate(eval(parse(text= paste0(age, "~", sex))), data = newdf)
	print(jnk)

	
	fStr = namez(xmu_read.markdown(jnk)[1,1], pattern = ".*Female \\(n = ([0-9]+)\\)", replacement = "\\1") # "Female (n = 351)"
	mStr = namez(xmu_read.markdown(jnk)[2,1], pattern =   ".*Male \\(n = ([0-9]+)\\)", replacement = "\\1") # "Male (n = 351)"

	tmp = newdf; tmp$one = 1
	jnk = umx_aggregate(eval(parse(text= paste0(age, " ~ one"))), data = tmp)
	print(jnk)
	Nstr = namez(xmu_read.markdown(jnk)[1,1], pattern = "1 \\(n = ([0-9]+)\\)", replacement = "\\1") # "1 (n = 703)"
	ageStr = xmu_read.markdown(jnk)[1,2] # "48.32 (12.92)"
	message(paste0("Subjects were ", Nstr, " prolific volunteers (", mStr," male, ", fStr, " female; mean age ", ageStr , " years)"))
	# "Subjects were n prolific volunteers ( m  male f female, mean age  yrs years)"
	invisible(newdf)
}

xmu_read.markdown <- function(file, stringsAsFactors = FALSE, strip.white = TRUE, ...){
	if (length(file) > 1) {
		lines <- file
	} else if (grepl('\n', file)) {
		con <- textConnection(file)
		lines <- readLines(con)
		close(con)
	} else {
		lines <- readLines(file)
	}
	lines <- lines[!grepl('^[[:blank:]+-=:_|]*$', lines)]
	lines <- gsub('(^\\s*?\\|)|(\\|\\s*?$)', '', lines)
	read.delim(text = paste(lines, collapse = '\n'), sep = '|', stringsAsFactors = stringsAsFactors, strip.white = strip.white, ...)
}

#' Clean up a prolific file for sharing by removing anonymity-compromising columns.
#'
#' prolific.ac IDs and other columns like IP and lat/long might compromise 
#' subject anonymity when shared.
#' `prolific_anonymize` replaces PIDs with a simple numeric sequence, preserving
#' repeated measures in long data, and removing other columns.
#' You can delete additional  columns by adding them to `extraColumns`. It is ideal for use 
#' when sharing data to ResearchBox (researchbox.org) which enforces anonymization.
#'
#' @param df Existing datafile to anonymize.
#' @param PID The prolific ID col name to anonymize
#' @param alsoDrop Any extra columns to delete (default NA)
#' @param baseOffset The numeric to start renumbering PIDs from (default = 1e4)
#' @param extraColumns A deprecated synonym for alsoDrop
#' @return - [[data.frame]]
#' @seealso - [prolific_check_ID()], [prolific_read_demog()], [umx_merge_randomized_columns()] 
#' @export
#' @family Data Functions
#' @references <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @examples
#' \dontrun{
#' tmp = prolific_anonymize(df, PID = "PID")
#' }
prolific_anonymize <- function(df = NULL, PID = "PID", alsoDrop = NA, baseOffset = 1e4, extraColumns = "deprecated"){
	revealingColumns = c("StartDate", "EndDate", "Status", "IPAddress", "Progress", "Duration..in.seconds.", "Finished", "RecordedDate", "ResponseId", "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalReference", "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage", "QID1210817776", "PROLIFIC_PID", "PID")
	if(extraColumns!= "deprecated"){
		umx_msg("Polite message: in future, please use 'alsoDrop= ' in place of 'extraColumns' ")
		alsoDrop = extraColumns
	}
	# cleanup revealingColumns
	if(PID %in% revealingColumns){
		message("Keeping original PID column")
		revealingColumns = revealingColumns[!revealingColumns==PID]
	}
	isPIDInNames   = umx_check_names(PID, df, die = FALSE)
	areExtrasFound = umx_check_names(alsoDrop, df, die = TRUE)
	
	if(isPIDInNames){
		# Anonymize the PID column
		oldValues = df[,PID]
		if(anyDuplicated(df[, PID])){
			message("Some IDs were duplicates. That pattern will be preserved")
			uniqueIDs = unique(oldValues)
			newIDs    = c((baseOffset+1):(baseOffset + length(uniqueIDs)) )
			lookuptbl = setNames(newIDs, uniqueIDs)
			df[,PID]  = lookuptbl[as.character(oldValues)]
		} else {
			message("No duplicates")
			newIDs   = c((baseOffset+1): (baseOffset + length(oldValues) ) )
			df[,PID] = newIDs
		}
	} else {
		message("No PID column in names: making one")
		# PID was not found, assume this df has no ID column so invent one. But this is super unusal do tell the user!
		df[,PID] = c((baseOffset+1): (baseOffset + dim(df)[1]) )
		message("Polite note: I did not find a PID column. So I created one called ", PID, " and stored anonymous sequential number IDs there")
	}
	# Clean up
	df = df[, names(df)[!names(df) %in% c(revealingColumns, alsoDrop)]]
	message("OK, what's left now is:")
	message(omxQuotes(names(df)))
	invisible(df)
}

#' Return PIDs in df
#'
#' Participants may time-out on Prolific, but still complete on Qualtrics. This identifies them.
#'
#' @param IDs Timed-out (or other) IDs to look for.
#' @param df to search.
#' @param IDcol Name of prolific ID column (default PROLIFIC_PID)
#' @return - list of IDs in the dataframe
#' @export
#' @seealso - [prolific_read_demog()], [prolific_anonymize()], [umx_merge_randomized_columns()] # [prolific_check_ID()]
#' @family Data Functions
#' @examples
#' # IDs = c("59d0ec2446447f00011edb063","5a08c9a7f2e3460001edb063f0254")
#' # prolific_check_ID(IDs, df)
prolific_check_ID <- function(IDs, df, IDcol = "PROLIFIC_PID") {
	IDs[which(IDs %in% df[, IDcol, drop = TRUE])]
}

#' prolific_scoring_stub
#'
#' Automatically generates a scoring stub for scales in a Prolific/Qualtrics dataset.
#'
#' @param df The data frame imported from Qualtrics/Prolific.
#' @param deleteJunk Logical. If TRUE, return the dataframe with the metadata/junk columns removed. (default = FALSE)
#' @return The data frame (invisibly). If deleteJunk = TRUE, junk columns are removed.
#' @export
#' @family Data Functions
#' @references \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}
#' @examples
#' \dontrun{
#' prolific_scoring_stub(df, deleteJunk = FALSE)
#' }
prolific_scoring_stub <- function(df = NULL, deleteJunk = FALSE) {
	if (is.null(df)) {
		stop("Please provide a valid dataframe.")
	}
	
	dfName = deparse(substitute(df))
	if (length(dfName) > 1 || !grepl("^[a-zA-Z_][a-zA-Z0-9._]*$", dfName)) {
		dfName = "df"
	}
	
	qualtricsJunk = c(
		"StartDate", "EndDate", "Status", "IPAddress", "Progress",
		"Duration..in.seconds.", "Finished", "RecordedDate", "ResponseId",
		"RecipientLastName", "RecipientFirstName", "RecipientEmail",
		"ExternalReference", "LocationLatitude", "LocationLongitude",
		"DistributionChannel", "UserLanguage"
	)
	
	# Identify columns matching the metadata list or ^QID[0-9]+$
	allColNames = names(df)
	isJunk = allColNames %in% qualtricsJunk | grepl("^QID[0-9]+$", allColNames)
	junkCols = allColNames[isJunk]
	candidateCols = allColNames[!isJunk]
	
	# Helper to format contiguous/non-contiguous items
	formatItems <- function(items) {
		items = sort(as.integer(items))
		if (length(items) == 0) {
			return("NULL")
		}
		if (length(items) == 1) {
			return(as.character(items))
		}
		if (all(diff(items) == 1)) {
			return(paste0(items[1], ":", items[length(items)]))
		} else {
			return(paste0("c(", paste(items, collapse = ", "), ")"))
		}
	}
	
	# Helper to score Likert options for sorting
	scoreOption <- function(opt) {
		optLower = tolower(opt)
		score = 0
		
		# Check neutral first to prevent "Neither agree nor disagree" matching "disagree"
		isNeu = grepl("neither|neutral|undecided|uncertain|dont know|don't know|no opinion|unopinionated", optLower)
		isNeg = grepl("disagree|disapprove|false|no|never|rarely|unlikely|bad|negative", optLower)
		isPos = grepl("agree|approve|true|yes|always|often|likely|good|positive", optLower)
		
		if (isNeu) {
			score = 0
		} else if (isNeg) {
			score = -10
			if (grepl("strongly|very|extremely|completely|definitely", optLower)) {
				score = score - 5
			}
			if (grepl("somewhat|slightly|moderately|mildly", optLower)) {
				score = score + 5
			}
		} else if (isPos) {
			score = 10
			if (grepl("strongly|very|extremely|completely|definitely", optLower)) {
				score = score + 5
			}
			if (grepl("somewhat|slightly|moderately|mildly", optLower)) {
				score = score - 5
			}
		} else {
			# Fallbacks for other scale types
			if (grepl("never", optLower)) score = -10
			if (grepl("rarely|seldom", optLower)) score = -5
			if (grepl("sometimes|occasionally", optLower)) score = 0
			if (grepl("often|frequently", optLower)) score = 5
			if (grepl("always|constantly", optLower)) score = 10
		}
		return(score)
	}
	
	# Helper to sort Likert options
	smartSortOptions <- function(options) {
		options = options[!is.na(options) & options != ""]
		if (length(options) == 0) {
			return(character(0))
		}
		asNums = suppressWarnings(as.numeric(options))
		if (!any(is.na(asNums))) {
			return(options[order(asNums)])
		}
		scores = sapply(options, scoreOption)
		return(options[order(scores, options)])
	}
	
	# Age and Sex detection and recoding
	recodingCode = character(0)
	ageIdx = grep("^age$", candidateCols, ignore.case = TRUE)
	if (length(ageIdx) > 0) {
		ageCol = candidateCols[ageIdx[1]]
		if (is.character(df[[ageCol]]) || is.factor(df[[ageCol]])) {
			recodingCode = c(recodingCode, paste0(dfName, "$", ageCol, " = as.numeric(", dfName, "$", ageCol, ")"))
		}
	}
	sexIdx = grep("^sex$", candidateCols, ignore.case = TRUE)
	if (length(sexIdx) > 0) {
		sexCol = candidateCols[sexIdx[1]]
		if (is.character(df[[sexCol]])) {
			recodingCode = c(recodingCode, paste0(dfName, "$", sexCol, " = as.factor(", dfName, "$", sexCol, ")"))
		}
	}
	
	# Print age/sex recoding if found
	if (length(recodingCode) > 0) {
		message("# Recode demographic columns to numeric/factor")
		message(paste(recodingCode, collapse = "\n"))
		message("")
	}
	
	# Extract bases and item numbers
	bases = character(0)
	itemsByBase = list()
	
	for (i in seq_along(candidateCols)) {
		colName = candidateCols[i]
		# 1. Try matching with a delimiter followed by digits at the end
		m = regmatches(colName, regexec("^(.*[._-])([0-9]+)$", colName))[[1]]
		if (length(m) == 3) {
			base = m[2]
			itemNum = as.integer(m[3])
		} else {
			# 2. Try matching with non-digits followed by digits at the end
			m = regmatches(colName, regexec("^([^0-9]+)([0-9]+)$", colName))[[1]]
			if (length(m) == 3) {
				base = m[2]
				itemNum = as.integer(m[3])
			} else {
				# 3. Fallback: match any characters followed by digits
				m = regmatches(colName, regexec("^(.*)([0-9]+)$", colName))[[1]]
				if (length(m) == 3) {
					base = m[2]
					itemNum = as.integer(m[3])
				} else {
					next
				}
			}
		}
		
		if (!base %in% bases) {
			bases = c(bases, base)
			itemsByBase[[base]] = list(items = integer(0), columns = character(0))
		}
		itemsByBase[[base]]$items = c(itemsByBase[[base]]$items, itemNum)
		itemsByBase[[base]]$columns = c(itemsByBase[[base]]$columns, colName)
	}
	
	# Filter scale bases which have at least 2 items
	detectedScales = character(0)
	for (base in bases) {
		items = itemsByBase[[base]]$items
		cols = itemsByBase[[base]]$columns
		if (length(items) >= 2) {
			# Sort both items and corresponding column names by item number
			ord = order(items)
			items = items[ord]
			cols = cols[ord]
			
			cleanName = gsub("[._-]$", "", base)
			strVarName = paste0(cleanName, "STR")
			
			# Extract responses across all items in scale
			allResponses = unlist(lapply(cols, function(c) as.character(df[[c]])))
			uniqueResponses = unique(allResponses)
			uniqueResponses = uniqueResponses[!is.na(uniqueResponses) & uniqueResponses != ""]
			sortedResponses = smartSortOptions(uniqueResponses)
			
			# Generate options string
			optStr = paste0(strVarName, " = ", paste(deparse(sortedResponses), collapse = ""))
			
			# Generate umx_score_scale call
			posStr = formatItems(items)
			maxVal = length(sortedResponses)
			if (maxVal == 0) {
				maxVal = "NULL"
			}
			
			scoreCall = paste0(
				dfName, " = umx_score_scale(name = ", deparse(cleanName),
				", base = ", deparse(base),
				", pos = ", posStr,
				", rev = NULL, min = 1, max = ", maxVal,
				", data = ", dfName,
				", score = \"mean\", mapStrings = ", strVarName,
				", na.rm = TRUE, alpha = TRUE)"
			)
			
			message(optStr)
			message(scoreCall)
			message("")
			detectedScales = c(detectedScales, cleanName)
		}
	}
	
	if (length(detectedScales) == 0) {
		message("# No multi-item scales detected.")
	}
	
	if (deleteJunk) {
		remainingCols = allColNames[!allColNames %in% junkCols]
		invisible(df[, remainingCols, drop = FALSE])
	} else {
		invisible(df)
	}
}
