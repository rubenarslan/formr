#' Gives the first non-missing element
#'
#' Just a simple shorthand to get the first, non-missing argument per default.
#' Can give more than one element and can include missing elements.
#' The inverse of \code{\link{last}}.
#'
#' @param x vector of which you want the first element
#' @param n number of elements to take from the beginning
#' @param na.rm whether to remove missings first, defaults to TRUE
#' @export
#' @examples
#' first( c(NA,1:10) )
#' last( c(NA, 1:10), 2, TRUE )

first = function(x, n = 1, na.rm = TRUE) {
    if(na.rm) x = na.omit(x)
    head(x, n)
}

#' Gives the last non-missing element
#'
#' Just a simple shorthand to get the last, non-missing argument per default.
#' Can give more than one element and can include missing elements.
#' The inverse of \code{\link{first}}.
#'
#' @param x vector of which you want the last element
#' @param n number of elements to take from the end
#' @param na.rm whether to remove missings first, defaults to TRUE
#' @export
#' @examples
#' last( c(1:10,NA) )
#' last( c(1:10,NA), 2, TRUE )

last = function(x, n = 1, na.rm = TRUE) {
    if(na.rm) x = na.omit(x)
    tail(x, n)
}

#' Gives the last element, doesn't omit missings
#'
#' Just a simple shorthand to get the current element (in a formr df,
#' where the last element is always the one from the current session).
#'
#' @param x vector of which you want the current element
#' @export
#' @examples
#' current( c(1:10,NA) )
#' current( 1:10 )
current = function(x) {
    tail(x, 1)
}


#' check whether a character string contains another
#'
#' Just a simple shorthand so that inexperienced R users don't have
#' to use somewhat complex functions such as \code{\link{grepl}} and \code{\link[stringr:str_detect]{str_detect}}
#' with non-default arguments (e.g. fixed params).
#'
#' @param haystack string in which you search
#' @param needle string to search for
#' @export
#' @examples
#' "1, 2, 3, 4, you" %contains% "you"
#' "1, 2, 3, 4, you" %contains% 1 # unlike str_detect casts all needles as characters
#' "1, 2, 3, 4, you" %contains% 343

"%contains%" = function(haystack, needle) {
    stringr::str_detect(haystack, stringr::fixed(as.character(needle)) )
}

#' check whether a character string contains another as a word
#'
#' Looks for a string appearing on its own. This is needed e.g.
#' when checking whether the replies to a mmc item, stored as a 
#' comma-separated list from 1 to 12 contain option 1 - you wouldn't
#' want to get a hit for 11 and 12.
#' Only works for search terms containing alphanumeric characters.
#' Just a simple shorthand so that inexperienced R users don't have
#' to use somewhat complex functions such as \code{\link{grepl}} and \code{\link[stringr:str_detect]{str_detect}}.
#'
#' @param haystack string in which you search
#' @param needle string to search for
#' @export
#' @examples
#' "1, 3, 4" %contains_word% "1" # TRUE
#' "1, 3, 4" %contains_word% 1 # TRUE unlike str_detect casts all needles as characters
#' "12, 14, 17" %contains_word% "1" # FALSE even though 12 contains 1

"%contains_word%" = function(haystack, needle) {
	stringr::str_detect(haystack, paste0("\\b",Hmisc::escapeRegex(as.character(needle)),"\\b") )
}

#' check whether a character string begins with a string
#'
#' Escapes any special RegExp characters in the search term. A way to check whether the search term (e.g. a variable name) is the beginning.
#' Just a simple shorthand so that inexperienced R users don't have
#' to use somewhat complex functions such as \code{\link{grepl}} and \code{\link[stringr:str_detect]{str_detect}}.
#'
#' @param haystack string in which you search
#' @param needle string to search for
#' @export
#' @examples
#' "1, 3, 4" %begins_with% "1" # TRUE
#' "1, 3, 4" %begins_with% 1 # unlike str_detect casts all needles as characters
#' "1, 3, 4" %begins_with% "." # FALSE

"%begins_with%" = function(haystack, needle) {
	stringr::str_detect(haystack, paste0("^",Hmisc::escapeRegex(as.character(needle))) )
}

#' check whether a character string ends with a string
#'
#' Escapes any special RegExp characters in the search term. A way to check whether the search term (e.g. a variable name) is the ending.
#' Just a simple shorthand so that inexperienced R users don't have
#' to use somewhat complex functions such as \code{\link{grepl}} and \code{\link[stringr:str_detect]{str_detect}}.
#'
#' @param haystack string in which you search
#' @param needle string to search for
#' @export
#' @examples
#' "1, 3, 4" %ends_with% "4" # TRUE
#' "1, 3, 4" %ends_with% 4 # unlike str_detect casts all needles as characters
#' "1, 3, 4" %ends_with% "." # FALSE

"%ends_with%" = function(haystack, needle) {
	stringr::str_detect(haystack, paste0(Hmisc::escapeRegex(as.character(needle)),"$") )
}


#' percentage of missings for each variable in a data.frame
#'
#' This functions simply reports the number of missings as the
#' percentage of the maximum number of rows.
#' It also works on single variables.
#'
#' @param df data.frame or variable
#' @param vars subset of variables, defaults to all
#' @export
#' @examples
#' fruits = c("apple", "banana", NA, "pear", "pinapple", NA)
#' pets = c("cat", "dog", "anteater", NA, NA, NA)
#' favorites = data.frame(fruits, pets)
#' miss_frac(favorites)
#' miss_frac(favorites$fruits)
#' miss_frac(favorites, 2)

miss_frac = function(df, vars = 1:NCOL(df)) { 
    if(NCOL(df) == 1) fracts = sum(is.na(df))
    else if(NCOL(df[,vars]) == 1) fracts = sum(is.na(df[,vars]))
    else fracts = colSums( plyr::colwise(is.na)(df[,vars]) )
    fracts / NROW(df)
}

#' aggregates two variables from two sources into one
#'
#' Takes two variables with different missings
#' and gives one variable with values of the second
#' variable substituted where the first had missings.
#'
#' @param df data.frame or variable
#' @param new_var new variable name
#' @param var1 first source. Assumed to be new_var.x (default suffixes after merging)
#' @param var2 second source. Assumed to be new_var.y (default suffixes after merging)
#' @param remove_old_variables Defaults to not keeping var1 and var2 in the resulting df.
#' @export
#' @examples
#' cars$dist.x = cars$dist
#' cars$dist.y = cars$dist
#' cars$dist.y[2:5] = NA
#' cars$dist.x[10:15] = NA # sprinkle missings
#' cars$dist = NULL # remove old variable
#' cars = aggregate2sources(cars, 'dist')

aggregate2sources = function(df, new_var, var1 = NULL, var2 = NULL, remove_old_variables = TRUE) {
	if(is.null(var1) && is.null(var2)) {
		var1 = paste0(new_var,".x")
		var2 = paste0(new_var,".y")
	}
    if(exists(new_var, where = df)) {
        warning(paste(new_var,"already exists. Maybe delete it or choose a different name, if you're saving over your original dataframe."))
    }
	df[, new_var ] = df[ , var1 ]
	oldmiss = sum(is.na(df[, new_var]))
	df[ is.na( df[, var1] ) , new_var] = df[ is.na( df[, var1] ) , var2]
    
    if(remove_old_variables) {
    	df[, var1] = NULL
    	df[, var2] = NULL
    }
	
	message( paste(oldmiss - sum(is.na(df[, new_var]))	, " fewer missings") )
	df
}

#' loads an RDS object, assigns it to an object of the base-filename
#'
#' \code{\link{saveRDS}} saves an object to a file, so unlike \code{\link{save}} and \code{\link{load}} you can assign the loaded object to a new variable using \code{\link{readRDS}}. 
#' However, sometimes it may be more convenient to assign the object in the RDS file to an object of the same name as the file. This is what \code{\link{loadRDS}} does. It extracts the filename using \code{\link{basename}} and \code{\link[tools:file_path_sans_ext]{file_path_sans_ext}}
#'
#' @param file path to file
#' @param refhook passed to readRDS
#' @param overwrite whether to overwrite an existing object of the same name. defaults to false.
#' @export
#' @examples
#' \dontrun{
#' loadRDS(file = "~/Models/Spouses.rds") # assigns object contained in file to variable "Spouses"
#' }

loadRDS = function(file, refhook = NULL, overwrite = FALSE) {
	object_name = basename(tools::file_path_sans_ext(file))
	if(exists(object_name, envir = parent.frame(), inherits = F) & !overwrite) {
		warning(paste(object_name, "would have been overwritten. Specify overwrite = TRUE to do so."))
	} else {
	assign(object_name,	readRDS(file, refhook), envir = parent.frame())
	}
}

#' @export
.formr <- new.env()
#' @export
.formr$last_action_time <- NULL
#' @export
.formr$last_action_date <- NULL

#' checks how much time has passed relative to the user's last action
#'
#' checks how much time has passed. You can choose the unit. Implemented via \code{\link[lubridate:dseconds]{dseconds}}, not periods, i.e. a minute has 60 seconds, an hour 60 minutes, a day 24 hours. No months and other uncertain time spans.
#'
#' @param seconds argument to \code{\link[lubridate:dseconds]{dseconds}}
#' @param minutes 60 seconds
#' @param hours 60 minutes
#' @param days 24 hours
#' @param time defaults to .formr$last_action_time, a hidden variable that is automatically set by formr.org
#' @export
#' @examples
#' time_passed(hours = 7, time = Sys.time())

time_passed = function(days = 0, hours = 0, minutes = 0, seconds = 0, time = NULL) {
	if(is.null(time) & !is.null(.formr$last_action_time)) 
		time = .formr$last_action_time
	time = as.POSIXct(time)
	stopifnot(!is.null(time))
	(time + 
	 	lubridate::dseconds( seconds + 
	 												60* minutes + 
	 												60*60* hours + 
	 												60*60*24* days ) 
	) < lubridate::here() # local time
}

#' checks whether a new day has broken (date has increased by at least one day)
#'
#' a simple utility functions to avoid that looped Skip Backwards/Skip Forwards in formr are true repeatedly.
#'
#' @param date defaults to .formr$last_action_date, a hidden variable that is automatically set by formr.org. Will be coerced to POSIXct.
#' @export
#' @examples
#' next_day(Sys.time()) # always false

next_day = function(date = NULL) {
	if(is.null(date) & !is.null(.formr$last_action_date)) date = .formr$last_action_date
	stopifnot(!is.null(date))
	date = lubridate::floor_date(as.POSIXct(date))
	date < lubridate::floor_date(lubridate::now(),"day")
}

#' checks whether the current time is in a certain time window
#'
#' supply min,max as POSIXct
#'
#' @param min POSIXct < max
#' @param max POSIXct > min
#' @export
#' @examples
#' in_time_window(Sys.time() - 1, Sys.time() + 1)

in_time_window = function(min, max) {
	min < lubridate::here() && max > lubridate::here()
}

#' generates valid email cids
#'
#' can be used as an argument to \code{\link[knitr:opts_knit]{opts_knit}}. If you attach the images properly, you can then send knit emails including plots. See the formr OpenCPU module on Github for a sample implementation.
#'
#' @param x image ID
#' @param ext extension, defaults to .png
#' @export
#' @examples
#' \dontrun{
#' library(knitr); library(formr)
#' opts_knit$set(upload.fun=formr::email_image)
#' }

email_image = function(x, ext = '.png') {
	cid = gsub("[^a-zA-Z0-9]", "", substring(x,8))
	structure(paste0("cid:",cid,ext), link = x)
}

#' pass in the url to the RDS representation of a openCPU session object, get the object
#'
#' useful to programmatically access openCPU session object stored in character variables etc.
#'
#' @param session_url the session url, e.g. https://public.opencpu.org/ocpu/tmp/x02a93ec/R/.val/rds
#' @param local defaults to FALSE, if true, will assume that the session is not on another server, and do some not-very-smart substitution to load it via the file system instead of HTTP/HTTPS
#' @export
#' @examples
#' \dontrun{
#' get_opencpu_rds("https://public.opencpu.org/ocpu/tmp/x02a93ec/R/.val/rds")
#' }
get_opencpu_rds= function(session_url, local = TRUE) {
	if(local) {
		sessionenv <- new.env();
		filepath = stringr::str_match(session_url, "/ocpu/tmp/([xa-f0-9]+)/([a-z0-9A-Z/.]+)")
		sessionfile <- file.path("/tmp/ocpu-www-data/tmp_library", filepath[,2], ".RData")
		if(file.exists(sessionfile)){
			load(sessionfile, envir=sessionenv);
			desired_obj = stringr::str_sub(filepath[,3],3, -5)
			sessionenv[[desired_obj]]
		}
	} else {
		readRDS(gzcon(curl::curl(session_url)))
	}
}

#' xtabs with sensible defaults
#'
#' xtabs requires two arguments (na.action and exclude) to show missing values along with other values. This function defaults to including missings and has only one argument 
#'
#' @param x passed to xtabs if it is a formula, transformed into a formula if it's a single object
#' @param ... passed to xtabs
#' @param exclude defaults to NULL (i.e. includes NA)
#' @export
#' @examples
#' x = NA
#' crosstabs(~ x)
crosstabs = function(x, ..., exclude = NULL) {
	if(!inherits(x,'formula')) x = as.formula(paste("~",deparse(substitute(x))),env = parent.frame())
	xtabs(formula = x, ..., na.action = na.pass, exclude = exclude)
}

#' proportions table
#'
#' quick and easy function  to show proportions of values of a variable,
#' defaults to including missings
#'
#' @param ... passed to crosstabs
#' @param exclude defaults to NULL (i.e. includes NA)
#' @export
#' @examples
#' x = NA
#' props(~ x)
props = function(..., exclude = NULL) { 
	prop.table(crosstabs(..., exclude = NULL))
}

#' take only nonmissing
#'
#' this function takes a subset of a dataset, omitting all
#' cases with missings in variables specified in "keep"
#' and omitting all variables that still have missings after that.
#' Good to see how large your dataset for a certain analysis 
#' will be and which covariates are "free" in terms of sample size.
#'
#' @param df dataset
#' @param keep defaults to empty vector
#' @export
#' @examples
#' data(ChickWeight)
#' ChickWeight[1:2,c('weight','Chick')] = NA
#' ChickWeight[3:4,'Diet'] = NA
#' names(ChickWeight); nrow(ChickWeight)
#' ChickWeight2 = take_nonmissing(ChickWeight, keep = c('weight'))
#' names(ChickWeight2); nrow(ChickWeight2)
take_nonmissing = function(df, keep = c()) {
	df = df[rowSums(is.na(subset(df, select = keep, drop=F)))==0, ] # omit all cases with missings in keep
	df = subset(df, select = names(which(colSums(is.na(df))==0)), drop = F) # omit all variables with missings
}



#' missingness patterns
#'
#' this function shows how common possible missingness patterns are. Emulates misschk in stata.
#' 1. excludes any variables that don't have any missings, so as not to clutter output. Disable using omit_complete
#' 2. sorts variables by number of missings, so that the usual suspects show up at the front.
#' 3. displays number of missings accounted for by each pattern
#'
#' @param df dataset
#' @param min_freq show only patterns that occur at least this often. Defaults to 1 observation.
#' @param long_pattern by default (FALSE) only shows column indices for space and legibility reasons.
#' @param print_legend prints a legend for the column indices, defaults to FALSE if long_pattern is set
#' @param show_culprit defaults to TRUE. In case a missingness pattern boils down to one variable, it will be shown here.
#' @param relative defaults to FALSE. If true, percentages are shown (relative to total before excluding minimum frequency).
#' @param omit_complete defaults to TRUE. Columns that don't have any missings are excluded.
#' @export
#' @examples
#' data(ChickWeight)
#' ChickWeight[1:2,c('weight','Chick')] = NA
#' ChickWeight[3:5,'Diet'] = NA
#' names(ChickWeight); nrow(ChickWeight)
#' missingness_patterns(ChickWeight)
missingness_patterns = function(df,  min_freq = ifelse(relative,1/nrow(df),1), long_pattern = FALSE, print_legend = ifelse(long_pattern, FALSE, TRUE), show_culprit = TRUE, relative = FALSE, omit_complete = TRUE) {
	missings_by_column = colSums(is.na(df))
	if (omit_complete) {
		takethese = missings_by_column != 0
	} else {
		takethese = TRUE
	}
	names(missings_by_column) = names(df)
	missings_by_column = sort(missings_by_column[ takethese  ],decreasing = T)
	any_missing_sorted = names(missings_by_column)
	df = subset(df, select = any_missing_sorted)
	cols = names(df)
	if (length(cols) == 0) {
		cat("No missings at all.\n")
		return(invisible(NULL))
	}
	df = !is.na(df)
	if (min_freq > 0) {
		counted = plyr::count(df)		
		names(counted) = c(cols, "Freq")
	} else {
		counted = as.data.frame( xtabs( data = df) )
	}
	if (relative) {
		counted$Freq = counted$Freq/sum(counted$Freq)
	}
	counted = counted[counted$Freq >= min_freq, ]
	pattern = character(length = nrow(counted))
	if (show_culprit) {
		culprit = rep(x = '_', nrow(counted))
	}
	for (i in 1:length(cols)) {
		if (show_culprit) {
		culprit[counted[,i] == "FALSE"] = ifelse(culprit[counted[,i] == "FALSE"] == "_", cols[i], "") # if it's a _, set it, if it's set, set it to empty
		}
		nr = as.character(i)
		pattern = paste0(pattern, ifelse(i == 1, '', '_'), ifelse(counted[,i] == "TRUE", stringr::str_pad('', stringr::str_length(nr), pad = '_') , nr ))
	}
	missingness = data.frame(Pattern = pattern, Freq = counted$Freq, Culprit = culprit)
	
	if (long_pattern == TRUE) {
		long_pattern = character(length = nrow(counted))
		for (i in 1:length(cols)) {
			long_pattern = paste0(long_pattern, ifelse(counted[,i] == "TRUE", "_", paste0(cols[i],".")))
		}
		missingness$Pattern = long_pattern
	}
	if (print_legend) {
		print(data.frame(index = 1:length(cols),col = cols, missings = missings_by_column), row.names = FALSE)
	}
	missingness = missingness[order(missingness$Freq,decreasing = T),]
	rownames(missingness) = NULL
	missingness
}