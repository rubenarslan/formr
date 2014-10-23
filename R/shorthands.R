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
    round( fracts / NROW(df) , 2) 
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

.formr.last_action_time = NA
.formr.last_action_date = NA

#' checks how much time has passed relative to the user's last action
#'
#' checks how much time has passed. You can choose the unit. Implemented via \code{\link[lubridate:dseconds]{dseconds}}, not periods, i.e. a minute has 60 seconds, an hour 60 minutes, a day 24 hours. No months and other uncertain time spans.
#'
#' @param seconds argument to \code{\link[lubridate:dseconds]{dseconds}}
#' @param minutes 60 seconds
#' @param hours 60 minutes
#' @param days 24 hours
#' @param time defaults to .formr.last_action_time
#' @export
#' @examples
#' time_passed(hours = 7, time = Sys.time())

time_passed = function(days = 0, hours = 0, minutes = 0, seconds = 0, time = .formr.last_action_time) {
	(time + 
	 	lubridate::dseconds( seconds + 
	 												60* minutes + 
	 												60*60* hours + 
	 												60*60*24* days ) 
	) < lubridate::here() # local time
}

#' checks whether a new day has broken (date has changed)
#'
#' a simple utility functions to avoid that looped Skip Backwards/Skip Forwards in formr are true repeatedly.
#'
#' @param date defaults to .formr.last_action_date
#' @export
#' @examples
#' next_day(as.Date(Sys.time()))

next_day = function(date = .formr.last_action_date) {
	date < lubridate::today("")
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
#' library(knitr); library(formr)
#' opts_knit$set(upload.fun=formr::email_image)

email_image = function(x, ext = '.png') {
	cid = gsub("[^a-zA-Z0-9]", "", substring(x,8))
	structure(paste0("cid:",cid,ext), link = x)
}