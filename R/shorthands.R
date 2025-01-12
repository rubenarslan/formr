#' Gives the first non-missing element
#'
#' Just a simple shorthand to get the first, non-missing argument per default.
#' Can give more than one element and can include missing elements.
#' The inverse of [last()].
#'
#' @param x vector of which you want the first element
#' @param n number of elements to take from the beginning
#' @param na.rm whether to remove missings first, defaults to TRUE
#' @export
#' @examples
#' first( c(NA,1:10) )
#' first( c(NA, 1:10), 2, TRUE )

first = function(x, n = 1, na.rm = TRUE) {
  if (na.rm) 
    x = stats::na.omit(x)
  utils::head(x, n)
}

#' Gives the last non-missing element
#'
#' Just a simple shorthand to get the last, non-missing argument per default.
#' Can give more than one element and can include missing elements.
#' The inverse of [first()].
#'
#' @param x vector of which you want the last element
#' @param n number of elements to take from the end
#' @param na.rm whether to remove missings first, defaults to TRUE
#' @export
#' @examples
#' last( c(1:10,NA) )
#' last( c(1:10,NA), 2, TRUE )

last = function(x, n = 1, na.rm = TRUE) {
  if (na.rm) 
    x = stats::na.omit(x)
  utils::tail(x, n)
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
  utils::tail(x, 1)
}


#' How many surveys were finished?
#'
#' Just a simple to check how many times a survey (e.g. diary)
#' was finished. It defaults to checking the "ended" variable for this.
#'
#' @param survey which survey are you asking about?
#' @param variable which variable should be filled out, defaults to "ended"
#' @export
#' @examples
#' survey = data.frame(ended = c("2016-05-28 10:11:00", NA, "2016-05-30 11:18:28"))
#' finished(survey = survey)
finished = function(survey, variable = "ended") {
	if (length(survey) > 0) {
		if (length(survey[, variable]) > 0) {
			sum(!is.na(survey[, variable]))
		} else {
			0
		}
	} else {
		0
	}
}

#' How many surveys were expired?
#'
#' Just a simple to check how many times a survey (e.g. diary)
#' has expired (i.e. user missed it). It defaults to checking the "expired" variable for this.
#'
#' @param survey which survey are you asking about?
#' @param variable which variable should be filled out, defaults to "ended"
#' @export
#' @examples
#' survey = data.frame(expired = c(NA, "2016-05-29 10:11:00", NA))
#' expired(survey = survey)
expired = function(survey, variable = "expired") {
	finished(survey, variable)
}

#' check whether a character string contains another
#'
#' Just a simple shorthand so that inexperienced R users don't have
#' to use somewhat complex functions such as [grepl()] and [stringr::str_detect()]
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
  stringr::str_detect(haystack, stringr::fixed(as.character(needle)))
}


# from Hmisc to keep formr pkg small
escapeRegex = function(string) 
{
	gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
}

#' check whether a character string contains another as a word
#'
#' Looks for a string appearing on its own. This is needed e.g.
#' when checking whether the replies to a mmc item, stored as a 
#' comma-separated list from 1 to 12 contain option 1 - you wouldn't
#' want to get a hit for 11 and 12.
#' Only works for search terms containing alphanumeric characters.
#' Just a simple shorthand so that inexperienced R users don't have
#' to use somewhat complex functions such as [grepl()] and [stringr::str_detect()].
#'
#' @param haystack string in which you search
#' @param needle string to search for
#' @export
#' @examples
#' "1, 3, 4" %contains_word% "1" # TRUE
#' "1, 3, 4" %contains_word% 1 # TRUE unlike str_detect casts all needles as characters
#' "12, 14, 17" %contains_word% "1" # FALSE even though 12 contains 1

"%contains_word%" = function(haystack, needle) {
  stringr::str_detect(haystack, paste0("\\b", escapeRegex(as.character(needle)), 
    "\\b"))
}

#' check whether a character string begins with a string
#'
#' Escapes any special RegExp characters in the search term. A way to check whether the search term 
#' (e.g. a variable name) is the beginning.
#' Just a simple shorthand so that inexperienced R users won't have to use somewhat complex functions such as [grepl()] and [stringr::str_detect()]. 
#' You can also use \\%starts_with\\%.
#'
#' @param haystack string in which you search
#' @param needle string to search for
#' @export
#' @aliases %starts_with%
#' @examples
#' "1, 3, 4" %begins_with% "1" # TRUE
#' "1, 3, 4" %begins_with% 1 # unlike str_detect casts all needles as characters
#' "1, 3, 4" %begins_with% "." # FALSE

"%begins_with%" = function(haystack, needle) {
  stringr::str_detect(haystack, paste0("^", escapeRegex(as.character(needle))))
}

#' @export
"%starts_with%" = `%begins_with%`

#' check whether a character string ends with a string
#'
#' Escapes any special RegExp characters in the search term. A way to check whether the search term (e.g. a variable name) is the ending.
#' Just a simple shorthand so that inexperienced R users don't have
#' to use somewhat complex functions such as [grepl()] and [stringr::str_detect()].
#'
#' @param haystack string in which you search
#' @param needle string to search for
#' @export
#' @examples
#' "1, 3, 4" %ends_with% "4" # TRUE
#' "1, 3, 4" %ends_with% 4 # unlike str_detect casts all needles as characters
#' "1, 3, 4" %ends_with% "." # FALSE

"%ends_with%" = function(haystack, needle) {
  stringr::str_detect(haystack, paste0(escapeRegex(as.character(needle)), 
    "$"))
}

#' @export
.formr <- new.env()
#' @export
.formr$last_action_time <- NULL
#' @export
.formr$last_action_date <- NULL

#' checks how much time has passed relative to the user's last action
#'
#' checks how much time has passed. You can choose the unit. Implemented via [lubridate::dseconds()], not periods, i.e. a minute has 60 seconds, an hour 60 minutes, a day 24 hours. Months and years are not well-defined durations, but we offer them anyway for convenience.  Returns true or false.
#'
#' @param seconds argument to [lubridate::dseconds()]
#' @param minutes 60 seconds
#' @param hours 60 minutes
#' @param days 24 hours
#' @param weeks 7 days
#' @param months 30 days
#' @param years 365 days
#' @param time defaults to .formr$last_action_time, a hidden variable that is automatically set by formr.org
#' @export
#' @examples
#' 
#' time_passed(hours = 7, time = Sys.time())

time_passed = function(years = 0, months = 0, weeks = 0, days = 0, 
  hours = 0, minutes = 0, seconds = 0, time = NULL) {
  if (is.null(time) & !is.null(.formr$last_action_time)) {
    time = .formr$last_action_time
  }
  time = as.POSIXct(time)
  stopifnot(!is.null(time))
  
  (time + lubridate::dseconds(seconds + 60 * minutes + 60 * 
    60 * hours + 60 * 60 * 24 * days + 60 * 60 * 24 * 7 * 
    weeks + 60 * 60 * 24 * 30 * months + 60 * 60 * 24 * 365 * 
    years)) < lubridate::now() # local time
}

#' checks whether a new day has broken (date has increased by at least one day)
#'
#' a simple utility functions to avoid that looped Skip Backwards/Skip Forwards in formr are true repeatedly.
#'
#' @param date defaults to .formr$last_action_date, a hidden variable that is automatically set by formr.org. Will be coerced to POSIXct.
#' @export
#' @examples
#' next_day(Sys.time())

next_day = function(date = NULL) {
  if (is.null(date) & !is.null(.formr$last_action_date)) 
    date = .formr$last_action_date
  stopifnot(!is.null(date))
  date = lubridate::floor_date(as.POSIXct(date), unit = 'days')
  date + lubridate::days(1)
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
  min < lubridate::now() && max > lubridate::now()
}


#' Like [ifelse()], but allows you to assign a third value to missings.
#' 
#' Deprecated. Please use [dplyr::if_else()] in the future.
#' Defaults to assigning the "no" value to missing values as well. Often missings encapsulate
#' some sort of meaning for the variable you're trying to define.
#' 
#' @param test passed to ifelse
#' @param yes passed to ifelse
#' @param no passed to ifelse
#' @param missing defaults to the value for no
#' @export
#' @examples
#' \dontrun{
#' data(beavers)
#' beaver1$activ[1:10] = NA
#' beaver1$hyperactive = ifelse(beaver1$activ > 1, 1, 0)
#' table(beaver1$hyperactive)
#' beaver1$hyperactive = ifelsena(beaver1$activ > 1, 1, 0)
#' table(beaver1$hyperactive)
#' }
ifelsena = function(test, yes, no, missing = no) {
	.Deprecated("dplyr::if_else")
	x = ifelse(test, yes, no)
	x[is.na(x)] = missing
	x
}

#' This function makes sure you know what to expect when evaluating uncertain results in an
#' if-clause. In most cases, you should not use this function, because it can lump a lot of very 
#' different cases together, but it may have some use for fool-proofing
#' certain if-clauses on formr.org, where a field in a survey may either not exist, be missing or have
#' a value to check. 
#' 
#' @param test condition. can only have length 0 or length 1
#' @param na returned if the condition has a missing value
#' @param null passed to ifelse
#' @export
#' @examples
#' testdf = data.frame(test1 = 1, test2 = NA)
#' if ( if_na_null(testdf$test1 == 1) ) { print("go on") }
#' if ( if_na_null(testdf$test2 == 1) ) { print("not shown") }
#' if ( if_na_null(testdf$test3 == 1) ) { print("not shown") }
#' tryCatch({ if ( if_na_null(testdf2$test1 == 1) ) { print("causes error") } }, 
#'    error = function(e) { warning(e) }) 
if_na_null = function(test, na = FALSE, null = FALSE) {
	if (length(test) > 1) {
		stop("test must have length 0 or 1, has length ", length(test))
	} else if (length(test) != 0) {
		if (!is.na(test)) {
			return = test
		} else {
			return = na
		}
	} else {
		return = null
	}
	return
}


#' Replace NA values with something else
#' 
#' Often, you want to substitute missing values with some implicit known value (e.g. if the question on number of sexual partners was skipped for sexually inactive people, you know the missing should turn into zero)
#' 
#' @param x the variable
#' @param missing What to replace missing values with
#' @export
#' @examples
#' number_of_sex_partners <- c(1, 3, 5, 10, NA, 29)
#' if_na(number_of_sex_partners, 0)

if_na <- function(x, missing) {
	if (length(missing) > 1) {
		missing <- missing[is.na(x)]
	}
	x[is.na(x)] <- missing
	x
}


