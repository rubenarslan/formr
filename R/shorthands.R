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
#' fruits = c('apple', 'banana', NA, 'pear', 'pinapple', NA)
#' pets = c('cat', 'dog', 'anteater', NA, NA, NA)
#' favorites = data.frame(fruits, pets)
#' miss_frac(favorites)
#' miss_frac(favorites$fruits)
#' miss_frac(favorites, 2)

miss_frac = function(df, vars = 1:NCOL(df)) {
  if (NCOL(df) == 1) 
    fracts = sum(is.na(df)) else if (NCOL(df[, vars]) == 1) 
    fracts = sum(is.na(df[, vars])) else fracts = colSums(is.na(df[, vars]))
  fracts/NROW(df)
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

aggregate2sources = function(df, new_var, var1 = NULL, var2 = NULL, 
  remove_old_variables = TRUE) {
  if (is.null(var1) && is.null(var2)) {
    var1 = paste0(new_var, ".x")
    var2 = paste0(new_var, ".y")
  }
  if (exists(new_var, where = df)) {
    warning(paste(new_var, "already exists. Maybe delete it or choose a different name, if you're saving over your original dataframe."))
  }
  df[, new_var] = df[, var1]
  oldmiss = sum(is.na(df[, new_var]))
  df[is.na(df[, var1]), new_var] = df[is.na(df[, var1]), var2]
  
  if (remove_old_variables) {
    df[, var1] = NULL
    df[, var2] = NULL
  }
  
  message(paste(oldmiss - sum(is.na(df[, new_var])), " fewer missings"))
  df
}

#' loads an RDS object, assigns it to an object of the base-filename
#'
#' [saveRDS()] saves an object to a file, so unlike [save()] and [load()] you can assign the loaded object to a new variable using [readRDS()]. 
#' However, sometimes it may be more convenient to assign the object in the RDS file to an object of the same name as the file. This is what [loadRDS()] does. It extracts the filename using [basename()] and [tools::file_path_sans_ext()]
#'
#' @param file path to file
#' @param refhook passed to readRDS
#' @param overwrite whether to overwrite an existing object of the same name. defaults to false.
#' @export
#' @examples
#' \dontrun{
#' loadRDS(file = '~/Models/Spouses.rds') # assigns object contained in file to variable 'Spouses'
#' }

loadRDS = function(file, refhook = NULL, overwrite = FALSE) {
  object_name = basename(tools::file_path_sans_ext(file))
  if (exists(object_name, envir = parent.frame(), inherits = F) & 
    !overwrite) {
    warning(paste(object_name, "would have been overwritten. Specify overwrite = TRUE to do so."))
  } else {
    assign(object_name, readRDS(file, refhook), envir = parent.frame())
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
#' checks how much time has passed. You can choose the unit. Implemented via [lubridate::dseconds()], not periods, i.e. a minute has 60 seconds, an hour 60 minutes, a day 24 hours. Months and years are not well-defined durations, but we offer them anyway for convenience. 
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
#' time_passed(hours = 7, time = Sys.time())

time_passed = function(years = 0, months = 0, weeks = 0, days = 0, 
  hours = 0, minutes = 0, seconds = 0, time = NULL) {
  if (is.null(time) & !is.null(.formr$last_action_time)) 
    time = .formr$last_action_time
  time = as.POSIXct(time)
  stopifnot(!is.null(time))
  (time + lubridate::dseconds(seconds + 60 * minutes + 60 * 
    60 * hours + 60 * 60 * 24 * days + 60 * 60 * 24 * 7 * 
    weeks + 60 * 60 * 24 * 30 * months + 60 * 60 * 24 * 365 * 
    years)) < lubridate::here()  # local time
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
  if (is.null(date) & !is.null(.formr$last_action_date)) 
    date = .formr$last_action_date
  stopifnot(!is.null(date))
  date = lubridate::floor_date(as.POSIXct(date))
  date < lubridate::floor_date(lubridate::now(), "day")
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
  if (!inherits(x, "formula")) 
    x = stats::as.formula(paste("~", deparse(substitute(x))), env = parent.frame())
  stats::xtabs(formula = x, ..., na.action = stats::na.pass, exclude = exclude)
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

#' get functions in the environment by their class. Useful to find e.g. all regression models you've stored in interactive programming.
#'
#' @param classes objects should have one of these classes 
#' @param envir defaults to looking in the calling environment of this function, passed to ls
#' @param top_class_only defaults to FALSE. If false, also returns objects inheriting from one of the specified classes.
#' @param ... passed to ls
#' @export
#' @examples
#' data(ChickWeight)
#' chickweight.m1 <- glm(weight ~ Time + Diet, family = gaussian, data = ChickWeight)
#' ls_by_class('lm')
#' c('chickweight.m1') %in% ls_by_class('lm')
#' c('chickweight.m1') %in% ls_by_class('lm', top_class_only = TRUE)
#' 
ls_by_class = function(classes, envir = parent.frame(), top_class_only = FALSE, 
  ...) {
  inlist <- ls(envir = envir, ...)
  ifexistsgetclass = function(x, envir) {
    if (exists(x, envir = envir, inherits = FALSE)) {
      obj = get(x, envir = envir, inherits = FALSE)
      class(obj)
    } else {
      NULL
    }
  }
  classlist <- sapply(inlist, function(x) ifexistsgetclass(x, 
    envir = envir))
  has_class = function(x) {
    if (top_class_only) {
      x[[1]] %in% classes
    } else {
      !(!length(intersect(x, classes)))
    }
  }
  if (length(classlist) > 0) {
    names(classlist[sapply(classlist, has_class)  ## take any who have a class that is in our list
])
  } else {
    character(0)
  }
}

#' Returns the number of missings in a variable or dataset. 
#' If missings are an explicit level in a factor variable, this
#' function defaults to reporting them anyway.
#' 
#' @param x variable
#' @param exclude only needed for factors. defaults to NA (count level=missing as missing), setting to 0 allows you to count level=missing as nonmissing
#' @export
#' @examples
#' data(beavers)
#' beaver1$activ[1:10] = NA
#' n_missing(beaver1$activ)
#' beaver1$activ = factor(beaver1$activ, exclude = NULL)
#' sum(is.na(beaver1$activ))
#' n_missing(beaver1$activ)
#' n_missing(beaver1$activ, exclude = NULL)

n_missing = function(x, exclude = NA) { 
	if (!is.null(exclude) && is.factor(x) && sum(is.na(levels(x))) > 0) {
		x = factor(x, exclude = exclude)
	}
	sum(is.na(x)) 
}

#' Returns the number of nonmissings in a variable or dataset. 
#' If missings are an explicit level in a factor variable, this
#' function defaults to excluding them anyway.
#' 
#' @param x variable
#' @param exclude only needed for factors. defaults to NA (count level=missing as missing), setting to 0 allows you to count level=missing as nonmissing
#' @export
#' @examples
#' data(beavers)
#' beaver1$activ[1:10] = NA
#' n_nonmissing(beaver1$activ)
#' beaver1$activ = factor(beaver1$activ, exclude = NULL)
#' sum(!is.na(beaver1$activ))
#' n_nonmissing(beaver1$activ)
#' n_nonmissing(beaver1$activ, exclude = NULL)

n_nonmissing = function(x, exclude = NA) { 
	if (!is.null(exclude) && is.factor(x) && sum(is.na(levels(x))) > 0) {
		x = factor(x, exclude = exclude)
	}
	sum(!is.na(x)) 
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

# 
# 
# #' packages = c("pacman", "dplyr")
# install_packages_in_parallel(packages, cores = 4, ...) {
# 	installed_pkgs = installed.packages()
# 	pkgs_with_deps = miniCRAN::pkgDep(packages)
# 	outdated_pkgs = old.packages(instPkgs = installed_pkgs[installed_pkgs[,1] %in% pkgs_with_deps, ])
# 	missing_pkgs_with_deps = setdiff(pkgs_with_deps, installed.packages()[, 1])
# 	to_install = union(outdated_pkgs[,1], missing_pkgs_with_deps)
# 		parallel::mclapply(pkgs_with_deps, FUN = function(x) { 
# 		.libPaths()
# 		# install.packages(pkgs = x, dependencies = FALSE, locking = "pkglog") }, mc.cores = cores)
# 		})
# }

