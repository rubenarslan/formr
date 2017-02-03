if (getRversion() >= "2.15.1")  utils::globalVariables(c(".")) # allow dplyr, maggritr

#' Connect to formr
#'
#' Connects to formr using your normal login and the httr library
#' which supports persistent session cookies.
#'
#' @param email your registered email address
#' @param password your password
#' @param host defaults to https://formr.org
#' @export
#' @examples
#' \dontrun{
#' formr_connect(email = 'you@@example.net', password = 'zebrafinch' )
#' }

formr_connect = function(email, password = NULL, host = "https://formr.org") {
  if (missing(email) || is.null(email)) 
    email = readline("Enter your email: ")
  if (missing(password) || is.null(password)) 
    password = readline("Enter your password: ")
  resp = httr::POST(paste0(host, "/public/login"), body = list(email = email, 
    password = password))
  text = httr::content(resp, encoding = "utf8", as = "text")
  if (resp$status_code == 200 && grepl("Success!", text, fixed = T)) 
    invisible(TRUE) else if (grepl("Error.", text, fixed = T)) 
    stop("Incorrect credentials.") else warning("Already logged in.")
}

#' Disconnect from formr
#'
#' Disconnects from formr if connected.
#'
#' @param host defaults to https://formr.org
#' @export
#' @examples
#' \dontrun{
#' formr_disconnect()
#' }

formr_disconnect = function(host = "https://formr.org") {
  resp = httr::GET(paste0(host, "/public/logout"))
  text = httr::content(resp, encoding = "utf8", as = "text")
  if (resp$status_code == 200 && grepl("Logged out", text, 
    fixed = T)) 
    invisible(TRUE) else warning("You weren't logged in.")
}

#' Download data from formr
#'
#' After connecting to formr using [formr_connect()]
#' you can download data using this command.
#'
#' @param survey_name case-sensitive name of a survey your account owns
#' @param host defaults to https://formr.org
#' @export
#' @examples
#' \dontrun{
#' formr_raw_results(survey_name = 'training_diary' )
#' }

formr_raw_results = function(survey_name, host = "https://formr.org") {
  resp = httr::GET(paste0(host, "/admin/survey/", survey_name, 
    "/export_results?format=json"))
  if (resp$status_code == 200) 
    jsonlite::fromJSON(httr::content(resp, encoding = "utf8", 
      as = "text")) else stop("This survey does not exist or isn't yours.")
}

#' Download items from formr
#'
#' After connecting to formr using [formr_connect()]
#' you can download items using this command. One of survey_name or path has to be specified, if both are specified, survey_name is preferred.
#'
#' @param survey_name case-sensitive name of a survey your account owns
#' @param host defaults to https://formr.org
#' @param path path to local JSON copy of the item table
#' @export
#' @examples
#' \dontrun{
#' formr_connect(email = 'you@@example.net', password = 'zebrafinch' )
#' formr_items(survey_name = 'training_diary' )
#' }
#' formr_items(path = 
#' 	system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))[1:2]

formr_items = function(survey_name = NULL, host = "https://formr.org", 
  path = NULL) {
  item_list = NULL
  if (!is.null(survey_name)) {
    resp = httr::GET(paste0(host, "/admin/survey/", survey_name, 
      "/export_item_table?format=json"))
    if (resp$status_code == 200) {
      item_list = jsonlite::fromJSON(txt = httr::content(resp, 
        encoding = "utf8", as = "text"), simplifyDataFrame = FALSE)
    } else {
      stop("This survey does not exist.")
    }
  } else {
    item_list = jsonlite::fromJSON(txt = path, simplifyDataFrame = FALSE)
  }
  if (!is.null(item_list)) {
    if (!is.null(item_list[["items"]])) {
      item_list = item_list[["items"]]
    }
    for (i in seq_along(item_list)) {
      if (item_list[[i]]$type == "rating_button") {
        from = 1
        to = 5
        by = 1
        if (!is.null(item_list[[i]]$type_options)) {
          # has the format 1,6 or 1,6,1 + possibly name of choice list
        	sequence = stringr::str_split(item_list[[i]]$type_options, 
        																"\\s")[[1]][1]
        	sequence = stringr::str_split(sequence, ",")[[1]]
          if (length(sequence) == 3) {
          from = as.numeric(sequence[1])
          to = as.numeric(sequence[2])
          by = as.numeric(sequence[3])
          } else if (length(sequence) == 2) {
          from = as.numeric(sequence[1])
          to = as.numeric(sequence[2])
          } else if (length(sequence) == 1) {
          to = as.numeric(sequence[1])
          }
        }
        sequence = seq(from, to, by)
        names(sequence) = sequence
        c1 = item_list[[i]]$choices$`1`
        c2 = item_list[[i]]$choices$`2`
        if (!is.null(c1)) {
	        sequence[1] = c1
        }
        if (!is.null(c2)) {
        	sequence[length(sequence)] = c2
        }
        item_list[[i]]$choices = as.list(sequence)
      }
    	# named array fails, if names go from 0 to len-1
	  	if (!is.null(item_list[[i]]$choices) && is.null(names(item_list[[i]]$choices))) {
	  		names(item_list[[i]]$choices) = 0:(length(item_list[[i]]$choices)-1)
	  	}
    }
  	names(item_list) = sapply(item_list, function(item) { item$name })
    class(item_list) = c("formr_item_list", class(item_list))
    item_list
  } else {
    stop("Have to specify either path to exported JSON file or get item table from formr.")
  }
}

#' Transform formr_item_list into a data.frame for ease of use
#'
#' This function just turns a formr_item_list into a data.frame. The reason, these lists don't come as data.frames as default is because the 'choices' are a list themselves. When transforming, the choice column contains a collapsed choice list, which may be less useful for some purposes. 
#'
#' @param x a formr_item_list
#' @param row.names not used
#' @param ... not used
#' 
#' @export
#' @examples
#' \dontrun{
#' formr_connect(email = 'you@@example.net', password = 'zebrafinch' )
#' as.data.frame(formr_items(survey_name = 'training_diary' ))
#' }
#' items = formr_items(path = 
#' system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
#' items_df = as.data.frame(items)
#' items_df[1,]


as.data.frame.formr_item_list = function(x, row.names, ...) {
  item_list = x
  names(item_list) = NULL
  for (i in seq_along(item_list)) {
    item_list[[i]][sapply(item_list[[i]], is.null)] <- NA  # NULLs are annoying when wanting to transform into a df
    
    if (!is.null(item_list[[i]]$choices)) {
      item_list[[i]]$choices = paste(paste0(names(item_list[[i]]$choices), 
        "=", item_list[[i]]$choices), collapse = ",")
    } else {
      # in some cases the choices column is missing
      # item_list[[i]]['choices'] = list(NULL)
    }
    item_list[[i]]$type_options = as.character(item_list[[i]]$type_options)
    item_list[[i]]$choice_list = as.character(item_list[[i]]$choice_list)
  }
  # item_list = lapply(item_list, FUN = as.data.frame)
  item_list = data.frame(dplyr::bind_rows(item_list))
  item_list$index = 1:nrow(item_list)
  item_list
}


#' Download detailed result timings and display counts from formr
#'
#' After connecting to formr using [formr_connect()]
#' you can download detailed times and display counts for each item using this command.
#'
#' @param survey_name case-sensitive name of a survey your account owns
#' @param host defaults to https://formr.org
#' @export
#' @examples
#' \dontrun{
#' formr_connect(email = 'you@@example.net', password = 'zebrafinch' )
#' formr_item_displays(survey_name = 'training_diary' )
#' }

formr_item_displays = function(survey_name, host = "https://formr.org") {
  resp = httr::GET(paste0(host, "/admin/survey/", survey_name, 
    "/export_itemdisplay?format=json"))
  if (resp$status_code == 200) 
    jsonlite::fromJSON(httr::content(resp, encoding = "utf8", 
      as = "text")) else stop("This survey does not exist.")
}

#' Download random groups
#'
#' formr has a specific module for randomisation.
#' After connecting using [formr_connect()]
#' you can download the assigned random groups and merge them with your data.
#'
#' @param run_name case-sensitive name of the run in which you randomised participants
#' @param host defaults to https://formr.org
#' @export
#' @examples
#' \dontrun{
#' formr_connect(email = 'you@@example.net', password = 'zebrafinch' )
#' formr_shuffled(run_name = 'different_drills' )
#' }

formr_shuffled = function(run_name, host = "https://formr.org") {
  resp = httr::GET(paste0(host, "/admin/run/", run_name, "/random_groups_export?format=json"))
  if (resp$status_code == 200) 
    jsonlite::fromJSON(httr::content(resp, encoding = "utf8", 
      as = "text")) else stop("This run does not exist.")
}

#' Random date in range
#' 
#' taken from Dirk Eddelbuettel's answer
#' here http://stackoverflow.com/a/14721124/263054
#'
#' @param N desired number of random dates
#' @param lower lower limit
#' @param upper upper limit

random_date_in_range <- function(N, lower = "2012/01/01", upper = "2012/12/31") {
  st <- as.POSIXct(as.Date(lower))
  et <- as.POSIXct(as.Date(upper))
  dt <- as.numeric(difftime(et, st, units = "sec"))
  ev <- sort(stats::runif(N, 0, dt))
  rt <- st + ev
  rt
}

#' Recognise data types based on item table
#'
#' Once you've retrieved an item table using [formr_items()] you can use this
#' function to correctly type your variables based on the item table (e.g. formr free text types will be character, but select_add_one will be factor, dates are also typed as Date, datetimes as POSIXct).
#'  
#'
#' @param survey_name case-sensitive name of a survey your account owns
#' @param item_list an item_list, will be auto-retrieved based on survey_name if omitted
#' @param results survey results, will be auto-retrieved based on survey_name if omitted
#' @param host defaults to https://formr.org
#' @export
#' @examples
#' results = jsonlite::fromJSON(txt = 
#' system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
#' class(results$created)
#' items = formr_items(path = 
#' system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
#' results = formr_recognise(item_list = items, results = results)
#' class(results$created)


formr_recognise = function(survey_name = NULL, item_list = formr_items(survey_name, 
  host = host), results = formr_raw_results(survey_name, host = host), 
  host = "https://formr.org") {
  # results fields that appear in all formr_results but aren't
  # custom items
  if (exists("created", where = results)) {
    results$created = as.POSIXct(results$created)
  	attributes(results$created)$label = "user first opened survey"
  }
  
  if (exists("modified", where = results)) {
    results$modified = as.POSIXct(results$modified)
    attributes(results$modified)$label = "user last edited survey"
  }
  if (exists("ended", where = results)) {
    results$ended = as.POSIXct(results$ended)
    attributes(results$ended)$label = "user finished survey"
  }
  
    if (is.null(item_list)) {
      warning("No item list provided, using type.convert as a fallback.")
      char_vars = sapply(results, is.character)
      if (length(char_vars) > 0) { # for special case: no data
	      type.convert = utils::type.convert
        results[, char_vars] = dplyr::mutate_all(results[, char_vars, drop = F], 
					dplyr::funs(type.convert(., as.is = TRUE)))
      }
    } else {
    	items_with_result_columns = names(results)
      for (i in seq_along(item_list)) {
        item = item_list[[i]]
        
        if (! item$name %in% items_with_result_columns) {
        	next
        }
        
        if (length(item$choices)) {
          # choice-based items
          results[, item$name] = utils::type.convert(as.character(results[, 
          item$name]), as.is = T)
          # numeric choices should be typed correctly by default
          if (is.character(results[, item$name])) {
          # save the factor with all possible levels e.g. mc, select
          if (all(unique(results[, item$name]) %in% 
            c(NA, names(item$choices)))) {
            results[, item$name] = factor(results[, 
            item$name], levels = names(item$choices))
          }
          # e.g. select_or_add_one stay character
          }
        } else if (item$type %in% c("text", "textarea", 
          "email", "letters")) {
          results[, item$name] = as.character(results[, 
          item$name])
        } else if (item$type %in% c("datetime")) {
          results[, item$name] = as.POSIXct(results[, 
          item$name])
        } else if (item$type %in% c("date")) {
          results[, item$name] = as.Date(results[, item$name], 
          format = "%Y-%m-%d")
        } else if (item$type %in% c("time")) {
          # results[, item$name ] = (results[, item$name ])
        } else if (item$type %in% c("number", "range", 
          "range_list")) {
          results[, item$name] = as.numeric(results[, 
          item$name])
        }
        attributes(results[, item$name])$label = item$label
      }
    }
    results

  results
}



#' Label values for SPSS and other software that supports value labels
#'
#' Once you've retrieved an item table using [formr_items()] you can use this
#' function to label your values
#'  
#'
#' @param results survey results
#' @param item_list an item_list, will be auto-retrieved from results attributes if omitted
#' @param item_types which item types should be given value labels (defaults to mc, select_one, mc_button)
#' @param numeric_too whether numeric items should be given value labels
#' @export
#' @examples
#' results = jsonlite::fromJSON(txt = 
#' system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
#' class(results$created)
#' items = formr_items(path = 
#' system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
#' results = formr_recognise(item_list = items, results = results)
#' results = formr_label_values_for_spss(item_list = items, results = results)
#' results$gods

formr_label_values_for_spss = function(results, item_list = NULL, item_types = c("mc","select_one", "mc_button", "mc_multiple_button", "select_or_add_one", "select_or_add_multiple", "mc_multiple"), numeric_too = FALSE) {
	if (is.null(item_list) && length(attributes(results)$item_list)) {
		item_list = attributes(results)$item_list
	} else if (is.null(item_list)) {
		stop("Need to specify an item list.")
	}
			item_names = names(results)
			for (i in seq_along(item_list)) {
				item = item_list[[i]]
				if (! item$name %in% item_names) {
					next
				} else if (length(item$choices) && item$type %in% item_types) {
					# choice-based items
					if (numeric_too || is.character(results[, item$name]) || is.factor(results[, item$name])) {
						# save the factor with all possible levels e.g. mc, select
						if (all(unique(results[, item$name]) %in% 
										c(NA, names(item$choices)))) {
								results[, item$name] = haven::labelled(
									as.character(results[, item$name]), 
									labels = unlist(item$choices)
								)
								attributes(results[, item$name])$label = item$label
						}
					}
				}
			}

	results
}




#' Simulate data based on item table
#'
#' Once you've retrieved an item table using [formr_items()] you can use this
#' function to sample data from the possible choices.
#' At the moment random data is only generated for choice-type
#' items and numeric ones, as these are most likely to enter data analysis.
#' Does not yet handle dates, times, text, locations, colors
#'  
#'
#' @param item_list the result of a call to [formr_connect()]
#' @param n defaults to 300
#' @export
#' @examples
#' \dontrun{
#' formr_connect(email = 'you@@example.net', password = 'zebrafinch' )
#' sim = formr_simulate_from_items(item_list = formr_items('training_diary'), n = 100)
#' summary(lm(pushups ~ pullups, data = sim))
#' }
#' items = formr_items(path = 
#' system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
#' fakedata = formr_simulate_from_items(items, n = 20)
#' fakedata[1:2,]


formr_simulate_from_items = function(item_list, n = 300) {
  sim = data.frame(id = 1:n)
  sim$created = random_date_in_range(n, Sys.time() - 10000000, 
    Sys.time())
  sim$modified = sim$ended = sim$created + lubridate::dseconds(stats::rpois(n, 
    lambda = length(item_list) * 20)  # assume 20 seconds per item
)

  for (i in seq_along(item_list)) {
    item = item_list[[i]]
    if (item$type %in% c("note", "mc_heading", "submit", "block")) {
      next
    } else if (length(item$choices)) {
      # choice-based items
      sample_from = utils::type.convert(names(item$choices), as.is = F)
      sim[, item$name] = sample(sample_from, size = n, 
        replace = T)
    } else if (length(item$type_options) && stringr::str_detect(item$type_options, 
      "^[0-9.,]+$")) {
      limits = as.numeric(stringr::str_split(item$type_options, 
        pattern = stringr::fixed(","))[[1]])
      if (length(limits) == 3) {
        sample_from = seq(from = limits[1], to = limits[2], 
          by = limits[3])
        sim[, item$name] = sample(sample_from, size = n, 
          replace = T)
      }
    }
  }
  sim
}


#' Reverse items based on item table or a fallback_max
#'
#' Example: If your data contains Extraversion_1, Extraversion_2R and Extraversion_3, there will be two new variables in the result: Extraversion_2 (reversed to align with _1 and _2) and Extraversion, the mean score of the three. If you supply an item table, the maximum possible answer to the item will be used to reverse it. If you don't, the maximum actually given answer or the fallback_max argument will be used to reverse it. It's faster to do this without an item table, but this can lead to problems, if you mis-specify the fallback max or the highest possible value does not occur in the data. 
#'  
#'
#' @param results survey results
#' @param item_list an item_list, defaults to NULL
#' @param fallback_max defaults to 5 - if the item_list is set to null, we will use this to reverse
#' @export
#' @examples
#' \dontrun{
#' formr_connect(email = 'you@@example.net', password = 'zebrafinch' )
#' icar_items = formr_items(survey_name='ICAR',host = 'http://localhost:8888/formr/')
#' # get some simulated data and aggregate it
#' sim_results = formr_simulate_from_items(icar_items)
#' reversed_items = formr_reverse(item_list = icar_items, results = sim_results)
#' }
#' results = jsonlite::fromJSON(txt = 
#' 	system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
#' items = formr_items(path = 
#' 	system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
#' formr_reverse(results, items)



formr_reverse = function(results, item_list = NULL, fallback_max = 5) {
  # reverse items first we're playing dumb and don't have the
  # item table to base our aggregation on?
  item_names = names(results)  # we use the item names of all items, including notes and text, hoping that there is no false positive
  
  if (is.null(item_list)) {
    char_vars = sapply(results, is.character)
    type.convert = utils::type.convert
    results[, char_vars] = dplyr::mutate_all(results[, char_vars, drop = F], 
    																				 dplyr::funs(type.convert(., as.is = TRUE)))
    
    # get reversed items
    reversed_items = item_names[stringr::str_detect(item_names, 
      "^(?i)[a-zA-Z0-9_]+?[0-9]+R$")]
    if (length(reversed_items)) {
      for (i in seq_along(reversed_items)) {
        # reverse these items based on fallback_max, or if higher the
        # item's own maximum
        results[, stringr::str_sub(reversed_items[i], 
          1, -2)] = (max(results[, reversed_items[i]], 
          fallback_max, na.rm = T) + 1) - results[, reversed_items[i]]
      }
    }
  } else {
    # if we have an item list we can do more
  	
    for (i in seq_along(item_list)) {
      item = item_list[[i]]
      if (! item$name %in% item_names) {
      	next
      } else if (length(item$choices)) {
        # choice-based items with a number and an 'R' at the end
        if (stringr::str_detect(item$name, "(?i)^([a-z0-9_]+?)[0-9]+R$")) {
          possible_replies = utils::type.convert(names(item$choices))
          
          if (!is.numeric(possible_replies)) {
          warning(item$name, " is not numeric and cannot be reversed.")
          } else {
          results[, stringr::str_sub(item$name, 1, 
            -2)] = max(possible_replies) + 1 - as.numeric(results[, 
            item$name])  # reverse\t# save as item name with the R truncated
          }
        }
      }
    }
  }
  results
}

#' Aggregate data based on item table
#'
#' If you've retrieved an item table using [formr_items()] you can use this
#' function to aggregate your multiple choice items into mean scores. 
#' If you do not have a item table (e.g. your data was not collected using formr, you don't want another HTTP request in a time-sensitive process).
#' Example: If your data contains Extraversion_1, Extraversion_2R and Extraversion_3, there will be two new variables in the result: Extraversion_2 (reversed to align with _1 and _2) and Extraversion, the mean score of the three.
#'  
#'
#' @param survey_name case-sensitive name of a survey your account owns
#' @param item_list an item_list, will be auto-retrieved based on survey_name if omitted
#' @param results survey results, will be auto-retrieved based on survey_name if omitted
#' @param host defaults to https://formr.org
#' @param compute_alphas defaults to TRUE, whether to compute  [psych::alpha()]
#' @param fallback_max defaults to 5 - if the item_list is set to null, we will use this to reverse
#' @param plot_likert defaults to TRUE - whether to make [likert::likert()] plots. Only possible if item_list is specified.

#' @param ... passed to  [psych::alpha()]
#' @export
#' @examples
#' results = jsonlite::fromJSON(txt = 
#' 	system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
#' items = formr_items(path = 
#' 	system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
#' agg = formr_aggregate(item_list = items, results = results, 
#' 	compute_alphas = TRUE, plot_likert = TRUE)
#' agg[, c('religiousness', 'prefer')]


formr_aggregate = function(survey_name, item_list = formr_items(survey_name, 
  host = host), results = formr_raw_results(survey_name, host = host), 
  host = "https://formr.org", compute_alphas = FALSE, fallback_max = 5, 
  plot_likert = FALSE, ...) {
  results = formr_reverse(results, item_list, fallback_max = fallback_max)
  item_names = names(results)  # update after reversing
  
  if (!is.null(item_list)) {
    if (!inherits(item_list, "formr_item_list")) {
      stop("The item_list has to be a formr item list.")
    }
    item_list_df = as.data.frame(item_list)
    item_list_df$scale = suppressWarnings(stringr::str_match(item_list_df$name, 
      "(?i)^([a-z0-9_]+?)_?[0-9]+R?$")[, 2])  # fit the pattern
    likert_scales = item_list_df[item_list_df$type %in% c("mc", 
      "mc_button", "rating_button"), ]
  } else {
    plot_likert = FALSE
  }
  
  scale_stubs = stringr::str_match(item_names, "(?i)^([a-z0-9_]+?)_?[0-9]+$")[, 
    2]  # fit the pattern
  # if the scale name ends in an underscore, remove it
  scales = unique(stats::na.omit(scale_stubs[duplicated(scale_stubs)]))  # only those which occur more than once
  # todo: should check whether they all share the same reply
  # options (choices, type_options)
  for (i in seq_along(scales)) {
    save_scale = scales[i]
    
    if (exists(save_scale, where = results)) {
      warning(save_scale, ": Would have generated scale, but a variable of that name existed already.")
      next
    }
    scale_item_names = item_names[which(scale_stubs == save_scale)]
    numbers = as.numeric(stringr::str_match(scale_item_names, 
      "(?i)^[a-z0-9_]+?([0-9])+$")[, 2])
    if (!setequal(intersect(scale_item_names, names(results)), 
      scale_item_names)) {
      warning(save_scale, ": Some items were missing. ", 
        paste(setdiff(scale_item_names, names(results)), 
          collapse = " "))
      next
    }
    if (length(scale_item_names) == 1) {
      warning(save_scale, ": seems to consist of only a single item.")
      next
    }
    if (!setequal(min(numbers):max(numbers), numbers)) {
      warning(save_scale, ": Some items from the scale might be missing, the lowest item number was ", 
        min(numbers), " the highest was ", max(numbers), 
        " but we didn't see ", paste(setdiff(min(numbers):max(numbers), 
          numbers), collapse = " "))
      next
    }
    if (!all(sapply(results[, scale_item_names], is.numeric))) {
      warning(save_scale, ": One of the items in the scale is not numeric. The scale was not aggregated.")
      next
    }
    
    if (!is.null(item_list)) {
      
      choice_lists = item_list[likert_scales[which(likert_scales$scale == 
        save_scale), "index"]]
      choice_values = unique(lapply(choice_lists, FUN = function(x) {
        names(x$choices)
      }))
      if (length(choice_values) != 1) {
        warning(save_scale, ": The responses were saved with different possible values. Hence, the scale could not be aggregated. We saw ", 
          paste(sapply(choice_values, FUN = paste, collapse = ";"), 
          collapse = " & "))
        next
      }
      choice_labels = unique(lapply(choice_lists, FUN = function(x) {
        x$choices
      }))
      if (length(choice_labels) != 1) {
        warning(save_scale, ": Was aggregated, but the response labels/item choices weren't identical across items, we saw ", 
          paste(sapply(choice_labels, FUN = paste, collapse = ";"), 
          collapse = " & "))
      }
    }
    # actually aggregate scale
    results[, save_scale] = rowMeans(results[, scale_item_names])
    
    if (plot_likert) {
      lik = formr_likert(choice_lists, results)
      if (!is.null(lik)) 
        print(graphics::plot(lik))
    }
    if (compute_alphas) {
      if (length(numbers) > 1) {
        rows_with_missings = nrow(results[, scale_item_names]) - 
          nrow(stats::na.omit(results[, scale_item_names]))
        if (rows_with_missings > 0) {
          warning("There were ", rows_with_missings, 
          " rows with missings in ", save_scale)
        }
        tryCatch({
          psych::print.psych(psych::alpha(stats::na.omit(results[, 
          scale_item_names]), title = save_scale, check.keys = F, 
          ...))
        }, error = function(e) {
          warning("There were problems with ", save_scale, 
          " or its items ", paste(scale_item_names, 
            collapse = " "), " while trying to compute internal consistencies. ", 
          e)
        })
      }
    }
  }
  if (plot_likert) {
    leftover_items = item_list[likert_scales[which(!likert_scales$scale %in% 
      scales), "index"]]
    for (i in seq_along(leftover_items)) {
    	print(
    		ggplot2::qplot(results[, leftover_items[[i]]$name ]) + 
    			ggplot2::xlab(leftover_items[[i]]$name)
    		)
    }
  }
  results
}

#' Download processed, aggregated results from formr
#'
#' After connecting to formr using [formr_connect()]
#' you can download data, it basically just does [formr_raw_results()] ,
#' [formr_recognise()] and [formr_aggregate()] in sequence.
#'
#' @param survey_name case-sensitive name of a survey your account owns
#' @param host defaults to https://formr.org
#' @param compute_alphas passed to formr_aggregate, defaults to TRUE
#' @param fallback_max passed to formr_reverse, defaults to 5
#' @param plot_likert passed to formr_aggregate, defaults to TRUE
#' @export
#' @examples
#' \dontrun{
#' formr_results(survey_name = 'training_diary' )
#' }

formr_results = function(survey_name, host = "https://formr.org", 
  compute_alphas = TRUE, fallback_max = 5, plot_likert = TRUE) {
  results = formr_raw_results(survey_name, host)
  item_list = formr_items(survey_name, host)
  results = formr_post_process_results(results = results, item_list = item_list, 
    compute_alphas = compute_alphas, fallback_max = fallback_max, 
    plot_likert = plot_likert)
  results
}

#' Processed, aggregated results
#'
#' This function chains [formr_recognise()] and [formr_aggregate()] 
#' in sequence. Useful if you want to post-process raw results before aggregating etc.
#'
#' @param item_list an item_list, defaults to NULL
#' @param results survey results
#' @param compute_alphas passed to formr_aggregate, defaults to TRUE
#' @param fallback_max passed to formr_reverse, defaults to 5
#' @param plot_likert passed to formr_aggregate, defaults to TRUE
#' @export
#' @examples
#' results = jsonlite::fromJSON(txt = 
#' 	system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
#' items = formr_items(path = 
#' 	system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
#' results = formr_post_process_results(items, results, 
#' compute_alphas = TRUE, plot_likert = TRUE)


formr_post_process_results = function(item_list = NULL, results, 
  compute_alphas = FALSE, fallback_max = 5, plot_likert = FALSE) {
  results = formr_recognise(item_list = item_list, results = results)
  results = formr_aggregate(item_list = item_list, results = results, 
    compute_alphas = compute_alphas, fallback_max = fallback_max, 
    plot_likert = plot_likert)
  attributes(results)$item_list = item_list
  class(results) = c("formr_data.frame", class(results))
  results
}

#' Get Likert scales
#'
#' If you've retrieved an item table using [formr_items()] you can use this
#' function to retrieve a [likert::likert()] object that can be used with the likert package functions (which makes nice plots). You can and should subset the results table to focus on items by scale or response format. The aggregator will interrupt if the response format changes.
#'  
#'
#' @param item_list an item_list
#' @param results survey results
#' @export
#' @examples
#' results = jsonlite::fromJSON(txt = 
#' 	system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
#' items = formr_items(path = 
#' 	system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
#' likert_items = formr_likert(item_list = items[2:5], results = results)
#' plot(likert_items)



formr_likert = function(item_list, results) {
  if (!inherits(item_list, "list")) {
    stop("The item_list has to be a list.")
  }
  item_numbers = c()
  choice_lists = item_list
  choice_labels = unique(lapply(choice_lists, FUN = function(x) {
    x$choices
  }))
  choice_values = unique(lapply(choice_lists, FUN = function(x) {
    names(x$choices)
  }))
  if (length(choice_values) != 1) {
    warning("Likert plot not possible, their were multiple response values. We saw ", 
      paste(sapply(choice_values, FUN = paste, collapse = ";"), 
        collapse = " & "))
    return(NULL)
  }
  if (length(choice_labels) != 1) {
    warning("Likert plot not possible, their were multiple response labels. We saw ", 
      paste(sapply(choice_labels, FUN = paste, collapse = ";"), 
        collapse = " & "))
    return(NULL)
  }
  
  for (i in seq_along(item_list)) {
    item = item_list[[i]]
    item_number = which(names(results) == item$name)
    
    
    if (length(item_number) > 0 & item$type %in% c("mc_button", 
      "mc", "rating_button")) {
      item_numbers = c(item_numbers, item_number)
      results[, item_number] = factor(results[, item$name], 
        levels = names(item$choices), labels = item$choices)
      names(results)[item_number] = paste(item$label, paste0("[", 
        item$name, "]"))  # seriously cumbersome way to rename single column
    }
  }
  if (ncol(results[, item_numbers, drop = FALSE]) > 0 & nrow(stats::na.omit(results[, 
    item_numbers, drop = FALSE]))) {
    likert::likert(results[, item_numbers, drop = FALSE])
  } else {
    NULL
  }
}

#' get item list from survey data.frame attributes
#'
#' 
#'
#' @param survey survey with item_list attribute
#' @export
#' @examples
#' example(formr_post_process_results)
#' items(results)[[1]]
items = function(survey) {
	attributes(survey)$item_list
}


#' generates valid email cids
#'
#' can be used as an argument to [knitr::opts_knit]. If you attach the images properly, you can then send knit emails including plots. See the formr OpenCPU module on Github for a sample implementation.
#'
#' @param x image ID
#' @param ext extension, defaults to .png
#' @export
#' @examples
#' \dontrun{
#' library(knitr); library(formr)
#' opts_knit$set(upload.fun=formr::email_image)
#' }

email_image = function(x, ext = ".png") {
	cid = gsub("[^a-zA-Z0-9]", "", substring(x, 8))
	structure(paste0("cid:", cid, ext), link = x)
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
#' get_opencpu_rds('https://public.opencpu.org/ocpu/tmp/x02a93ec/R/.val/rds')
#' }
get_opencpu_rds = function(session_url, local = TRUE) {
	if (local) {
		sessionenv <- new.env()
		filepath = stringr::str_match(session_url, "/ocpu/tmp/([xa-f0-9]+)/([a-z0-9A-Z/.]+)")
		sessionfile <- file.path("/tmp/ocpu-www-data/tmp_library", 
														 filepath[, 2], ".RData")
		if (file.exists(sessionfile)) {
			load(sessionfile, envir = sessionenv)
			desired_obj = stringr::str_sub(filepath[, 3], 3, 
																		 -5)
			sessionenv[[desired_obj]]
		}
	} else {
		readRDS(gzcon(curl::curl(session_url)))
	}
}

# # # # # ## testing with credentials formr_connect('', '')
# vorab = formr_raw_results('Vorab_Fragebogen1') vorab_items
# = formr_items('Vorab_Fragebogen1') vorab_item_displays =
# formr_item_displays('Vorab_Fragebogen1') vorab_processed =
# formr_recognise(item_list=vorab_items, results=vorab)
# vorab_sim =
# formr_simulate_from_items(item_list=vorab_items)
# vorab_sim_agg = formr_aggregate(item_list=vorab_items,
# results=vorab_sim, compute_alphas = T) vorab_proc_agg =
# formr_aggregate(item_list=vorab_items,
# results=vorab_processed,compute_alphas=T) # vorab_raw_agg =
# formr_aggregate(item_list=vorab_items,
# results=vorab,compute_alphas=T) vorab_raw_agg =
# formr_aggregate(item_list=NULL,
# results=vorab,compute_alphas=T) vorab_comp =
# formr_results('Vorab_Fragebogen1') options(warn=2)

# todo: better rmarkdown with proper linebreaks
# http://rmarkdown.rstudio.com/developer_custom_formats.html
