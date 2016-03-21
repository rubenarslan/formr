
#' It's easy to attach packages that overwrite functions from other packages. Especially dplyr has a lot of conflicts
#' with base packages, MASS and plyr. Because some of these conflicts do not always lead to error messages, sometimes
#' just incorrect behaviour, this function exists. Don't trust your faulty memory, just check whether dplyr's (or any other
#' package's) functions are 'on top' if you so desire.
#'
#' @param fix defaults to true. Detaches the desired package (without unloading) and loads it again. Won't work for base packages and can't overwrite functions that you defined yourself.
#' @param package the package you want to be on top (loaded last), defaults to dplyr
#' @param iteration for internal use only, if set to 0 the function will call itself to check that it worked, if set to 1, it won't.
#' @export
#' @examples
#' amigoingmad(fix = FALSE, package = 'formr')
amigoingmad = function(fix = TRUE, package = "dplyr", iteration = 0 ) {
	if (iteration > 1) {
		stop("Can't fix.")
	}
	conf = unique(conflicts())
	want_package = paste0("package:", package)
	conflicts_desired_package = conf[conf %in% ls(want_package)]
	conflict_envs = sapply(conflicts_desired_package, FUN = function(x) {
		environmentName(pryr::where(x, globalenv())) # relative to globalenv, not formr env
	})
	is_good = conflict_envs == want_package
	potentially_bad_confs = conflicts_desired_package[!is_good]
	potentially_bad_envs = conflict_envs[!is_good]
	have_to_fix = rep(FALSE, length(potentially_bad_confs))
	for (i in seq_along(potentially_bad_confs)) {
		if (!identical(body(get(potentially_bad_confs[i], pos = want_package)), 
									 body(get(potentially_bad_confs[i])))) {
			have_to_fix[i] = TRUE
		}
	}
	
	if (any(have_to_fix)) {
		message("The following functions don't have the environment you want.")
		print(data.frame(`function.` = potentially_bad_confs[have_to_fix], 
										 environment = potentially_bad_envs[have_to_fix]), 
					row.names = F)
		if (fix) {
			base::detach(name = want_package, character.only = TRUE)
			base::library(package, character.only = TRUE)
			message("Tried to fix this, calling myself again to make sure...")
			amigoingmad(fix, package, iteration + 1)
			message("Sanity restored!")
		}
	} else if (iteration == 0) {
		message("Everything looks normal. Maybe it's you.")
	}
}
