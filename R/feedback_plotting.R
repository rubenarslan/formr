#' Plot a normed value on the standard normal
#'
#' Pass in a z-standardised value (x - Mean)/SD,
#' get a standard normal distribution. 
#'
#' @param normed_value a z-standardised value
#' @param ylab Y-axis label, defaults to "Percentage of other people with this value"
#' @param xlab X-axis label, empty by default, useful for labeling the plotted trait
#' @param colour defaults to blue
#' @param x_ticks the ticks labels for -2,1,0,1 and 2 SDs around the mean, default to minuses, pluses and the average sign 
#' @export
#' @import ggplot2
#' @examples
#' normed_value = scale(x = 20, center = 14, scale = 5) # standardise value
#' qplot_on_normal(normed_value, xlab = "Extraversion")

qplot_on_normal = function(normed_value,  ylab = "Percentage of other people with this value", 
													 xlab = '' , colour = "blue", x_ticks = c('--','-','0','+','++')) 
{
	ggplot()+
	stat_function(aes_string(x="x"), fun = stats::dnorm, size = 1, data = data.frame(x = -3:3)) +
	geom_vline(xintercept= normed_value, colour = colour,size = 1) +
	scale_x_continuous(xlab, breaks = c(-2:2), labels = x_ticks) +
	scale_y_continuous(ylab, labels = scales::percent_format())+
	theme_minimal() + 
	theme(text = element_text(size = 18))
}


#' Text feedback based on groups
#'
#' If you pass in a z-standardised value (x - Mean)/SD,
#' and a vector of feedback text chunks, that has either three
#' or five elements, the text chunks will be used in this order
#' \[very low\], low, average, high, \[very high\] corresponding to these
#' intervals \[low, -2\], \[-2, -1\], \[-1, 1\], \[1, 2\], \[2, high\]
#'
#' @param normed_value a z-standardised value
#' @param chunks a three or five element long character vector containing the text chunks for feedback
#' @export
#' @examples
#' feedback_chunk(normed_value = 0.7, chunks = c("You are rather introverted.",
#' "You're approximately as extraverted as most people.","You are rather extraverted."))

feedback_chunk = function(normed_value,  chunks) 
{
	chunks = as.character(chunks)
	if(! (length(chunks) %in% c(3,5))) stop("Have to provide either three or five chunks.")
	if(length(chunks) == 3) chunks = c(chunks[1], chunks, chunks[3]) # recycle
	
	if(normed_value <= -2)      chunks[1]
	else if(normed_value <= -1) chunks[2]
	else if(normed_value <= 1)  chunks[3]
	else if(normed_value <= 2)  chunks[4]
	else                        chunks[5]
}

#' Plot normed values as a barchart
#'
#' Pass in a data.frame with z-standardised values (x - Mean)/SD,
#' and variable names, get a bar chart. Getting your data.frame into this shape
#' probably will mean using tidyr and dplyr
#' If the data.frame has an se column or ymax/ymin columns, these will be displayed 
#' on top of the bars and the bars will become transparent.
#'
#' @param normed_data a dataset with a value column containing z-standardised value 
#' and a variable column containing labels for those values
#' @param ylab Y-axis label, defaults to "Percentage of other people with this value"
#' @param xlab X-axis label, empty by default, useful for labeling the plotted trait
#' @param title Plot title
#' @param y_ticks the ticks labels for -2,1,0,1 and 2 SDs around the mean, default to 
#' minuses, pluses and the average sign 
#' @export
#' @import ggplot2
#' @examples
#' normed_data = data.frame(variable = c("Extraversion","Openness",
#' "Agreeableness","Neuroticism","Conscientiousness"), 
#' value = c(-3,1,-1,0.5,2)) # standardise value
#' qplot_on_bar(normed_data, title = "Your personality")
#' normed_data = data.frame(variable = c("Extraversion","Openness",
#' "Agreeableness","Neuroticism","Conscientiousness"), 
#' value = c(-3,1,-1,0.5,2), se = c(0.2,0.3,0.2,0.25,0.4)) # standardise value
#' qplot_on_bar(normed_data, title = "Your personality")

qplot_on_bar = function(normed_data, ylab = "Your value", xlab = "Trait", title = '', y_ticks = c('--','-','0','+','++'))
{
	if(! all(c("value","variable") %in% names(normed_data))) stop("Malformed file, check help.")
	if(exists("se",where = normed_data))
	{
		normed_data$ymin = normed_data$value - normed_data$se
		normed_data$ymax = normed_data$value + normed_data$se
	}
	plot = 
	ggplot(normed_data, aes_string(x = "variable", y = "value", fill = "variable")) +
		ggtitle(title)+
		scale_fill_brewer("",palette="Set1")+
		scale_y_continuous(ylab, breaks=c(-2,-1,0,1,2),labels= y_ticks) +
		scale_x_discrete(xlab) +
		theme_minimal() + 
		theme(text= element_text(size = 18)) +
		expand_limits(y=c(-2.5,2.5))
	if(exists("ymin",where=normed_data)) {
		plot + geom_linerange(aes_string(ymin = "ymin", ymax = "ymax", colour = "variable"), size = 1) + scale_colour_brewer("",palette="Set1") + geom_bar(stat="identity",position=position_dodge(), alpha = 0.7)

	} else plot + geom_bar(stat="identity",position=position_dodge())
}


#' Time-polar plot
#'
#' Pass in a data.frame with z-standardised values (x - Mean)/SD,
#' and variable names, get a bar chart. Getting your data.frame into this shape
#' probably will mean using tidyr + dplyr.
#' If the data.frame has an se column or ymax/ymin columns, these will be displayed on top of the bars and the bars will become transparent.
#'
#' @param normed_data a dataset with a value column containing z-standardised value and a variable column containing labels for those values
#' @param ylab Y-axis label, defaults to "Percentage of other people with this value"
#' @param title Plot title
#' @export
#' @import ggplot2
#' @examples
#' weekdays = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
#' normed_data = data.frame(variable = factor(weekdays, weekdays), 
#' 	value = c(0,1,0.2,0.5,1.5,2,1)) # standardise value
#' qplot_on_polar(normed_data, title = "Your alcohol consumption across the week")
#' normed_data = data.frame(variable = factor(1:24,1:24), 
#' 	value = 3+rnorm(24), se = rep(0.2,24)) # standardise value
#' qplot_on_polar(normed_data, title = "Your mood around the clock")

qplot_on_polar = function(normed_data, ylab = "Your value", title = '')
{
	if(! all(c("value","variable") %in% names(normed_data)) ) stop("Malformed file, check help.")

	if(exists("se",where = normed_data))
	{
		normed_data$ymin = normed_data$value - normed_data$se
		normed_data$ymax = normed_data$value + normed_data$se
	}
	plot = 
		ggplot(normed_data, aes_string(x = "variable", y = "value", fill = "value")) +
		ggtitle(title)+
		scale_y_continuous("",breaks=c()) +
		xlab("") +
		scale_fill_continuous(ylab) +
		theme_minimal() + 
		theme(text= element_text(size = 18)) +
		coord_polar()
	if(exists("ymin",where=normed_data)) {
		plot + geom_linerange(aes_string(ymin = "ymin", ymax = "ymax", colour = "value"), size = 1) + 
			geom_bar(stat="identity",position=position_dodge(), alpha = 0.7)	+ scale_colour_continuous(ylab)
		
	} else plot + geom_bar(stat="identity",position=position_dodge())
}
