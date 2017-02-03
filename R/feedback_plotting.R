#' iterate adding ribbons to a ggplot2 plot at varying confidence levels to shade by confidence. Horribly inefficient, because smooth stat is computed every time, but flexible.
#'
#' @param levels the confidence levels that are supposed to be displayed, defaults to 0.6, 0.8, 0.95
#' @param base_alpha divided by length(levels)
#' @param fill_gradient a vector of colors that has at least the same length as levels. Color each ribbon differently
#' @param fill a single color for the ribbon
#' @param stat defaults to smooth
#' @param ... everything else is passed to and documented in [ggplot2::geom_smooth()]
#' @inheritParams ggplot2::geom_smooth
#' @export
#' @examples
#' data(beavers)
#' plot = ggplot2::ggplot(beaver1, ggplot2::aes(time, temp))
#' plot + geom_shady_smooth() + ggplot2::facet_wrap(~ day)
#' plot + geom_shady_smooth(fill = 'blue', levels = seq(0.05,0.95,0.1))
#' plot + geom_shady_smooth(size = 0.1, fill = '#49afcd', levels = seq(0.1,0.8,0.01))
#' plot + geom_shady_smooth(fill_gradient = c('red', 'orange', 'yellow'), base_alpha = 3)
geom_shady_smooth <- function(mapping = NULL, data = NULL, stat = "smooth", 
															method = "auto", formula = y ~ x, se = TRUE, position = "identity", 
															na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, levels = c(0.6, 
																																															0.8, 0.95), base_alpha = 1, fill_gradient = NULL, fill = "black", 
															...) {
	layers = list()
	ribbon_alpha = base_alpha/length(levels)
	if (ribbon_alpha > 1) 
		ribbon_alpha = 1
	
	params <- list(na.rm = na.rm, fill = fill, ...)
	if (identical(stat, "smooth")) {
		params$method <- method
		params$formula <- formula
		params$se <- se
	}
	params_ribbon = params
	params_ribbon$color = NULL  # don't want the line color to be the stroke of the ribbon
	params_ribbon$alpha = ribbon_alpha  # alpha level for ribbon is automatically based on nr of levels and base_alpha
	levels = rev(levels)
	if (!is.null(fill_gradient)) {
		stopifnot(length(fill_gradient) == length(levels))
		fill_gradient = rev(fill_gradient)
	}
	for (i in seq_along(levels)) {
		params_ribbon$level = levels[[i]]
		if (!is.null(fill_gradient)) {
			params_ribbon$fill = fill_gradient[i]
		}
		layers[[i]] = ggplot2::layer(data = data, mapping = mapping, 
																 stat = stat, geom = ggplot2::GeomRibbon, position = position, 
																 show.legend = show.legend, inherit.aes = inherit.aes, 
																 params = params_ribbon)
	}
	params$fill = NULL  # line knows no fill aesthetic
	layers[[i + 1]] = ggplot2::layer(data = data, mapping = mapping, 
																	 stat = stat, geom = ggplot2::GeomLine, position = position, 
																	 show.legend = show.legend, inherit.aes = inherit.aes, 
																	 params = params)
	layers
}

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
  stat_function(aes(x=-3:3), fun = stats::dnorm,size = I(1)) + 
  geom_vline(xintercept= normed_value, colour= colour,size = I(1)) +
	scale_x_continuous(xlab, breaks = c(-2:2),labels = x_ticks) +
	scale_y_continuous(ylab, labels = scales::percent_format())+
	theme_minimal() + 
	theme(text = element_text(size = 18))
}


#' Text feedback based on groups
#'
#' If you pass in a z-standardised value (x - Mean)/SD,
#' and a vector of feedback text chunks, that has either three
#' or five elements, the text chunks will be used in this order
#' [very low], low, average, high, [very high] corresponding to these
#' intervals [low, -2], [-2, -1], [-1, 1], [1, 2], [2, high]
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

waffle_df = function(x, rows = NULL, cols = NULL) {
	# x = sort(x)
	total = length(x)
	# Specify unique x and y coord for each case
	if (is.null(rows) & is.null(cols)) {
		rows = cols = ceiling(sqrt(total))
		if ((rows * cols - total) == cols) {
			cols = cols - 1 # if we have an empty column, remove it
		}
	} else if (is.numeric(cols) & is.numeric(rows)) {
		if (total < rows * cols) {
			warning(paste0("Total ",total," smaller than number of cells (",rows,"x",cols,")"))
		}
	} else if (is.numeric(rows)) {
		cols = ceiling(total / rows)
	} else if (is.numeric(cols)) {
		rows = ceiling(total / cols)
	}
	x_pa = c(x, rep(NA, times = rows * cols - total))
	dim(x_pa) = c(cols, rows)
	xdf = reshape2::melt(x_pa)
	xdf$value = factor(xdf$value,exclude = NA)
	xdf$Var1 = xdf$Var1 - 1 # left shift all
	xdf$Var1 = xdf$Var1 + floor(xdf$Var1 / 5) * 2 / sqrt(total) # every fifth gets extra distance
	xdf$Var1 = xdf$Var1 + floor(xdf$Var1 / 10) * 1 / sqrt(total) # every tenth gets even more
	xdf$Var2 = xdf$Var2 - 1 # up shift all
	xdf$Var2 = xdf$Var2 + floor(xdf$Var2 / 5) * 2 / sqrt(total)
	xdf$Var2 = xdf$Var2 + floor(xdf$Var2 / 10) * 1 / sqrt(total)
	xdf
}

#' Waffle plot
#'
#' Pass in a a variable and get a waffle plot.
#' Useful to display simple counts or if the variable has different values,
#' a square pie chart. If the variable has a length that makes the individual squares
#' hard to see, consider showing hundreds, thousands etc.
#' 
#' To avoid the Hermann grid illusion, don't use dark colours.
#'
#' @param x a variable with not too many unique values
#' @param shape defaults to a filled square
#' @param rows defaults to the rounded up square root of the number of values
#' @param cols defaults to the rounded down square root of the number of values
#' @param drop_shadow_h horizontal offset of the drop shadow, tinker with this to get a proper shadow effect
#' @param drop_shadow_v vertical offset of the drop shadow
#' @export
#' @import ggplot2
#' @examples
#' qplot_waffle(rep(1:2,each=200))
qplot_waffle = function(x, shape = 15, rows = NULL, cols = NULL, drop_shadow_h = -0.3, drop_shadow_v = 0.3) {
	xdf = waffle_df(x, rows, cols)
	total = length(x)
	types = length(unique(stats::na.omit(x)))
	
	miss_value = is.na(levels(xdf$value)[xdf$value])
	xdf$Var1_offset = xdf$Var1 + drop_shadow_h/sqrt(total)
	xdf$Var2_offset = xdf$Var2 + drop_shadow_v/sqrt(total)
	ggplot(xdf) + 
		geom_point(aes_string(x = "Var1_offset", y = "Var2_offset"), 
							 colour = ifelse(miss_value, NA, "black"),
							 shape = shape,
							 size = round(140/sqrt(total)),
							 show_legend = F) +
		geom_point(aes_string(x = "Var1", y = "Var2", colour = "value"), 
							 shape = shape,
							 size = round(140/sqrt(total)),
							 show_legend = ifelse(types>1,T,F)) +
		coord_fixed() +
		theme_minimal() +
		guides(colour = guide_legend(override.aes = list(shape = 15, size = 12) ) ) +
		ylab("")+ xlab("")+
		scale_x_continuous(expand = c(0.12, 0.12)) +
		scale_y_continuous(expand = c(0.12, 0.12), trans = "reverse") +
		theme(
			panel.background = element_rect(colour = NA), 
			panel.border = element_blank(),
			strip.background = element_blank(),
			panel.grid = element_blank(),
			axis.line = element_blank(), 
			axis.text = element_blank(),
			axis.title = element_blank(),
			axis.ticks = element_blank()) +
	scale_colour_manual("",values = c("#aea96f","#a5c25c","#a3ccdc"))
}

fontawesome_square = '\uf0c8'

#' Waffle plot (text)
#'
#' Pass in a a variable and get a waffle plot.
#' Useful to display simple counts or if the variable has different values,
#' a square pie chart. If the variable has a length that makes the individual squares
#' hard to see, consider showing hundreds, thousands etc.
#' 
#' This functions is like waffle_plot but it allows you to specify custom symbols
#' from FontAwesome. Copypaste them from here: http://fontawesome.io/cheatsheet
#'
#' To avoid the Hermann grid illusion, don't use dark colours.
#'
#' @param x a variable with not too many unique values
#' @param symbol pass a unicode symbol from FontAwesome here. Defaults to a square with rounded edges
#' @param rows defaults to the rounded up square root of the number of values
#' @param cols defaults to the rounded down square root of the number of values
#' @param drop_shadow_h horizontal offset of the drop shadow, tinker with this to get a proper shadow effect
#' @param drop_shadow_v vertical offset of the drop shadow
#' @param font_family defaults to FontAwesome
#' @param font_face defaults to Regular
#' @param font_size defaults to round(140/sqrt(length(x)))
#' @export
#' @import ggplot2
#' @encoding UTF-8
#' @examples
#' \dontrun{
#' qplot_waffle_text(rep(1:2,each=30), rows = 5)
#' }
qplot_waffle_text = function(x, symbol = fontawesome_square, rows = NULL, cols = NULL, drop_shadow_h = -0.9, drop_shadow_v = 0.9, font_family = "FontAwesome", font_face = "Regular",
  font_size = round(140/sqrt(length(x)))) {
  xdf = waffle_df(x, rows, cols)
  types = length(unique(stats::na.omit(x)))

  miss_value = is.na(levels(xdf$value)[xdf$value])
  xdf$Var1_offset = xdf$Var1 + drop_shadow_h * font_size/140
  xdf$Var2_offset = xdf$Var2 + drop_shadow_v * font_size/140

  ggplot(xdf) + 
    geom_point(aes_string(x = "Var1", y = "Var2", colour = "value"),size = 0,show_legend = ifelse(types>1,T,F)) +
    geom_text(aes_string(x = "Var1_offset", y = "Var2_offset"), 
              colour = ifelse(miss_value, NA, "black"),
              size = font_size,label= symbol,
              show_legend = F, alpha = .3, family = font_family, face = font_face) +
    geom_text(aes_string(x = "Var1", y = "Var2", colour = "value"), show_legend = F,
              size = font_size, label= symbol, family = font_family, face = font_face) +
    coord_fixed() +
    theme_minimal() +
    guides(colour = guide_legend(override.aes = list(shape = 15, size = 12) ) ) +
    ylab("") + xlab("") +
    scale_x_continuous(expand = c(0.12, 0.12)) +
    scale_y_continuous(expand = c(0.12, 0.12), trans = "reverse") +
    theme(
      panel.background = element_rect(colour = NA), 
      panel.border = element_blank(),
      strip.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(), 
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()) +
    scale_colour_manual("",values = c("#aea96f","#a5c25c","#a3ccdc"))
}


#' Waffle plot (tile)
#'
#' Pass in a a variable and get a waffle plot.
#' Useful to display simple counts or if the variable has different values,
#' a square pie chart. If the variable has a length that makes the individual squares
#' hard to see, consider showing hundreds, thousands etc.
#' 
#' This function allows and requires the least tinkering, but also does not
#' drop shadows.
#' To avoid the Hermann grid illusion, don't use dark colours.
#'
#' adapted from http://shinyapps.stat.ubc.ca/r-graph-catalog/
#' who adapted it from http://www.techques.com/question/17-17842/How-to-make-waffle-charts-in-R
#' who adapted it from http://ux.stackexchange.com/a/46543/56341
#' 
#'
#' @param x a variable with not too many unique values
#' @param rows defaults to the rounded up square root of the number of values
#' @param cols defaults to the rounded down square root of the number of values
#' @export
#' @import ggplot2
#' @examples
#' qplot_waffle_tile(rep(1:2,each=500))
qplot_waffle_tile = function(x, rows = NULL, cols = NULL) {
	xdf = waffle_df(x, rows, cols)
	total = length(x)
	types = length(unique(stats::na.omit(x)))
		
	miss_value = is.na(levels(xdf$value)[xdf$value])
	xdf$color = "white"
	
	ggplot(xdf) + 
		geom_tile(aes_string(x = "Var1", y = "Var2", fill = "value", color = "color"), size = 25/sqrt(total),show_legend = ifelse(types > 1, T, F)) +
		scale_x_continuous(expand = c(0, 0)) +
		scale_y_continuous(expand = c(0, 0), trans = "reverse") +
		scale_color_manual(values = "white",guide = F)+
		coord_fixed() +
		theme_minimal() + 
		theme(panel.border = element_blank(),
					plot.title = element_text(size = rel(1.2), face = "bold"),
					axis.line = element_blank(),
					axis.text = element_blank(),
					axis.title = element_blank(),
					axis.ticks = element_blank(),
					panel.grid = element_blank())  +
	scale_fill_manual("",values = c("#aea96f","#a5c25c","#a3ccdc"))
}

#    
#    plot_value_on_other_data = function(variable,title, xlab, ylab = 'Häufigkeit dieses Wertes in unserer Stichprobe',standardize=T, wave2=T) {
#    tryCatch({
#    	wavecolour = wave1colour
#    	if(wave2) wavecolour = wave2colour
#    
#    	if(standardize) {
#    	stand = scale(si[,variable])[,1]
#    	} else 
#    	{
#    		stand = si[,variable]
#    	}
#    	zvalue = stand[which(si$id==id)]
#    	stopifnot(!is.na(zvalue))
#    	if(!is.na(zvalue)) {
#        
#    		if(standardize) {
#        if(zvalue>3) zvalue=3
#        if(zvalue<(-3)) zvalue=-3  
	#    		stand = stand + rnorm(length(stand),sd=0.2)
#    
#        }
#    	normal = data.frame(werte = seq(-3,3,length=2000),haeufigkeit = dnorm(seq(-3,3,length=2000),mean=0, sd=1))
#    		plot=
#    			ggplot()+
#    			geom_bar(data=as.data.frame(stand),aes(x=stand,y=..density..),alpha=0.4,fill=wavecolour)+
#    			geom_vline(colour=wavecolour,xintercept=zvalue,size=1.2)+
#    			xlab(xlab)+
#        	ylab(ylab)+
#    			labs(title=title) + 
#    			scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4),labels=c('0%','10%','20%','30%','40%'))
#    		if(standardize)
#    			{
#    				plot + geom_line(data=normal,aes(x=werte,y=haeufigkeit),alpha=0.8)+
#      scale_x_continuous(breaks=c(-2,-1,0,1,2),labels=c('stark unter-\ndurchschnitlich','unterdurch-\nschnittlich','Durchschnitt','überdurch-\nschnittlich','stark über-\ndurchschnittlich'))
#    
#    		} else {
#    			plot
#    		}
#    
#    	}
#    }, error = function(e) {stop(simpleError('Dieser Wert fehlt bei Ihnen.'))})
#    }
#    plot_association = function(xvar, yvar, title, xlab, ylab, wave1=T)
#    {
#    	si$xvar = scale(si[,xvar])
#    	si$yvar = scale(si[,yvar])
#    	colo = wave2colour
#    	if(wave1) colo = wave1colour
#    	ggplot(data=si,aes(x=xvar,y=yvar,alpha=Wer)) + 
#    		geom_jitter(aes(size=Wer),colour=colo) + 
#    		scale_alpha_discrete(range = c(0.4, 1))+
#    		scale_size_discrete(range = c(2, 4))+
#    		geom_smooth(colour=colo,method="lm",se=F) + 
#    		scale_x_continuous(xlab,breaks=c(-2,-1,0,1,2),labels=c('--','-','0','+','++')) + 
#    		scale_y_continuous(ylab,breaks=c(-2,-1,0,1,2),labels=c('--','-','0','+','++')) + 
#    		labs(title=title)
#    }
#    
#    plot_diary_association = function(xvar, yvar, title, xlab, ylab)
#    {
#    tryCatch({
#    	ddd = diary1[,c('id',xvar,yvar)]
#    	ddd2 = diary2[,c('id',paste0(xvar,'_w2'),paste0(yvar,'_w2'))]
#    	names(ddd2) = names(ddd) = c('id','xvar','yvar')
#    	ddd$Welle = 1;	ddd2$Welle = 2
#    	ddd = rbind(ddd,ddd2)
#    	ddd$Welle = factor(ddd$Welle)
#    	ddd$xvar = scale(ddd$xvar) + rnorm(nrow(ddd),sd=0.1)
#    	ddd$yvar = scale(ddd$yvar) + rnorm(nrow(ddd),sd=0.1)
#    	multi = lmer(yvar ~ xvar + (1|id), data=ddd) # to get mean slope and intercept
#    	meaneff = fixef(multi)
#    	ddd = ddd[which(ddd$id==id),]
#    	stopifnot(!all(is.na(ddd$xvar)),!all(is.na(ddd$yvar)))
#    
#    	
#    	ggplot(ddd,aes(x=xvar,yvar)) + 
#    		geom_point(aes(colour=Welle)) + 
#    		scale_colour_manual(values=cbPalette)+
#    		geom_smooth(method="lm",colour=wave2colour,se=F) + 
#    		geom_abline(linetype=2,intercept=meaneff[1] , slope=meaneff[2]) + 
#    		geom_abline(linetype=2,intercept=meaneff[1] , slope=meaneff[2]) + 
#    		scale_x_continuous(xlab,breaks=c(-2,-1,0,1,2),labels=c('--','-','0','+','++')) + 
#    		scale_y_continuous(ylab,breaks=c(-2,-1,0,1,2),labels=c('--','-','0','+','++')) +
#    		labs(title=title)
#    }, error = function(e) {stop(simpleError('Dieser Wert fehlt bei Ihnen.'))})
#    }
#    
#    plot_both_waves = function(variable,title, xlab, ylab = 'Häufigkeit dieses Wertes in unserer Stichprobe',standardize=T,binwidth=0.25) {
#    	tryCatch({
#    	variable2 = paste0(variable,'_w2')
#    	if(standardize) {
#    	stand = scale(si[,variable])[,1]
#    	stand2 = scale(si[,variable2])[,1]
#    	} else 
#    	{
#    		stand = si[,variable]
#    		stand2 = si[,variable2]
#    	}
#    	zvalue = stand[which(si$id==id)]
#    	zvalue2 = stand2[which(si$id==id)]
#    	stopifnot(!is.na(zvalue),!is.na(zvalue))
#    	if(!is.na(zvalue)) {
#        
#    		if(standardize) {
#        if(zvalue>3) zvalue=3
#        if(zvalue<(-3)) zvalue=-3  
#        if(zvalue2>3) zvalue2=3
#        if(zvalue2<(-3)) zvalue2=-3  
#    		stand = stand + rnorm(length(stand),sd=0.2)
#    		stand2 = stand2 + rnorm(length(stand),sd=0.2)
#    
#        }
#    
#    	normal = data.frame(werte = seq(-3,3,length=2000),haeufigkeit = dnorm(seq(-3,3,length=2000),mean=0, sd=1))
#    		plot=
#    			ggplot()+
#    			geom_bar(binwidth=binwidth,data=as.data.frame(stand),aes(x=stand,y=..density..),alpha=0.3,fill=wave1colour)+
#    			geom_bar(binwidth=binwidth,data=as.data.frame(stand2),aes(x=stand2,y=..density..),alpha=0.3,fill=wave2colour)+
#    			geom_vline(colour=wave1colour,xintercept=zvalue,size=1.2)+
#    			geom_vline(colour=wave2colour,xintercept=zvalue2,size=1.2)+
#    			xlab(xlab)+
#        	ylab(ylab)+
#    			labs(title=title) + 
#    			scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4),labels=c('0%','10%','20%','30%','40%'))
#    		if(standardize)
#    			{
#    				plot + geom_line(data=normal,aes(x=werte,y=haeufigkeit),alpha=0.8)+
#      scale_x_continuous(breaks=c(-2,-1,0,1,2),labels=c('stark unter-\ndurchschnitlich','unterdurch-\nschnittlich','Durchschnitt','überdurch-\nschnittlich','stark über-\ndurchschnittlich'))
#    
#    		} else {
#    			plot
#    		}
#    
#    	}
#    	}, error = function(e) {stop(simpleError('Dieser Wert fehlt bei Ihnen.'))})
#    }
#    
#    individual_scaled = function(var) {
#    	scale(as.numeric(si[,var]))[which(si$id==id),1]
#    }
#    plot_change = function(w1,w2,title, xlab, ylab = 'Häufigkeit dieses Wertes in unserer Stichprobe') {
#    	tryCatch({
#    	change = data.frame(Welle = 1:2, Wert=c(individual_scaled(w1),individual_scaled(w2)))
#    ggplot(change,aes(x=Welle,y=Wert))+
#            geom_line(size=2)+
#            xlab(xlab)+
#            ylab(ylab)+
#            scale_y_continuous(limits=c(-3,3),breaks=c(-2,-1,0,1,2),labels=c('stark\n unterdurchschnitlich','unterdurchschnittlich','Durchschnitt','überdurchschnittlich','stark\n überdurchschnittlich'))+
#            scale_x_continuous(breaks=c(1,2),labels=c('Welle 1','Welle 2'))+
#            labs(title=title)
#    		}, error = function(e) {stop(simpleError('Dieser Wert fehlt bei Ihnen.'))})
#    
#    }
#    change_scaled = function(w1,w2) {
#    	sim = melt(si[,c('id',w1,w2)])
#    	sim$value = scale(as.numeric(sim[,'value']))
#    	sim$Welle = ifelse(str_detect(sim$variable,"_w2$"),2,1)
#    	rbind(
#    		sim[which(sim$id==id),c('id','Welle','value')],
#    		ddply(sim[which(sim$id!=id),],"Welle",summarise,id=NA,value=mean(value,na.rm=T))
#    	)
#    }
#    
#    plot_change_compared = function(w1,w2,title, xlab, ylab = 'Häufigkeit dieses Wertes im Vergleich mit anderen Teilnehmern') {
#    	tryCatch({
#    	change = change_scaled(w1,w2)
#    	stopifnot(!is.na(change$value))
#    	change$Wer = factor(ifelse(is.na(change$id),'Andere','Sie'))
#    ggplot(change,aes(x=Welle,y=value,colour=Wer))+
#            geom_line(size=1)+
#    				scale_colour_manual(values=youthey)+
#            xlab(xlab)+
#            ylab(ylab)+
#            scale_y_continuous(limits=c(-3,3),breaks=c(-2,-1,0,1,2),labels=c('stark\n unterdurchschnitlich','unterdurchschnittlich','Durchschnitt','überdurchschnittlich','stark\n überdurchschnittlich'))+
#            scale_x_continuous(breaks=c(1,2),labels=c('Welle 1','Welle 2'))+
#            labs(title=title)
#    	}, error = function(e) {stop(simpleError('Dieser Wert fehlt bei Ihnen.'))})
#    }
