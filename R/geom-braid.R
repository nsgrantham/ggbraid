#' Braided ribbons
#'
#' `geom_braid()` is an extension of `geom_ribbon()` that uses `stat_braid()`
#' to correctly fill the area between two alternating series (lines or steps)
#' with two different colors.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_ribbon
#' @param method Intersection and imputation method to use to braid the ribbon,
#'   accepts `NULL`, `"line"`, or `"step"`. For `method = NULL`, the default,
#'   print a message to the console and use `method = "line"`. For
#'   `method = "line"`, silently braid the ribbon with two series drawn by
#'   `geom_line()` or `geom_path()`. For `method = "step"`, silently braid the
#'   ribbon with two series drawn by `geom_step()`.
#' @param na.rm If `NA`, the default, missing values are imputed by
#'   `method`. If `FALSE`, missing values are kept and appear as gaps in the
#'   ribbon. If `TRUE`, missing values are removed.
#' @param geom Override the default connection with `geom_braid()`.
#' @return A ggplot2 layer that can be added to a plot created with `ggplot()`.
#' @examples
#' library(ggplot2)
#'
#' # To demonstrate the features of `geom_braid()` we'll use a subset of the
#' # `txhousing` dataset from ggplot2.
#'
#' tx_long <- with(txhousing, txhousing[city %in% c("Dallas", "Austin"), ])
#' tx_long <- with(tx_long, tx_long[date >= 2008, ])
#' tx_long <- subset(tx_long, select = c(date, city, inventory))
#'
#' tx_wide <- data.frame(
#'   date = with(tx_long, date[city == "Dallas"]),
#'   dallas = with(tx_long, inventory[city == "Dallas"]),
#'   austin = with(tx_long, inventory[city == "Austin"])
#' )
#' tx_wide <- with(tx_wide, tx_wide[date >= 2008, ])
#'
#' p <- ggplot(tx_long, aes(date))
#'
#' p + geom_line(aes(y = inventory, linetype = city))
#'
#' # Use `geom_braid()` to draw a ribbon between the two lines, just as we would
#' # with `geom_ribbon()`.
#' p +
#'   geom_line(aes(y = inventory, linetype = city)) +
#'   geom_braid(aes(ymin = austin, ymax = dallas), data = tx_wide, alpha = 0.3)
#'
#' # Now fill the ribbon between the two series with different colors depending
#' # on which series is over or under the other. Do so by mapping any of the
#' # following to the `fill` aesthetic:
#' #   `austin < dallas`
#' #   `austin > dallas`
#' #   `austin <= dallas`
#' #   `austin >= dallas`
#' p +
#'   geom_line(aes(y = inventory, linetype = city)) +
#'   geom_braid(
#'     aes(ymin = austin, ymax = dallas, fill = austin > dallas),
#'     data = tx_wide,
#'     method = "line",
#'     alpha = 0.6
#'   )
#'
#' # Alternatively, map `after_stat(braid)` to `fill` which will apply
#' # `ymin < ymax` by default, in this case `austin < dallas`
#' p +
#'   geom_line(aes(y = inventory, linetype = city)) +
#'   geom_braid(
#'     aes(ymin = austin, ymax = dallas, fill = after_stat(braid)),
#'     data = tx_wide,
#'     method = "line",
#'     alpha = 0.6
#'   )
#'
#' # To braid a ribbon with two series drawn with `geom_step()`, use
#' # `method = "step"` in `geom_braid()`.
#' p +
#'   geom_step(aes(y = inventory, linetype = city)) +
#'   geom_braid(
#'     aes(ymin = austin, ymax = dallas),
#'     data = tx_wide,
#'     method = "step",
#'     alpha = 0.3
#'   )
#'
#' p +
#'   geom_step(aes(y = inventory, linetype = city)) +
#'   geom_braid(
#'     aes(ymin = austin, ymax = dallas, fill = austin < dallas),
#'     data = tx_wide,
#'     method = "step",
#'     alpha = 0.6
#'   )
#'
#' # How does `geom_braid()` handle missing values? Let's replace some existing
#' # values with `NA`s to demonstrate.
#'
#' set.seed(42)  # for reproducibility
#'
#' tx_long[sample(1:nrow(tx_long), 20), "inventory"] <- NA
#'
#' tx_wide <- transform(tx_wide,
#'   dallas = with(tx_long, inventory[city == "Dallas"]),
#'   austin = with(tx_long, inventory[city == "Austin"])
#' )
#'
#' p <- ggplot(tx_long, aes(date))
#'
#' p + geom_line(aes(y = inventory, linetype = city), na.rm = TRUE)
#'
#' # If `na.rm = NA`, the default, `geom_braid()` imputes missing values that
#' # occur between observations in a series.
#' p +
#'   geom_line(aes(y = inventory, linetype = city), na.rm = TRUE) +
#'   geom_braid(
#'     aes(ymin = austin, ymax = dallas, fill = austin < dallas),
#'     data = tx_wide,
#'     method = "line",
#'     alpha = 0.6
#'   )
#'
#' # If `na.rm = FALSE`, `geom_braid()` keeps the missing values and portrays
#' # them as gaps in the ribbon.
#' p +
#'   geom_line(aes(y = inventory, linetype = city), na.rm = TRUE) +
#'   geom_braid(
#'     aes(ymin = austin, ymax = dallas, fill = austin < dallas),
#'     data = tx_wide,
#'     method = "line",
#'     alpha = 0.6,
#'     na.rm = FALSE
#'   )
#'
#' # If `na.rm = TRUE`, `geom_braid()` removes the missing values. However,
#' # because this removes rows in `tx_wide` where only one of `austin` and
#' # `dallas` may be missing, the resulting ribbon will likely not match the
#' # lines drawn with `geom_line()` using `tx_long`.
#' p +
#'   geom_line(aes(y = inventory, linetype = city), na.rm = TRUE) +
#'   geom_braid(
#'     aes(ymin = austin, ymax = dallas, fill = austin < dallas),
#'     data = tx_wide,
#'     method = "line",
#'     alpha = 0.6,
#'     na.rm = TRUE
#'   )
#'
#' # Happy braiding!
#' @export
geom_braid <- function(
	mapping = NULL,
	data = NULL,
	position = "identity",
	...,
	method = NULL,
	na.rm = NA,
	show.legend = NA,
	inherit.aes = TRUE
) {

	params <- list(
		na.rm = na.rm,
		method = method,
		...
	)

	layer(
		data = data,
		mapping = mapping,
		stat = "braid",
		geom = GeomBraid,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = params
	)
}

#' Proto braid
#' @rdname ggbraid-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBraid <- ggproto("GeomBraid", GeomRibbon,
	required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),

	setup_params = function(data, params) {
		if (is.na(params$na.rm)) {
			params$na.rm <- FALSE
		}
		GeomRibbon$setup_params(data, params)
	},

	setup_data = function(data, params) {
		GeomRibbon$setup_data(data, params)
	}
)
