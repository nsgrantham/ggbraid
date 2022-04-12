#' Braids
#'
#' `geom_braid()` is an extension of `geom_ribbon()` to
#' appropriately fill the area between two alternating lines
#' with different colors, whether one line is over or under
#' the other.
#'
#' @seealso [geom_ribbon()] for non-alternating lines
#'
#' @export
geom_braid <- function(
	mapping = NULL,
	data = NULL,
	stat = "braid",
	position = "identity",
	...,
	method = NULL,
	na.impute = FALSE,
	na.rm = FALSE,
	show.legend = NA,
	inherit.aes = TRUE
) {

	params <- list(
		na.rm = na.rm,
		...
	)

	if (identical(stat, "braid")) {
		params$method <- method
		params$na.impute <- na.impute
	}

	layer(
		data = data,
		mapping = mapping,
		stat = stat,
		geom = GeomBraid,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = params
	)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBraid <- ggproto("GeomBraid", GeomRibbon,
	required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),

	setup_params = function(data, params) {
		GeomRibbon$setup_params(data, params)
	},

	setup_data = function(data, params) {
		GeomRibbon$setup_data(data, params)
	}
)
