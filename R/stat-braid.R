
#' @rdname geom_braid
#' @export
stat_braid <- function(
	mapping = NULL,
	data = NULL,
	geom = "braid",
	position = "identity",
	na.rm = FALSE,
	show.legend = NA,
	inherit.aes = TRUE,
	...
) {
	layer(
		stat = StatBraid,
		data = data,
		mapping = mapping,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(
			na.rm = na.rm,
			...
		)
	)
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBraid <- ggproto("StatBraid", Stat,

	required_aes = c("x", "ymin", "ymax"),

	compute_panel = function(data, scales, params) {

		data <- transform(data,
			y1 = ymin,
			y2 = ymax,
			ymin = pmin(ymin, ymax),
			ymax = pmax(ymin, ymax)
		)

		if (! "fill" %in% names(data)) return(data)

		data <- data[order(data$x), ]

		# To define a braid we need to combine three different data frames.
		# We call these data frames braid1, braid2, and braid3.

		# The simplest data frame is braid1. It contains all rows in data for which
		# ymax > ymin. Put another way, it includes all non-intersection points in
		# the original data.

		braid1 <- subset(data, ymax > ymin, select = -c(y1, y2))

		# braid2 includes all intersection points in the original data, with
		# additional rows

		braid2 <- rbind(
			transform(data, fill = lag1(fill)),
			transform(data, fill = lead1(fill))
		)
		braid2 <- transform(braid2, group = as.integer(as.factor(fill)))
		braid2 <- subset(braid2, ymax == ymin, select = -c(y1, y2))

		# Lastly, we calculate the intersection points between y1 and y2 that do not
		# exist in data.
		#
		# Consider the intersection of two lines: one defined by points (a, b) and
		# (c, d), and one defined by points (e, f) and (g, h).
		#
	  #              • (g, h)
	  #             /
		#  (a, b)    /
		#        •--o--•
		#          /    (c, d)
		#         /
		#        • (e, f)
		#
		# If b > f and d < h, or if b < f and d > h, then the two lines intersect at
		# a single point (x, y) defined by
		#   x = (u * (e - g) - v * (a - c)) / w
		# 	y = (u * (f - h) - v * (b - d)) / w
		# where
		#		u = a * d - b * c
		#   v = e * h - f * g
		#   w = (a - c) * (f - h) - (b - d) * (e - g)
		#
		# For more information on this formula, visit
		# https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line

		braid3 <- transform(data,
			a = x,  c = lead1(x),  e = x,  g = lead1(x),
	    b = y1, d = lead1(y1), f = y2, h = lead1(y2)
		)
		braid3 <- subset(braid3, ((b > f) & (d < h)) | ((b < f) & (d > h)))
		braid3 <- transform(braid3,
			w = (a - c) * (f - h) - (b - d) * (e - g),
			u = a * d - b * c,
			v = e * h - f * g
		)
		braid3 <- transform(braid3,
			x = (u * (e - g) - v * (a - c)) / w,
			y = (u * (f - h) - v * (b - d)) / w
		)
		braid3 <- transform(braid3, ymax = y, ymin = y)
		braid3 <- subset(braid3, select = x:ymax)
		fills <- unique(data$fill)
		groups <- unique(as.integer(as.factor(data$fill)))
		panel <- data$PANEL[1]
		braid3 <- rbind(
			transform(braid3, fill = fills[1], PANEL = panel, group = groups[1]),
			transform(braid3, fill = fills[2], PANEL = panel, group = groups[2])
		)

		rbind(braid1, braid2, braid3)
	}
)
