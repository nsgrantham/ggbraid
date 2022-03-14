
#' @rdname geom_braid
#' @export
stat_braid <- function(
	mapping = NULL,
	data = NULL,
	geom = "braid",
	position = "identity",
	...,
	method = NULL,
	na.rm = FALSE,
	show.legend = NA,
	inherit.aes = TRUE
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
			method = method,
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

	setup_params = function(data, params) {
		msg <- character()
		if (is.null(params$method)) {
			params$method <- "line"
			msg <- c(msg, paste0("method = '", params$method, "'"))
		}
		if (length(msg) > 0) {
			message("`geom_braid()` using ", msg)
		}
		params
	},

	compute_panel = function(data, scales, method = NULL) {

		if (! "fill" %in% names(data)) {
			return(data)
		}

		data <- transform(data,
			y1 = ymin,
			y2 = ymax,
			ymin = pmin(ymin, ymax),
			ymax = pmax(ymin, ymax)
		)

		data <- data[order(data$x), ]

		if (! "fill" %in% names(data)) return(data)

		fill_groups <- unique(subset(data, select = c(fill, group)))
		fill_vals <- fill_groups$fill
		group_vals <- fill_groups$group

		# To define a braid we need to combine three different data frames.
		# We call these data frames braid1, braid2, and braid3.

		x_dups <- unique(data$x[duplicated(data$x)])

		braid1a <- data.frame()
		for (x_dup in x_dups) {
			sub <- unique(subset(data, x == x_dup))
			sub_lead <- transform(sub,
				y1 = lead1(y1),
				y2 = lead1(y2),
				fill = lead1(fill)
			)
			sub_lead <- transform(sub_lead,
				ymin = pmin(y1, y2),
				ymax = pmax(y1, y2),
				group = group_vals[sapply(fill, function(x) which(x == fill_vals))]
			)
			for (i in 1:nrow(sub)) {
				sub_i <- sub[i, ]
				sub_lead_i <- sub_lead[i, ]
				sub_next_i <- data.frame()
				if (sub_i$fill != sub_lead_i$fill) {
					is_y1_equal <- sub_i$y1 == sub_lead_i$y1
					is_y2_equal <- sub_i$y2 == sub_lead_i$y2
					if (is_y1_equal) {
						sub_next_i <- rbind(
							transform(sub_lead_i,
								ymin = y1,
								ymax = y1,
								fill = fill_vals[1],
								group = group_vals[1]
							),
							transform(sub_lead_i,
								ymin = y1,
								ymax = y1,
								fill = fill_vals[2],
								group = group_vals[2]
							)
						)
				  }
					if (is_y2_equal) {
						sub_next_i <- rbind(
							transform(sub_lead_i,
								ymin = y2,
								ymax = y2,
								fill = fill_vals[1],
								group = group_vals[1]
							),
							transform(sub_lead_i,
								ymin = y2,
								ymax = y2,
								fill = fill_vals[2],
								group = group_vals[2]
							)
						)
					}
				}
				braid1a <- rbind(braid1a, sub_i, sub_next_i)
			}
		}
		braid1a <- unique(subset(braid1a, select = -c(y1, y2)))

		braid1b <- subset(data,
			!(x %in% x_dups) & (ymax > ymin),
			select = -c(y1, y2)
		)

		# braid2 includes all intersection points in the original data, with
		# additional rows

		braid2 <- rbind(
			transform(data, fill = lag1(fill)),
			transform(data, fill = lead1(fill))
		)
		braid2 <- transform(braid2, group = as.integer(as.factor(fill)))
		braid2 <- subset(braid2, ymax == ymin, select = -c(y1, y2))
		braid2 <- unique(braid2)
		braid2 <- braid2[order(braid2$x), ]

		# Lastly, we calculate the intersection points between y1 and y2 that do not
		# exist in data.

		braid3 <- data.frame()

		if (identical(method, "line")) {
		  # Consider the intersection of two lines:
			# one defined by points (a, b) and (c, d), and another defined by points
			# (e, f) and (g, h).
		  #
	    #              • (g, h)
	    #             /
		  #  (a, b)    /
		  #        •--o--•
		  #          /    (c, d)
		  #         /
		  #        • (e, f)
		  #
		  # If b > f and d < h, or if b < f and d > h, then the two lines intersect
			# at a single point (x, y) defined by
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
		  braid3 <- subset(braid3,
		  	(a < c) & ((b > f) & (d < h)) | ((b < f) & (d > h))
		  )
		  print(braid3)
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
		}

		if (nrow(braid3) > 0) {
			fills <- unique(data$fill)
			groups <- unique(as.integer(as.factor(data$fill)))
			panel <- data$PANEL[1]
			braid3 <- rbind(
				transform(braid3, fill = fills[1], PANEL = panel, group = groups[1]),
				transform(braid3, fill = fills[2], PANEL = panel, group = groups[2])
			)
		}

		braid <- rbind(
			braid1a,
			braid1b,
			braid2,
			braid3
		)
		braid <- unique(braid[order(braid$x), ])
		braid
	}
)
