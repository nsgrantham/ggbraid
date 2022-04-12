
#' @rdname geom_braid
#' @export
stat_braid <- function(
	mapping = NULL,
	data = NULL,
	geom = "braid",
	position = "identity",
	...,
	method = NULL,
	na.impute = FALSE,
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
			na.impute = na.impute,
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

	required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),

	setup_params = function(data, params) {
		params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = TRUE)

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

	setup_data = function(data, params) {
		if (params$na.impute) {
			data <- impute_na(data)
		}
		data
	},

	compute_panel = function(data, scales, method = NULL, flipped_aes = FALSE, na.impute = FALSE) {
		data$flipped_aes <- flipped_aes
		data <- flip_data(data, flipped_aes)

		has_fill <- "fill" %in% colnames(data)

		data <- with(data, data[order(x), ])

		data <- transform(data,
			braid = if (has_fill) fill else ymin < ymax,
			y1 = ymin,
			y2 = ymax
		)

		data <- transform(data,
			group = if (has_fill) group else as.integer(braid) + 1,
			ymin = pmin(y1, y2),
			ymax = pmax(y1, y2)
		)

		data$fill <- NULL

		if (identical(method, "line")) {
			braided <- compute_braided_lines(data)
		}

		if (identical(method, "step")) {
			braided <- compute_braided_steps(data)
		}

		if (has_fill) {
			braided <- transform(braided, fill = braid)
		}

		braided <- subset(braided, select = -c(y1, y2))
		flip_data(braided, flipped_aes)
	}
)

impute_na <- function(data) {
	has_fill <- "fill" %in% colnames(data)
	if (has_fill) {
		braid_fill_group <- unique(
			subset(
				transform(data, braid = ymin < ymax),
				select = c(braid, fill, group)
			)
		)
	}

	n <- nrow(data)
	for (i in 1:n) {
		x_curr <- data$x[i]
		x_prev <- data$x[i-1]

		ymin_curr <- data$ymin[i]
		if (is.na(ymin_curr)) {
			i_next <- which(!is.na(data$ymin[i:n]))[1] + i - 1
			x_next <- data$x[i_next]
			ymin_next <- data$ymin[i_next]
			ymin_prev <- data$ymin[i-1]
			r <- if (x_next > x_prev) (x_curr - x_prev) / (x_next - x_prev) else 0
			data[i, "ymin"] <- ymin_prev + r * (ymin_next - ymin_prev)
		}

		ymax_curr <- data$ymax[i]
		if (is.na(ymax_curr)) {
			i_next <- which(!is.na(data$ymax[i:n]))[1] + i - 1
			x_next <- data$x[i_next]
			ymax_next <- data$ymax[i_next]
			ymax_prev <- data$ymax[i-1]
			r <- if (x_next > x_prev) (x_curr - x_prev) / (x_next - x_prev) else 0
			data[i, "ymax"] <- ymax_prev + r * (ymax_next - ymax_prev)
		}
	}

	if (has_fill) {
		rows <- rownames(data)
		data <- transform(data, row_id = 1:nrow(data), braid = ymin < ymax)
		data <- subset(data, select = -c(fill, group))
		data <- merge(data, braid_fill_group, by = "braid", sort = FALSE)
		data <- with(data, data[order(row_id), ])
		rownames(data) <- rows
		data <- subset(data, select = -c(row_id, braid))
	}

	data
}

compute_braided_lines <- function(data) {
	row_pairs <- lapply(1:nrow(data), function(i) data[i:(i+1), ])
	do.call(rbind, lapply(row_pairs, braid_lines_row_pair))
}

braid_lines_row_pair <- function(row_pair) {
	row1 <- row_pair[1, ]
	row2 <- row_pair[2, ]

	if (is.na(row2$braid)) {
		return(row1)
	}

	if (row1$braid == row2$braid) {
		return(row1)
	}

	if (row2$ymin == row2$ymax) {  # explicit intersection
		return(
			rbind(
				row1,
				transform(row2, braid = row1$braid, group = row1$group)
			)
		)
	}

	if (row1$x < row2$x) {
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
		# at a single point (x0, y0) defined by
	  #   x0 = (u * (e - g) - v * (a - c)) / w
	  # 	y0 = (u * (f - h) - v * (b - d)) / w
	  # where
	  #		u = a * d - b * c
	  #   v = e * h - f * g
	  #   w = (a - c) * (f - h) - (b - d) * (e - g)
	  #
	  # For more information on this formula, visit
	  # https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line

		a <- row1$x
		e <- row1$x
		c <- row2$x
		g <- row2$x
		b <- row1$y1
		f <- row1$y2
		d <- row2$y1
		h <- row2$y2

		w <- (a - c) * (f - h) - (b - d) * (e - g)
	 	u <- a * d - b * c
	 	v <- e * h - f * g

	 	x0 <- (u * (e - g) - v * (a - c)) / w
	 	y0 <- (u * (f - h) - v * (b - d)) / w

	 	return(
	 		rbind(
	 			row1,
	 			transform(row1, x = x0, ymin = y0, ymax = y0),
	 			transform(row2, x = x0, ymin = y0, ymax = y0)
	 		)
		)
	}

	if (row1$x == row2$x) {
		if (row1$y1 == row2$y1) {
			return(
				rbind(
					row1,
					transform(row1, ymin = y1, ymax = y1),
					transform(row2, ymin = y1, ymax = y1)
				)
			)
		} else if (row1$y2 == row2$y2) {
			return(
				rbind(
					row1,
					transform(row1, ymin = y2, ymax = y2),
					transform(row2, ymin = y2, ymax = y2)
				)
			)
		} else {
			# Two overlapping vertical lines so there are infinite intersections.
			# Define a single point to serve as a reasonable intersection.
			y2_mid <- (row1$y2 + row2$y2) / 2
			y1_mid <- (row1$y1 + row2$y1) / 2
			y0 <- (y1_mid + y2_mid) / 2
			return(
				rbind(
					row1,
					transform(row1, ymin = y0, ymax = y0),
					transform(row2, ymin = y0, ymax = y0)
				)
			)
		}
	}
}

compute_braided_steps <- function(data) {
	row_pairs <- lapply(1:nrow(data), function(i) data[i:(i+1), ])
	do.call(rbind, lapply(row_pairs, braid_steps_row_pair))
}

braid_steps_row_pair <- function(row_pair) {
	row1 <- row_pair[1, ]
	row2 <- row_pair[2, ]

	if (is.na(row2$braid)) {
		return(row1)
	}

	if (row1$braid == row2$braid) {
		return(
			rbind(
				row1,
				transform(row1, x = row2$x)
			)
		)
	}

	if (row1$ymin == row1$ymax) {
		return(
			rbind(
				row1,
		  	transform(row1, x = row2$x),
				transform(row1, x = row2$x, braid = row2$braid, group = row2$group)
			)
		)
	}

	if (row2$ymin == row2$ymax) {
		return(
			rbind(
				row1,
		  	transform(row1, x = row2$x),
				transform(row2, braid = row1$braid, group = row1$group)
			)
		)
	}

	if (row1$y1 == row2$y1) {
		return(
			rbind(
				row1,
				transform(row1, x = row2$x),
				transform(row1, x = row2$x, ymin = y1, ymax = y1),
				transform(row2, ymin = y1, ymax = y1)
			)
		)
	} else if (row1$y2 == row2$y2) {
		return(
			rbind(
				row1,
				transform(row1, x = row2$x),
				transform(row1, x = row2$x, ymin = y2, ymax = y2),
				transform(row2, ymin = y2, ymax = y2)
			)
		)
	} else {
		# Two overlapping vertical lines so there are infinite intersections.
		# Define a single point to serve as a reasonable intersection.
		y2_mid <- (row1$y2 + row1$y2) / 2
		y1_mid <- (row1$y1 + row2$y1) / 2
		y0 <- (y1_mid + y2_mid) / 2
		return(
			rbind(
				row1,
				transform(row1, x = row2$x),
				transform(row1, x = row2$x, ymin = y0, ymax = y0),
				transform(row2, ymin = y0, ymax = y0)
			)
		)
	}
}
