
#' @rdname geom_braid
#' @export
stat_braid <- function(
	mapping = NULL,
	data = NULL,
	geom = "braid",
	position = "identity",
	...,
	method = NULL,
	na.rm = NA,
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


#' Proto braid
#' @rdname ggbraid-ggproto
#' @format NULL
#' @usage NULL
#' @keywords internal
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
		data$flipped_aes <- params$flipped_aes
		data <- flip_data(data, params$flipped_aes)

		data <- with(data, data[order(x), ])
		data <- with(data, data[!is.na(x), ])

		has_fill <- "fill" %in% colnames(data)

		if (has_fill) {
			data <- transform(data, braid = as.logical(as.integer(as.factor(fill)) - 1))
		} else {
			data <- transform(data, braid = ymin < ymax)
		}
		data <- transform(data, group = as.integer(braid) + 1)
		data$group[is.na(data$braid)] <- -1

		if (any(is.na(data[, c("ymin", "ymax")]))) {
			if (is.na(params$na.rm)) {
				data <- impute_na(data, method = params$method)
			} else if (params$na.rm) {
				data <- remove_na(data)
			} else {
				data <- keep_na(data, method = params$method)
			}
		}

		flip_data(data, params$flipped_aes)
	},

	compute_panel = function(data, scales, method = NULL, flipped_aes = FALSE) {
		data <- flip_data(data, flipped_aes)

		has_fill <- "fill" %in% colnames(data)
		if (has_fill) {
			braid_map <- get_braid_map(data)
			braid_map$group <- NULL
			data$fill <- NULL
		}

		data <- transform(data, y1 = ymin, y2 = ymax)
		data <- transform(data, ymin = pmin(y1, y2), ymax = pmax(y1, y2))

		if (identical(method, "line")) {
			braided <- compute_braided_lines(data)
		}

		if (identical(method, "step")) {
			braided <- compute_braided_steps(data)
		}

		if (has_fill) {
			braided <- transform(braided, row_id = 1:nrow(braided))
			braided <- merge(
				braided[, !(colnames(braided) %in% c("fill"))],
				braid_map,
				by = "braid",
				all.x = TRUE,
				sort = FALSE
			)
			braided <- with(braided, braided[order(row_id), ])
			braided <- subset(braided, select = -row_id)
		}

		braided <- subset(braided, select = -c(y1, y2))
		rownames(braided) <- NULL

		flip_data(braided, flipped_aes)
	}
)

#' @importFrom stats na.omit
get_braid_op <- function(data) {
	braid_na.omit <- na.omit(data$braid)
	braid_ops <- c(`<`, `<=`, `>`, `>=`)
	for (braid_op in braid_ops) {
		if (all(braid_na.omit == na.omit(braid_op(data$ymin, data$ymax)))) {
			return(braid_op)
		}
	}
	`<`
}

impute_na <- function(data, method) {
	braid_map <- get_braid_map(data)
	braid_op <- get_braid_op(data)

	n <- nrow(data)
	for (i in 2:n) {
		x_curr <- data$x[i]
		x_prev <- data$x[i-1]

		ymin_curr <- data$ymin[i]
		if (is.na(ymin_curr)) {
			ymin_prev <- data$ymin[i-1]
			i_next <- which(!is.na(data$ymin[i:n]))[1] + i - 1
			if (!is.na(i_next)) {
				if (identical(method, "line")) {
					x_next <- data$x[i_next]
					ymin_next <- data$ymin[i_next]
					r <- if (x_next > x_prev) (x_curr - x_prev) / (x_next - x_prev) else 0
					data[i, "ymin"] <- ymin_prev + r * (ymin_next - ymin_prev)
				}
				if (identical(method, "step")) {
					data[i, "ymin"] <- ymin_prev
				}
			}
		}

		ymax_curr <- data$ymax[i]
		if (is.na(ymax_curr)) {
			ymax_prev <- data$ymax[i-1]
			i_next <- which(!is.na(data$ymax[i:n]))[1] + i - 1
			if (!is.na(i_next)) {
				if (identical(method, "line")) {
					x_next <- data$x[i_next]
					ymax_next <- data$ymax[i_next]
					r <- if (x_next > x_prev) (x_curr - x_prev) / (x_next - x_prev) else 0
					data[i, "ymax"] <- ymax_prev + r * (ymax_next - ymax_prev)
				}
				if (identical(method, "step")) {
					data[i, "ymax"] <- ymax_prev
				}
			}
		}
	}

	row_id <- ymin <- ymax <- NULL  # only included to silence notes in devtools::check()
	rows <- rownames(data)
	data <- transform(data, row_id = 1:n, braid = braid_op(ymin, ymax))
	data <- data[, !(colnames(data) %in% c("fill", "group"))]
	data <- merge(data, braid_map, by = "braid", sort = FALSE)
	data <- with(data, data[order(row_id), ])
	data <- subset(data, select = -row_id)
	rownames(data) <- rows

	remove_na(data)
}

get_braid_map <- function(data) {
	braid_map <- merge(
		data.frame(braid = c(TRUE, FALSE, NA), group = c(2, 1, -1)),
		unique(data[, colnames(data) %in% c("braid", "fill"), drop = FALSE]),
		by = "braid",
		all.x = TRUE,
		sort = FALSE
	)
	if ("fill" %in% colnames(braid_map)) {
		is_fill_missing <- with(braid_map, is.na(fill) & !is.na(braid))
		braid_map[is_fill_missing, "fill"] <- braid_map[is_fill_missing, "braid"]
	}
	braid_map
}

keep_na <- function(data, method) {
	n <- nrow(data)
	is_prev_na <- TRUE

	for (i in 1:n) {
		ymin <- data$ymin[i]
		ymax <- data$ymax[i]

		if (identical(method, "step")) {
			if (is.na(ymin)) {
				data[i, "ymin"] <- if (i == 1) NA else data$ymin[i-1]
			}
			if (is.na(ymax)) {
				data[i, "ymax"] <- if (i == 1) NA else data$ymax[i-1]
			}
		}

		if (any(is.na(c(ymin, ymax))) && !is_prev_na) {
			data[(i+1):n, "group"] <- data[(i+1):n, "group"] + 2
		}

		braid <- data$braid[i]
		if (is.na(braid)) {
			data[i, "braid"] <- if (i == 1) NA else data$braid[i-1]
			if (is_prev_na || identical(method, "line")) {
				data[i, "group"] <- -1
			} else {
				data[i, "group"] <- data$group[i-1]
			}
		}
		is_prev_na <- is.na(braid)
	}

	remove_na(data)
}

remove_na <- function(data) {
	data[stats::complete.cases(data[, c("ymin", "ymax")]), ]
}

compute_braided_lines <- function(data) {
	splits <- cut(data$group, seq(0.5, max(data$group) + 1.5, by = 2))
	do.call(rbind, lapply(split(data, splits), braid_lines))
}

braid_lines <- function(data) {
	row_pairs <- lapply(1:nrow(data), function(i) data[i:(i+1), ])
	do.call(rbind, lapply(row_pairs, braid_lines_row_pair))
}

braid_lines_row_pair <- function(row_pair) {
	y1 <- y2 <- NULL  # only included to silence notes in devtools::check()
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
	splits <- cut(data$group, seq(0.5, max(data$group) + 1.5, by = 2))
	do.call(rbind, lapply(split(data, splits), braid_steps))
}

braid_steps <- function (data) {
	row_pairs <- lapply(1:nrow(data), function(i) data[i:(i+1), ])
	do.call(rbind, lapply(row_pairs, braid_steps_row_pair))
}

braid_steps_row_pair <- function(row_pair) {
	y1 <- y2 <- NULL  # only included to silence notes in devtools::check()
	row1 <- row_pair[1, ]
	row2 <- row_pair[2, ]

	if (is.na(row2$braid)) {
		return(row1)
	}

	if (row1$braid == row2$braid) {
		return(
			rbind(
				row1,
				transform(row1, x = row2$x, group = row2$group)
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
