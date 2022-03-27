
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

	default_aes = aes(fill = after_stat(braid)),

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
		data <- with(data, data[order(x), ])

		data <- transform(data,
			braid = ymin < ymax,
			y1 = ymin,
			y2 = ymax,
			ymin = pmin(ymin, ymax),
			ymax = pmax(ymin, ymax)
		)

		data <- transform(data, group = as.integer(braid) + 1)

		if (identical(method, "line")) {
			braided <- compute_braided_lines(data)
		}

		if (identical(method, "step")) {
			braided <- compute_braided_steps(data)
		}

		drop_vars <- c("y1", "y2")
		braided[, setdiff(colnames(braided), drop_vars)]
	}
)

compute_braided_lines <- function(data) {
	lines <- data.frame(matrix(nrow = 0, ncol = ncol(data)))
	colnames(lines) <- colnames(data)
	n <- nrow(data)
	for (i in 1:n) {
		curr_row <- data[i, ]
		lines <- rbind(lines, curr_row)

		if (i == n) {
			break
		}

		next_row <- data[i + 1, ]

		if (curr_row$braid == next_row$braid) {
			next
		}

		if (next_row$ymin == next_row$ymax) {  # explicit intersection
			lines <- rbind(
				lines,
			  transform(next_row, braid = curr_row$braid, group = curr_row$group)
			)
			next
		}

		if (next_row$x > curr_row$x) {
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

			a <- curr_row$x
			e <- curr_row$x
			c <- next_row$x
			g <- next_row$x
			b <- curr_row$y1
			f <- curr_row$y2
			d <- next_row$y1
			h <- next_row$y2

			w <- (a - c) * (f - h) - (b - d) * (e - g)
		 	u <- a * d - b * c
		 	v <- e * h - f * g

		 	x0 <- (u * (e - g) - v * (a - c)) / w
		 	y0 <- (u * (f - h) - v * (b - d)) / w

		 	lines <- rbind(
		 		lines,
		 		transform(curr_row, x = x0, ymin = y0, ymax = y0),
		 		transform(next_row, x = x0, ymin = y0, ymax = y0)
		 	)
		 	next
		}

		if (next_row$x == curr_row$x) {
			if (next_row$y1 == curr_row$y1) {
				lines <- rbind(
					lines,
					transform(curr_row, ymin = y1, ymax = y1),
					transform(next_row, ymin = y1, ymax = y1)
				)
			} else if (next_row$y2 == curr_row$y2) {
				lines <- rbind(
					lines,
					transform(curr_row, ymin = y2, ymax = y2),
					transform(next_row, ymin = y2, ymax = y2)
				)
			} else {
				# Two overlapping vertical lines so there are infinite intersections.
				# Define a single point to serve as a reasonable intersection.
				y2_mid <- (next_row$y2 + curr_row$y2) / 2
				y1_mid <- (next_row$y1 + curr_row$y1) / 2
				y0_mid <- (y1_mid + y2_mid) / 2
				lines <- rbind(
					lines,
					transform(curr_row, ymin = y0, ymax = y0),
					transform(next_row, ymin = y0, ymax = y0)
				)
			}
		}
	}
	lines
}

compute_braid_steps <- function(data) {
	steps <- data.frame(matrix(nrow = 0, ncol = ncol(data)))
	colnames(steps) <- colnames(data)
	n <- nrow(data)
	for (i in 1:n) {
		curr_row <- data[i, ]
		steps <- rbind(steps, curr_row)

		if (i == n) {
			break
		}

		next_row <- data[i + 1, ]

		if (curr_row$braid == next_row$braid) {
			steps <- rbind(
				steps,
				transform(curr_row, x = next_row$x)
			)
			next
		}

		if (curr_row$ymin == curr_row$ymax) {
			steps <- rbind(
				steps,
			  transform(curr_row, x = next_row$x),
				transform(curr_row, x = next_row$x, braid = next_row$braid, group = next_row$group)
			)
			next
		}

		if (next_row$ymin == next_row$ymax) {
			steps <- rbind(
				steps,
			  transform(curr_row, x = next_row$x),
				transform(next_row, braid = curr_row$braid, group = curr_row$group)
			)
			next
		}

		if (next_row$y1 == curr_row$y1) {
			steps <- rbind(
				steps,
				transform(curr_row, x = next_row$x),
				transform(curr_row, x = next_row$x, ymin = y1, ymax = y1),
				transform(next_row, ymin = y1, ymax = y1)
			)
		} else if (next_row$y2 == curr_row$y2) {
			steps <- rbind(
				steps,
				transform(curr_row, x = next_row$x),
				transform(curr_row, x = next_row$x, ymin = y2, ymax = y2),
				transform(next_row, ymin = y2, ymax = y2)
			)
		} else {
			# Two overlapping vertical lines so there are infinite intersections.
			# Define a single point to serve as a reasonable intersection.
			y2_mid <- (next_row$y2 + curr_row$y2) / 2
			y1_mid <- (next_row$y1 + curr_row$y1) / 2
			y0_mid <- (y1_mid + y2_mid) / 2
			steps <- rbind(
				steps,
				transform(curr_row, x = next_row$x),
				transform(curr_row, x = next_row$x, ymin = y0, ymax = y0),
				transform(next_row, ymin = y0, ymax = y0)
			)
		}
	}
	steps
}
