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
