
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
