
remove_na <- function(data) {
	data[stats::complete.cases(data[, c("ymin", "ymax")]), ]
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
