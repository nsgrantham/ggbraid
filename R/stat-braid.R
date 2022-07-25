
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

