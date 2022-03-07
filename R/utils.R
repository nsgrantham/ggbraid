lag1 <- function(x) {
	# c(1, 2, 3) --> c(1, 1, 2)
	c(head(x, 1), head(x, length(x) - 1))
}

lead1 <- function(x) {
	# c(1, 2, 3) --> c(2, 2, 3)
	c(tail(x, length(x) - 1), tail(x, 1))
}
