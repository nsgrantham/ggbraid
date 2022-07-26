test_that("geom_braid works in both directions", {
  df <- data.frame(
  	x = seq_len(5),
  	a = c(1, 2, 1.5, 1.8, 1),
    b = c(4, 6, 5, 4.5, 5.2)
  )

  p <- ggplot(df, aes(x, ymin = a, ymax = b)) + geom_braid(method = "line")
	x <- layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(df, aes(y = x, xmin = a, xmax = b)) + geom_braid(method = "line")
  y <- layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, flip_data(y, flip = TRUE)[, names(x)])
})
