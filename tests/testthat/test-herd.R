
test_that(
  "correct inputs work",
  {

    df <- data.frame(
      id = paste(1:100),
      x = factor(rep(letters[1:2], 50)),
      z = rep(letters[3:4], 50),
      y = 1:100
    )

    expect_error(
      cat_spread(df, y),
      regexp = 'No categorical'
    )

    one_hot_df <- cat_spread(df, x)

    expect_true(all(as.numeric(df$x == 'a') == one_hot_df$x_a))
    expect_true(all(as.numeric(df$x == 'b') == one_hot_df$x_b))

    one_hot_df <- cat_spread(df, -id)

    expect_equal(
      names(one_hot_df),
      c('id', 'x_a', 'x_b', 'z_c', 'z_d', 'y')
    )

    no_hot_df <- cat_gather(
      one_hot_df,
      factor_levels = list(x = c("a", "b"), z = c("c", "d"))
    )

    expect_true(all(df$x==no_hot_df$x))

    no_hot_df <- cat_gather(one_hot_df,
      factor_levels = get_factor_levels(df, -id)
    )

    expect_true(all(df$x == no_hot_df$x))
    expect_true(all(df$y == no_hot_df$y))
    expect_true(all(df$z == no_hot_df$z))

    df_nocats <- df
    df_nocats[, c('x','z')] <- NULL

    df_train <- data.frame(
      x = c("A","B","C","D"),
      y = 1:4
    )

    expect_warning(cat_spread(df_train), regexp = ': x')


    df_train <- data.frame(
      x = c("A","B","C","D","D"),
      y = 1:5
    )

    df_test <- data.frame(
      x = c("A","A","B","C","C"),
      y = 2:6
    )

    df_test <- cat_transfer(to = df_test, from = df_train)

    df_test_spread <- cat_spread(df_test)

    expect_true(all(df_test$x_D==0))

    expect_equal(
      names(df_test_spread),
      names(cat_spread(df_train))
    )

    df_bad_test <- data.frame(
      x = c("A","A","B","C"),
      y = 2:5,
      z = c('a','b','c','d')
    )

    df_bad_train <- data.frame(
      x = c("A","A","B","C"),
      y = 2:5,
      z = c('a','b','c','d')
    )

    expect_error(
      object = cat_transfer(to = df_bad_test, from = df_train),
      regexp = "from: z"
    )

    expect_error(
      object = cat_transfer(to = df_test, from = df_bad_train),
      regexp = "to: z"
    )

    df_bad_train <- data.frame(
      x = c("A","A","B","C"),
      y = 2:5,
      z = c('a','b','c','d'),
      stringsAsFactors = FALSE
    )

    df_test <- data.frame(
      x = c("A","A","B","C"),
      y = 2:5,
      z = c('a','b','c','d'),
      stringsAsFactors = FALSE
    )

    expect_warning(
      cat_transfer(to = df_test, from = df_bad_train),
      regexp = "x and z"
    )

  })
