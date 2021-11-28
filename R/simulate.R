
simulate_rcrds <- function(.data, ...) {
  dots <- enquos(..., .named = TRUE)
  dots_nms <- names(dots)
  out <- .data
  for(i in seq_along(dots)) {
    arcrd <- dots_nms[i]
    res <- eval_tidy(dots[[i]], list(data = out, var = arcrd))
    out[[arcrd]] <- new_edibble_rcrd(res$values,
                                     unit_name = .data[[arcrd]] %@% "unit_name",
                                     unit_values = .data[[arcrd]] %@% "unit_values",
                                     effects = res$effects)

  }
  out
}

true_effects <- function(f, effects) {
  vlev <- levels(f)
  res <- setNames(rep(0, length(vlev)), vlev)
  if(is.list(effects)) {
    res[1:length(effects$values)] <- effects$values
    true_effects(f, res)
  } else if(is_named(effects)) {
    veff <- names(effects)
    vmiss <- setdiff(vlev, veff)
    res[veff] <- effects
    unname(res[f])
  } else if(is_vector(effects)) {
    res[1:length(effects)] <- effects
    true_effects(f, res)
  }
}

#' @export
params <- function(...) {
  structure(list2(...),
            class = "parameters")
}

sim_fixed <- function(mean = 0, params = NULL) {
  e <- caller_env(n = 1)$.top_env
  res <- e$data
  n <- nrow(res)
  prms <- params[["mean"]] %||% params
  # effects
  evars <- names(prms)
  eff <- data.frame(row.names = seq(n))
  for(avar in evars) {
    if(is_formula(prms[[avar]])) {
      prms[[avar]] <- eval_tidy(get_expr(prms[[avar]]),
                                   list(data = e$data, var = avar))
    }
    eff[[avar]] <- true_effects(res[[avar]], prms[[avar]])
  }
  # mean
  if(is_formula(mean)) {
    vars <- all.vars(mean)
    check_var_exists(res %@% "design", label = unique(c(vars, evars)))
    mean <- eval(get_expr(do.call("substitute", list(mean, eff))))
  }
  list(values = mean, effects = eff)
}



eval_effects <- function(data, params, input, n) {
  res <- data.frame(row.names = seq(n))
  prms <- names(params)
  for(avar in prms) {
    res[[avar]] <- true_effects(data[[avar]], params[[avar]])
  }
  if(is_formula(input)) {
    vars <- all.vars(input)
    # check_var_exists(data %@% "design", label = unique(c(vars, prms)))
    input <- eval(get_expr(do.call("substitute", list(input, res))))
  }
  list(input = input,
       effects = res)
}

sim_beta <- function(shape1, shape2, params = NULL) {
  info <- get_topenv_info()
  prm_shape1 <- eval_effects(info$data, params$shape1, shape1, info$n)
  prm_shape2 <- eval_effects(info$data, params$shape2, shape2, info$n)
  list(values = stats::rbeta(info$n, prm_shape1$input, prm_shape2$input),
       effects = list(shape1 = prm_shape1,
                      shape2 = prm_shape2))
}

sim_cauchy <- function(location, scale, params = NULL) {
  info <- get_topenv_info()
  prm_location <- eval_effects(info$data, params$location, location, info$n)
  prm_scale <- eval_effects(info$data, params$scale, scale, info$n)
  list(values = stats::rcauchy(info$n, prm_location$input, prm_scale$input),
       effects = list(shape1 = prm_location,
                      shape2 = prm_scale))
}

sim_chisq <- function(df, ncp = 0, params = NULL) {
  info <- get_topenv_info()
  prm_df <- eval_effects(info$data, params$df, df, info$n)
  prm_ncp <- eval_effects(info$data, params$ncp, ncp, info$n)
  list(values = stats::rchisq(info$n, prm_df$input, prm_ncp$input),
       effects = list(shape1 = prm_df,
                      shape2 = prm_ncp))
}

sim_exponential <- function(rate, params = NULL) {
  info <- get_topenv_info()
  prm_rate <- eval_effects(info$data, params$rate, rate, info$n)
  list(values = stats::rexp(info$n, prm_rate$input),
       effects = list(rate = prm_rate))
}

sim_f <- function(df1, df2, ncp = NULL, params = NULL) {
  # supply NA for no ncp
  info <- get_topenv_info()
  prm_df1 <- eval_effects(info$data, params$df1, df1, info$n)
  prm_df2 <- eval_effects(info$data, params$df2, df2, info$n)
  prm_ncp <- eval_effects(info$data, params$ncp, ncp, info$n)
  if(is.null(ncp)) {
    values <- stats::rf(info$n, prm_df1$input, prm_df2$input)
  } else {
    values <- double(info$n)
    if(length(prm_ncp$input) > 1) {
      w <- is.na(prm_ncp$input)
      values[w] <- stats::rf(sum(w), prms_df1$input[w], prms_df2$input[w], prms_ncp$input[w])
      values[!w] <- stats::rf(sum(!w), prms_df1$input[!w], prms_df2$input[!w], prms_ncp$input[!w])
    } else {
      if(is.na(prm_ncp$input)) {
        values <- stats::rf(info$n, prm_df1$input, prm_df2$input)
      } else {
        values <- stats::rf(info$n, prm_df1$input, prm_df2$input, prm_ncp$input)
      }
    }
  }
  list(values = values,
       effects = list(df1 = prm_df1,
                      df2 = prm_df2,
                      ncp = prm_ncp))
}

sim_gamma <- function(shape, rate, params = NULL) {
  info <- get_topenv_info()
  prm_shape <- eval_effects(info$data, params$shape, shape, info$n)
  prm_rate <- eval_effects(info$data, params$rate, rate, info$n)
  list(values = stats::rgamma(info$n, prm_shape$input, prm_rate$input),
       effects = list(shape = prm_shape,
                      rate = prm_rate))
}

sim_hypergeometric <- function(m, n, k, params = NULL) {
  info <- get_topenv_info()
  prm_m <- eval_effects(info$data, params$m, m, info$n)
  prm_n <- eval_effects(info$data, params$n, n, info$n)
  prm_k <- eval_effects(info$data, params$k, k, info$n)
  list(values = stats::rhyper(info$n, prm_m$input, prm_n$input, prm_k$input),
       effects = list(m = prm_m,
                      n = prm_n,
                      k = prm_k))
}

sim_normal <- function(mean = 0, sd = 1, params = NULL) {
  info <- get_topenv_info()
  eff_mean <- eval_effects(info$data, params$mean, mean, info$n)
  eff_sd <- eval_effects(info$data, params$sd, sd, info$n)
  list(values = stats::rnorm(info$n, eff_mean$input, eff_sd$input),
       effects = list(mean = eff_mean,
                      sd = eff_sd))
}

sim_t <- function(df, mean = 0, sd = 1, ncp = NULL, params = NULL) {
  info <- get_topenv_info()
  prm_df <- eval_effects(info$data, params$df, df, info$n)
  prm_mean <- eval_effects(info$data, params$mean, mean, info$n)
  prm_sd <- eval_effects(info$data, params$sd, sd, info$n)
  prm_ncp <- eval_effects(info$data, params$ncp, ncp, info$n)
  if(is.null(ncp)) {
    values <- stats::rt(info$n, prm_df$input) * prm_sd$input + prm_mean$input
  } else {
    values <- stats::rt(info$n, prm_df$input, prm_ncp) * prm_sd$input + prm_mean$input
  }
  list(values = values,
       effects = list(df = prm_df,
                      mean = prm_mean,
                      sd = prm_sd,
                      ncp = prm_ncp))
}

sim_uniform <- function(min, max, params = NULL) {
  info <- get_topenv_info()
  prm_min <- eval_effects(info$data, params$min, min, info$n)
  prm_max <- eval_effects(info$data, params$max, max, info$n)
  list(values = stats::runif(info$n, prm_min$input, prm_max$input),
       effects = list(min = prm_min,
                      max = prm_max))
}

sim_weibull <- function(shape, scale = 1, params = NULL) {
  info <- get_topenv_info()
  prm_shape <- eval_effects(info$data, params$shape, shape, info$n)
  prm_scale <- eval_effects(info$data, params$scale, scale, info$n)
  list(values = stats::rweibull(info$n, prm_shape$input, prm_scale$input),
       effects = list(shape = prm_shape,
                      scale = prm_scale))
}

sim_bernoulli <- function(prob, params = NULL) {
  info <- get_topenv_info()
  prm_prob <- eval_effects(info$data, params$prob, prob, info$n)
  list(values = stats::rbinom(info$n, 1, prm_prob$input),
       effects = list(prob = prm_prob))
}

sim_binomial <- function(size, prob, params = NULL) {
  info <- get_topenv_info()
  prm_size <- eval_effects(info$data, params$size, size, info$n)
  prm_prob <- eval_effects(info$data, params$prob, prob, info$n)
  list(values = stats::rbinom(info$n, prm_size$input, prm_prob$input),
       effects = list(size = prm_size,
                      prob = prm_prob))
}

sim_geometric <- function(prob, params = NULL) {
  info <- get_topenv_info()
  prm_prob <- eval_effects(info$data, params$prob, prob, info$n)
  list(values = stats::rgeom(info$n, prm_prob$input),
       effects = list(prob = prm_prob))
}

sim_negative_binomial <- function(size, prob, params = NULL) {
  info <- get_topenv_info()
  prm_size <- eval_effects(info$data, params$size, size, info$n)
  prm_prob <- eval_effects(info$data, params$prob, prob, info$n)
  list(values = stats::rnbinom(info$n, prm_size$input, prm_prob$input),
       effects = list(size = prm_size,
                      prob = prm_prob))
}

sim_poisson <- function(lambda, params = NULL) {
  info <- get_topenv_info()
  prm_lambda <- eval_effects(info$data, params$lambda, lambda, info$n)
  list(values = stats::rpois(info$n, prm_lambda$input),
       effects = list(lambda = prm_lambda))
}


get_topenv_info <- function() {
  e <- caller_env(n = 2)$.top_env
  data <- e$data
  n <- nlevels(data[[e$var]])
  list(data = data,
       n = ifelse(n==0, nrow(data), n))
}



