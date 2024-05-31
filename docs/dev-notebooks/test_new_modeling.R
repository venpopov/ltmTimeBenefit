tar_load(c(exp1_data_agg, exp2_data_agg, exp3_data_agg))

p1 <- start_fun2(pars = c("prop", "prop_ltm", "rate", "tau", "gain")) 
estimate_model(p1, exp1_data_agg, exclude_sp1 = TRUE, iter = 1)

p2 <- start_fun2(pars = c("prop", "prop_ltm", "rate", "tau"))
estimate_model(p2, exp2_data_agg, exclude_sp1 = TRUE, fixed_params = NULL, method = "L-", iter = 2)

estimate_model(
  start_fun2(
    pars = c("prop", "prop_ltm", "tau", "rate", "gain", "prop_prim"),
    growth = "asy"
  ),
  exp3_data_agg,
  exclude_sp1 = TRUE, growth = "asy", method = "L-",
  iter = 2
)

estimate_model(
  start_fun2(
    pars = c("prop", "prop_ltm", "tau", "rate", "gain", "prop_prim"),
    growth = "asy"
  ),
  data = exp3_data_agg,
  exclude_sp1 = TRUE, growth = "asy", method = "L-",
  control = list(maxit = 1e6),
  iter = 2
)

fit <- estimate_model(
  start_fun2(
    pars = c("prop", "prop_ltm", "tau", "rate", "gain", "prop_prim"),
    growth = "asy"
  ),
  data = exp3_data_agg,
  exclude_sp1 = TRUE,
  fixed_params = c(growth = "asy"),
  iter = 20
)


exp3_data_agg$pred <- predict(fit)

exp3_data_agg |>
  ggplot(aes(x = gap, y = p_correct, color = chunk)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  geom_line(aes(y = pred), linetype = "dashed", linewidth = 1.1) +
  scale_color_discrete("First chunk LTM?") +
  facet_wrap(~itemtype)


fit1 <- estimate_model(
  start_fun2(
    pars = c("prop", "prop_ltm", "tau", "rate", "gain", "prop_prim"),
    growth = "asy"
  ),
  data = exp3_data_agg,
  exclude_sp1 = TRUE,
  fixed_params = c(growth = "asy"),
  iter = 20
)

fit2 <- estimate_model(
  start_fun2(
    pars = c("prop", "prop_ltm", "tau", "rate", "gain"),
    growth = "asy"
  ),
  data = exp3_data_agg,
  exclude_sp1 = TRUE,
  fixed_params = c(growth = "asy"),
  iter = 20
)

fit3 <- estimate_model(
  start_fun2(pars = c("prop", "prop_ltm", "tau", "rate", "gain")),
  data = exp3_data_agg,
  exclude_sp1 = TRUE,
  fixed_params = c(growth = "linear"),
  iter = 20
)

fit4 <- estimate_model(
  start_fun2(pars = c("prop", "prop_ltm", "tau", "rate", "gain", "prop_prim")),
  data = exp3_data_agg,
  exclude_sp1 = TRUE,
  fixed_params = c(growth = "linear"),
  iter = 20
)


exp3_data_agg$pred4 <- predict(fit4)

exp3_data_agg |>
  ggplot(aes(x = gap, y = p_correct, color = chunk)) +
  geom_line(aes(y = pred4), linetype = "dashed", linewidth = 1.1) +
  scale_color_discrete("First chunk LTM?") +
  facet_wrap(~itemtype)


exp3_data_agg$str4 <- predict(fit4, type = "strength")

exp3_data_agg |>
  ggplot(aes(x = gap, y = p_correct, color = chunk)) +
  geom_line(aes(y = str4), linetype = "dashed", linewidth = 1.1) +
  scale_color_discrete("First chunk LTM?") +
  facet_wrap(~itemtype)
