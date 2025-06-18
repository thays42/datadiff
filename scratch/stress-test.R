x <- bind_cols(
  set_names(example_timeseries, paste0(names(example_timeseries), ".a")),
  set_names(example_timeseries, paste0(names(example_timeseries), ".b")),
  set_names(example_timeseries, paste0(names(example_timeseries), ".c")),
  set_names(example_timeseries, paste0(names(example_timeseries), ".d")),
  set_names(example_timeseries, paste0(names(example_timeseries), ".e"))
)
x <- x[rep(seq_along(x), times = 10000), ]

y <- x
y$value1.a[33] <- -999
y$date.e[24] <- as.Date("2000-01-01")


t <- diffdata(x, y)
p <- show_diff(t)

render_diff(t)


a <- example_types
b <- a[c(3, 3, 2, 4, 4, 4, 1), ]
diffdata(a, b)

x <- bind_cols(
  set_names(example_timeseries, paste0(names(example_timeseries), ".a")),
  set_names(example_timeseries, paste0(names(example_timeseries), ".b")),
  set_names(example_timeseries, paste0(names(example_timeseries), ".c")),
  set_names(example_timeseries, paste0(names(example_timeseries), ".d")),
  set_names(example_timeseries, paste0(names(example_timeseries), ".e"))
)
x <- x[rep(seq_along(x), times = 10), ]

y <- x
y$value1.a[33] <- -999
y$date.e[24] <- as.Date("2000-01-01")

diffdata(x, y, context_cols = ends_with(".b"))
