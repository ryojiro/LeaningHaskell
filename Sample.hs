module Sample where
  fun 0 = 0
  fun n = n + fun(n-1)
