# 
# wt = 1 / rep(3, 3)
# cov(df_i)
# 
# sqrt(t(wt) %*% cov(df_i) %*% wt)
# 
# 
# Wa = 0.5
# Ra = 0.0625
# Sa = 0.175
# 
# Wb = 0.500
# Rb = 0.040
# Sb = 0.045
# 
# Pab = 0.2
# 
# Vp = Wa^2 * Sa^2 + Wb^2 * Sb^2 + 2 * Wa * Wb * Sa * Sb * Pab # 0.00895
# 
# (Wa^2 * Sa^2 + Wa * Wb * Sa * Sb * Vab) / Vp # 0.00805 / 0.00895 = 0.8994413
# (Wb^2 * Sb^2 + Wa * Wb * Sa * Sb * Vab) / Vp # 0.0009  / 0.00895 = 0.1005587
# 
# (Ra-0.0175) / Va
# (Rb-0.0175) / Vb
# 
# z <- data.table(expand.grid(a, b))
# z[Var1 >= 0 & Var2 >= 0 & Var3 >= 0]
# z[, Var3 == 1 - Var1 - Var2]
# z[Var1 >= 0 & Var2 >= 0 & Var3 >= 0]
# 
# a <- b <- c <- seq(0, 1, 0.01)
# z <- data.table(expand.grid(a, b, c))
# setnames(z, c("a", "b", "c"))
# z <- z[a + b + c == 1]
# 
# z <- data.table(expand.grid(a, b))
# setnames(z, c("a", "b"))
# z <- z[a + b == 1]
# 
# eps <- 2e-2
# 
# rc_lst <- vector(mode = "list", length = nrow(z))
# Vp <- cov(df_i)
# for (i in 1:nrow(z)) {
#   Wp <- unlist(z[i, ])
#   Wp <- c
#   rc <- as.numeric(diag(Wp) %*% Vp %*% Wp) / as.numeric(t(Wp) %*% Vp %*% Wp)
#   rc_lst[i] <- sd(rc)
#   cat(i, ":", rc, "\n")
# }
# 
# which(unlist(rc_lst) == min(unlist(rc_lst)))
# Wp = unlist(z[75])
# z[1596]