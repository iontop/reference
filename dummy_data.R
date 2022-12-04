start <- as.POSIXct("2022-09-01")
interval <- 60 #sec
end <- start + as.difftime(1, units = "days") # 1day
time <- seq(start, end, by = interval)

dwatt <- runif(length(time), min = 700, max = 1100) %>% sort()
p_ip <- runif(length(time), min = 100, max = 250) %>% sort()
t_ip <- runif(length(time), min = 500, max = 610) %>% sort()
hrh_p <- runif(length(time), min = 35, max = 50) %>% sort()
hrh_t <- runif(length(time), min = 550, max = 620) %>% sort()
ssh_p <- runif(length(time), min = 0.20, max = 0.30)

df <- data.frame(time, dwatt, p_ip, t_ip, hrh_p, hrh_t, ssh_p, row.names = NULL)
head(df)
tail(df)

write.csv(df, file = "dummy.csv",row.names = FALSE, fileEncoding = "UTF-8")