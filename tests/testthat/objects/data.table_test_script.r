# data.table functions test
library(data.table)

dt <- as.data.table(cbind(x = 1:4, y = 1:2))
tt <- dt[, .(sumtst = sum(x)), by = y]

fwrite(tt, paste0(test_path(), '/objects/dt_test_success.csv'))
