
# 1. Connect to Ingenuity database
library(DBI)
#library(config)

conn_args <- config::get("dataconnection")
myconn <- dbConnect(odbc::odbc(),
                    Driver = conn_args$driver,
                    Server = conn_args$server,
                    UID    = conn_args$uid,
                    PWD    = conn_args$pwd,
                    Port   = conn_args$port,
                    Database = conn_args$database
)


