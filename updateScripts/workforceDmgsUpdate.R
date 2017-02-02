#!/usr/bin/Rscript

####################################################################################################

# This script updates data for Workforce Demographics Dashboard

####################################################################################################

suppressMessages(library(atomisc))      # teradataConnect()
suppressMessages(library(readr))        # fast I/O

# read SQL to extract data
sql <- paste(read_lines("workforceDmgsUpdate.sql"), collapse="\n")

# submit query to dwh
con     <- teradataConnect()
wddDmgs <- dbGetQuery(con, sql)
x       <- dbDisconnect(con)

# print output
cat(format_csv(wddDmgs))