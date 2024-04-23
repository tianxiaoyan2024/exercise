# --------------------------------------------
# Script Name: r_database.R
# Purpose: This scribes how to to upload the data of Doubs, a built-in dataset
#          of ade4 package into a schema of PostgreSQL or the SQLite.
#          .

# Author:     Xiaoyan Tian
# Email:      xiaoyantian@mail.ustc.edu.cn
# Date:       2024-04-10
#第一版，还未精进，需修改！
# --------------------------------------------

cat("\014") #clears rhe console
rm(list=ls()) #remove all variales# Load required libraries

library(reticulate)
library(DBI)

# Choose the database backend (SQLite or PostgreSQL)
# For SQLite, use RSQLite
# For PostgreSQL, use RPostgreSQL
# Install the respective package if not already installed
# install.packages("RSQLite")
# install.packages("RPostgreSQL")

# Load the Doubs dataset from the ade4 package
library(ade4)
data("doubs")

# For SQLite
library(RSQLite)
db_conn <- dbConnect(SQLite(), dbname = "mydatabase.sqlite")

dbWriteTable(db_conn, 'fish', doubs$fish)
dbWriteTable(db_conn, 'species', doubs$species)
dbWriteTable(db_conn, 'env', doubs$env)
dbWriteTable(db_conn, 'xy', doubs$xy)

# List the tables
dbListTables(db_conn)

# Close the database connection
dbDisconnect(db_conn)

# For PostgreSQL
library(RPostgreSQL)
library(RPostgres)
db_conn <- dbConnect(RPostgres::Postgres(), 
                     host = "localhost", 
                     port = 5432, 
                     user = "xytian", 
                     password = "****", 
                     dbname = "test_doubs")

# Check the connection
dbIsValid(db_conn)

# Load the Doubs dataset from the ade4 package
library(ade4)
data("doubs")

# Convert the Doubs dataset to a data frame
doubs_df <- as.data.frame(doubs)

# Upload the data frame to the database
dbWriteTable(db_conn, "doubs_table", doubs_df, overwrite = TRUE)

# Close the database connection
dbDisconnect(db_conn)

# Output success message
cat("Data uploaded successfully!\n")
