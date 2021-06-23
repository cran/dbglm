[![Build Status](https://travis-ci.com/caoalbert/dbglm.svg?branch=master)](https://travis-ci.com/caoalbert/dbglm)
[![R-CMD-check](https://github.com/caoalbert/dbglm/workflows/R-CMD-check/badge.svg)](https://github.com/caoalbert/dbglm/actions)

This package fits generalised linear models to moderately large data sets stored in a relational database. The code has implementations for MonetDB, SQLite, and duckDB, but should be easy to adapt to any other database that has EXP and RAND. The package can also be compatible with Google big query, however, downloading data seems to be automatically required. 

The code takes a subsample of the data, fits the model in memory, then improves the estimate with one step of Fisher scoring computed with a single SQL aggregation query. In addition, the package allows users to conduct glm regression with large datasets that could not be processed by the function glm due to RAM usage limit. 

An example of using duckDB as the backend for a local file: 

```{r}
# Establish the connection 
con_duck<- dbConnect(duckdb::duckdb()) 
# Read in the local dataset 
duckdb_read_csv(con_duck, "Fleet30Nov2017a.csv", "Fleet30Nov2017a.csv", quote = "", lower.case.names=TRUE, check.names = T) 
# Using duckDB as a database 
cars<- dbReadTable(con_duck, "Fleet30Nov2017a.csv") 
# Using dbglm 
model<-dbglm(isred~power_rating+number_of_seats+gross_vehicle_mass,tbl=cars) 
```
An example of using an existing dataframe in the environment:

```{r}
duckdb::duckdb_register(con, "fleet", fleet1)
```
