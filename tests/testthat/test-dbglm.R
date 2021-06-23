test_that("dbglm works", {
  library(purrr)
  library(duckdb)

  data("fleet1")

  con_duck<- dbConnect(duckdb::duckdb())
  duckdb::duckdb_register(con_duck, df=fleet1, name = "cars1")
  cars<- dbReadTable(con_duck, "cars1")
  cars1 <- filter(cars, vehicle_type == "PASSENGER CAR/VAN") %>%
    mutate(isred=ifelse(basic_colour=="RED",1,0)) %>%
    filter(number_of_seats >1 & number_of_seats < 7) %>% filter(number_of_axles==2) %>%
    compute()
  set.seed(1)
  model<-dbglm(isred~power_rating+number_of_seats+gross_vehicle_mass,tbl=cars1)
  num<- round(as.numeric(unname(model$tildebeta[1])),3)
  expect_equal(num, -63.736)
})
