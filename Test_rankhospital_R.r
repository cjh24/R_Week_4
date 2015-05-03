source("rankhospital.R")

rankhospital("TX", "heart failure", 4)
# "DETAR HOSPITAL NAVARRO"

rankhospital("MD", "heart attack", "worst")
# "HARFORD MEMORIAL HOSPITAL"

rankhospital("MN", "heart attack", 5000)
# NA