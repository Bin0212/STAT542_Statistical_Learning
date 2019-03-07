library(Information)

# Basic WOE and IV -- no external cross validation
# Set ncore=2 since CRAN does now allow more than 2 for examples
# For real applications, leave ncore is NULL to get the default which is: number of cores - 1
data(train, package="Information")
train <- subset(train, TREATMENT==1)
IV <- create_infotables(data=train, y="PURCHASE", ncore=2)

# Show the first records of the IV summary table, thresh(rule of thumb) = 0.05
print(head(IV$Summary), row.names=FALSE)

# Show the WOE table for the variable called N_OPEN_REV_ACTS
print(IV$Tables$N_OPEN_REV_ACTS, row.names=FALSE)