library(arules)
library(Matrix)

# Load your dataset or create a sample dataset
data2 <- read.csv("cleaned_dataset.csv")

# drop non-nominal attributes
data2$service.fee <- NULL
data2$price <- NULL
data2$Construction.year <- NULL



data2$instant_bookable <- ifelse(data2$instant_bookable == 0, "not_bookable", "bookable")

data2$host_identity_verified <- paste(data2$host_identity_verified, "identity", sep = " ")

data2$cancellation_policy <- paste(data2$cancellation_policy, "policy", sep = " ")


selected_attributes <- c("instant_bookable", "host_identity_verified",
                         "cancellation_policy", "room.type", "neighbourhood")

# Create transactions
for (col in selected_attributes) {
  data2[[col]] <- factor(data2[[col]], levels = unique(data2[[col]]))
}


# Create transactions
transactions <- as(data2[, selected_attributes], "transactions")

# Apply FP-growth algorithm
frequent_patterns <- eclat(transactions, parameter = list(supp = 0.1))

# Explore results
summary(frequent_patterns)


