wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = F)
# remove id column
wbcd <- wbcd[,-1]
# code labels
wbcd$diagnosis <- factor(wbcd$diagnosis, levels=c("B", "M"), labels = c("Benign", "Malignant"))

# normalization
wbcd_n <- as.data.frame(lapply(wbcd[2:31], minmaxnorm))

# seperate training and test set
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

