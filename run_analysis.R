library(plyr)

merge.datasets = function() {
    train.x <- read.table("data/UCI HAR Dataset/train/X_train.txt")
    train.y <- read.table("data/UCI HAR Dataset/train/y_train.txt")
    train.subject <- read.table("data/UCI HAR Dataset/train/subject_train.txt")

    test.x <- read.table("data/UCI HAR Dataset/test/X_test.txt")
    test.y <- read.table("data/UCI HAR Dataset/test/y_test.txt")
    test.subject <- read.table("data/UCI HAR Dataset/test/subject_test.txt")

    all_datas.x <- rbind(train.x, test.x)
    all_datas.y <- rbind(train.y, test.y)
    all_datas.subject <- rbind(train.subject, test.subject)
    list(x=all_datas.x, y=all_datas.y, subject=all_datas.subject)
}

extract.mean.and.std = function(df) {
    features <- read.table("data/UCI HAR Dataset/features.txt")
    mean.col <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
    std.col <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))
    edf <- df[, (mean.col | std.col)]
    colnames(edf) <- features[(mean.col | std.col), 2]
    edf
}

name.activities = function(df) {
    colnames(df) <- "activity"
    df$activity[df$activity == 1] = "WALKING"
    df$activity[df$activity == 2] = "WALKING_UPSTAIRS"
    df$activity[df$activity == 3] = "WALKING_DOWNSTAIRS"
    df$activity[df$activity == 4] = "SITTING"
    df$activity[df$activity == 5] = "STANDING"
    df$activity[df$activity == 6] = "LAYING"
    df
}

bind.data <- function(x, y, subjects) {
    cbind(x, y, subjects)
}

create.tidy.dataset = function(df) {
    tidy <- ddply(df, .(subject, activity), function(x) colMeans(x[,1:60]))
    tidy
}

clean.data = function() {
    all_datas <- merge.datasets()
    cx <- extract.mean.and.std(all_datas$x)
    cy <- name.activities(all_datas$y)
    colnames(all_datas$subject) <- c("subject")
    combined <- bind.data(cx, cy, all_datas$subject)
    tidy <- create.tidy.dataset(combined)
    write.csv(tidy, "tidy.txt", row.names=FALSE)
}

clean.data()