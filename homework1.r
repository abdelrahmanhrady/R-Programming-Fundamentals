# Problem 1

student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)


# Part A

section <- factor(section, levels = c("A", "B"))

students <- data.frame(
  student_id = student_id,
  section = section,
  quiz1 = quiz1,
  quiz2 = quiz2,
  passed = passed
)

score_matrix <- cbind(quiz1, quiz2)
rownames(score_matrix) <- student_id
colnames(score_matrix) <- c("quiz1", "quiz2")

course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)

typeof(section)
class(section)
length(section)

typeof(students)
class(students)
str(students)
dim(students)

typeof(score_matrix)
class(score_matrix)
str(score_matrix)
dim(score_matrix)

typeof(course_record)
class(course_record)
length(course_record)
str(course_record)

# A vector stores values of the same basic type, while a list can hold different kinds of objects.
# A matrix is 2D and all its values have the same type, while a factor represents categories.
# A data frame is also 2D, but its columns can have different types.


# Part B

score_matrix["S04", "quiz2"]

score_matrix[1:2, , drop = FALSE]

course_record["course"]
course_record[["course"]]
course_record$course

# [ returns a subset and usually keeps its structure.
# [[ pulls out one specific element, while $ pulls out an element by its name.


# Part C

students$average <- rowMeans(students[, c("quiz1", "quiz2")])

students$excellent <- students$average >= 90

section_A_80 <- students[
  students$section == "A" & students$average >= 80,
  c("student_id", "section", "average")
]

section_A_80

student_averages <- students$average
names(student_averages) <- students$student_id

student_averages

# These calculations are vectorized because they work on whole vectors or rows
# instead of going through each student with a loop.



# Problem 2

csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"


# Part A

measurements <- read.csv(
  text = csv_text,
  na.strings = c("", "NA")
)

head(measurements)
str(measurements)
dim(measurements)
names(measurements)

colSums(is.na(measurements))

measurements_complete <- measurements[complete.cases(measurements), ]

measurements_complete

removed_ids <- measurements$sample_id[!complete.cases(measurements)]

removed_ids

# x == NA does not work because comparing something to NA gives NA back.
# is.na(x) should be used to check if something is missing.


# Part B

measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)

levels(measurements$site)
levels(measurements$status)

measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

measurements$ph_below_7 <- measurements$ph < 7

measurements$ph_below_7

selected_measurements <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
  c("sample_id", "site", "temp_c", "temp_f", "ph")
]

selected_measurements

overall_mean <- mean(measurements$temp_c, na.rm = TRUE)
overall_mean

south_mean <- mean(
  measurements$temp_c[measurements$site == "South"],
  na.rm = TRUE
)

south_mean


# Part C

A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

element_product <- A * B
element_product
dim(element_product)

matrix_product <- A %*% B
matrix_product
dim(matrix_product)

# * multiplies values in the same positions in the matrices.
# %*% does matrix multiplication using rows from A and columns from B.



# Problem 3

student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)


# Part A

grade_one <- function(
  score,
  a_min = 90,
  b_min = 80,
  c_min = 70,
  d_min = 60
) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)


# Part B

grades <- rep(NA_character_, length(scores))

for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}

names(grades) <- student_id

grades

# i is the position of the score that the loop is currently working with.


# Part C-1

summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  total_count <- length(x)
  missing_count <- sum(is.na(x))

  mean_score <- mean(x, na.rm = na.rm)
  sd_score <- sd(x, na.rm = na.rm)
  min_score <- min(x, na.rm = na.rm)
  max_score <- max(x, na.rm = na.rm)

  c(
    total = total_count,
    missing = missing_count,
    mean = round(mean_score, digits),
    sd = round(sd_score, digits),
    min = round(min_score, digits),
    max = round(max_score, digits)
  )
}

summarize_scores(scores)

summarize_scores(
  x = scores,
  na.rm = TRUE,
  digits = 2
)


# Part C-2

plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(
  scores,
  type = "b",
  pch = 19,
  xlab = "Position",
  ylab = "Score",
  main = "Student Scores"
)

# The function plots each score against its position in the vector.
# The ... passes the extra plot options into plot().
# Since position 3 is NA, there will not be a point there.