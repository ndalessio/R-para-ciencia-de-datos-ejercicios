# Itireration 

library(tidyverse)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# Funciton to calculate the median of each column

output <- vector("double", ncol(df)) # 0 0 0 0 

for (i in seq_along(df)) {
  print(i)
  output[[i]] <- median(df[[i]])
}

#Exercises 21.2.1
# Write for loops to:

# 1. Compute the mean of every column in mtcars.

means_df <- function(df){
  output <- vector("double", ncol(df))
  for (i in seq_along(df)){
    output[[i]] <- mean(df[[i]])
  }
  return(output)
}

# 2. Determine the type of each column in nycflights13::flights.

type_ncol <- function(df){
  output <- vector("character", ncol(df))
  for (i in seq_along(df)){
    output[[i]] <- typeof(df[[i]])
  }
  return(output)
}

# 3. Compute the number of unique values in each column of iris.

unique_values <- function(df){
  output <- vector("double", ncol(df))
  for (i in seq_along(df)){
    output[[i]] <- length(unique(df[[i]]))
  }
  return(output)
}

# sapply(mtcars, function(x) length(unique(x)))



# For loop variations:
#1. Modifying an existing object

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale01 <- function(x){
  rng <- range(x, na.rm = TRUE)
  (x - rng[1] / rng[2] - rng[1])
}

for (i in seq_along(df)){
  df[[i]] <- rescale01(df[[i]])
}



# Loop variations

# 21.3.5 Exercises

#1. Imagine you have a directory full of CSV files that you want to read in. You have their paths in a vector, files <- dir("data/", pattern = "\\.csv$", full.names = TRUE), and now want to read each one with read_csv(). Write the for loop that will load them into a single data frame.

files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)

#since we now the number of files we got:
df_list <- vector("list", length(files))

for(i in seq_along(files)){
  df_list[[i]] <- read_csv(files[[i]])
}

# to combine the list of data frames in one data frame:

df <- bind_rows(df_list)


#2. What happens if you use for (nm in names(x)) and x has no names? What if only some of the elements are named? What if the names are not unique?

# If there are no names, no iterations.
# If there is a name missing, would give an error
# If there are duplicated names, will give the first one. 

#Example of the third case:

x <- c(a = 11, a = 12, c = 13)
names(x)

for(nm in names(x)){
  print(nm)
  print(x[[nm]])
}


#3. Write a function that prints the mean of each numeric column in a data frame, along with its name. 

show_mean <- function(df, digits = 2){
  maxstr <- max(str_length(names(df)))
  
  for(nm in names(df)){
    if(is.numeric(df[[nm]])){
      cat(
        str_c(str_pad(str_c(nm, ":"), maxstr + 1L, side = "right"),
              format(mean(df[[nm]]), digits = digits, nsmall = digits),
              sep = " "
        ), 
        "\n"
      )
    }
  }
}

show_mean(iris)

#4. What does this code do? How does it work?

trans <- list(
  disp = function(x) x * 0.0163871, 
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)

for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}

# The code mmutates two variables. disp and am.
# var in names trans = disp and am. so in this case the loop would go for those variables
# mtcars[[disp]] <- trans[[disp]](mtcars[[disp]])  tras[[disp]]( ) this is a function
# applied to mrcars[[disp]] which is the column


# For loops vs. functionals

# The map function
#Exercise 21.5.1
# 1. Write code that uses one of the map functions to:

#Compute the mean of every column in mtcars.
map_dbl(mtcars, mean)

#Determine the type of each column in nycflights13::flights.
map_chr(nycflights13::flights, typeof)

#Compute the number of unique values in each column of iris.
map_int(iris, n_distinct) 
#also map_int(function(x), length(unique(x))) or map_int(iris, ~length(unique(.x)))

#Generate 10 random normals for each of  
map(c(-10, 0, 10, 100), ~rnorm(n = 10, mean = .))

#2.How can you create a single vector that for each column in a data frame indicates whether or not it’s a factor?

map_lgl(diamonds, is.factor)

#3. What happens when you use the map functions on vectors that aren’t lists? What does map(1:5, runif) do? Why?

map(1:5, runif)

# the output is always a list. The map function loops between 1:5. Previous expression is equivalent to this:
list(
  runif(1),
  runif(2),
  runif(3),
  runif(4),
  runif(5)
)

#4. What does map(-2:2, rnorm, n = 5) do? Why? What does map_dbl(-2:2, rnorm, n = 5) do? Why?

map(-2:2, rnorm, n = 5)
# This expression takes samples of size five from five normal distributions, with means of (-2, -1, 0, 1, and 2), but the same standard deviation (1). It returns a list with each element a numeric vectors of length 5.

map_dbl(-2:2, rnorm, n = 5)
# Raises an error. This is because the map_dbl() function requires the function it applies to each element to return a numeric vector of length one. If the function returns either a non-numeric vector or a numeric vector with a length greater than one, map_dbl() will raise an error. The reason for this strictness is that map_dbl() guarantees that it will return a numeric vector of the same length as its input vector.

#To return a numeric vector, use flatten_dbl() to coerce the list returned by map() to a numeric vector.

map(-2:2, rnorm, n = 5) %>% 
  flatten_dbl()

#5. Rewrite map(x, function(df) lm(mpg ~ wt, data = df)) to eliminate the anonymous function.

x <- split(mtcars, mtcars$cyl)
map(x, function(df) lm(mpg ~ wt, data = df))

# we can eliminate the anonymus func

map(x, ~ lm(mpg ~ wt, data = .))

#another way to eliminate an anonymous func is to create a named one
run_reg <- function(df) {
  lm(mpg ~ wt, data = df)
}
map(x, run_reg)











