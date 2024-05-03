#8.machine learning - 6.3 Matrix factorization comp check

# In this exercise, we will see one of the ways that this decomposition can be useful.
# To do this, we will construct a dataset that represents grade scores for 100 students
# in 24 different subjects. The overall average has been removed so this data represents
# the percentage point each student received above or below the average test score. 
# So a 0 represents an average grade (C), a 25 is a high grade (A+), and a -25 represents
# a low grade (F). You can simulate the data like this:
set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

# Our goal is to describe the student performances as succinctly as possible. For
# example, we want to know if these test results are all just a random independent
# numbers. Are all students just about as good? Does being good in one subject  
# imply you will be good in another? How does the SVD help with all this? We will
# go step by step to show that with just three relatively small pairs of vectors 
# we can explain much of the variability in this  dataset. 

#Q1 visualize the 24 test score for the 100 students by plotting an image:
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)
#the students who test well are at the top of the image and there seems to be three groupings by subject

#Q2 You can examine the correlation between the test scores directly like this: 
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
#there is correlation amoung all tests, but higher if the tests are in science and math and even higher within each subject

#Q3 Use svd() to compute the SVD of y.  This function will return U, V, and diagonal entries of D.
s <- svd(y)
names(s)

#check that SVD is working:
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y-y_svd))

#Compute the sum of squares of the columns of Y and store them in ss_y. Then compute
# the sum of squares of columns of the transformed YV and store them in ss_yv. Confirm
# that sum(ss_y) is equal to sum(ss_yv).

y_sq <- y*y
ss_y <-colSums(y_sq)
sum(ss_y)


y_svd_sq <- (y%*%s$v)*(y%*%s$v)
ss_yv <- colSums(y_svd_sq)
sum(ss_yv)


#Q4 We see that the total sum of squares is preserved. This is because  is orthogonal.
# Now to start understanding how  is useful, plot ss_y against the column number 
# and then do the same for ss_yv.
plot(ss_y)
plot(ss_yv)

#Q5 Now notice that we didn't have to compute ss_yv because we already have the answer.
#How? Remember that  and because  is orthogonal, we know that the sum of squares 
#of the columns of  are the diagonal entries of  squared. Confirm this by plotting 
#the square root of ss_yv versus the diagonal entries of D.
plot(sqrt(ss_yv),s$d)

#Q6 from the above we know that the sum of squares of the columns of Y (the total
# sum of squares) adds up to the sum of s$d^2 and that the transformation YV gives 
# us columns with sums of squares equal to s$d^2. Now compute the percent of the 
# total variability that is explained by just the first three columns of YV .
sum(s$d[1:3]^2)/sum(s$d^2)

#now we know we can explain almost all of the variability with just 3 columns of data

#Q7 compute UD without constructing diag(s$d) or using matrix mutiplication
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

#Q8 We know thatthe first column of UD has the most variability of all the columns
# of UD. Earlier we looked at an image of  using my_image(y), in which we saw that
# the student to student variability is quite large and that students that are good
# in one subject tend to be good in all. This implies that the average (across all 
#subjects) for each student should explain a lot of the variability. Compute the 
# average score for each student, plot it against U1D1 , and describe what you find.

plot(-s$u[,1]*s$d[1], rowMeans(y))

#Q9 Make an image plot of V  and describe the first column relative to others and 
# how this relates to taking an average.
my_image(s$v)
#the first column is close to being a constant, which implies that the first column
#of YV is the sum of the rows of Y multiplied by some constant, and is thus proportional to an average.

