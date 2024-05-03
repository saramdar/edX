#8.machine learning - 6.3 Matrix factorization

### Matrix Factorization
library(dslabs)
library(tidyverse)
library(caret)
data("movielens")

#generate training data
train_small <- movielens %>% 
     group_by(movieId) %>%
     filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% #keep movies with more than 50 unique ratings or has movieID 3253
     group_by(userId) %>%
     filter(n() >= 50) %>% ungroup() #keep userIDs with more than 50 unique ratings

y <- train_small %>% 
     dplyr::select(userId, movieId, rating) %>%
     pivot_wider(names_from = "movieId", values_from = "rating") %>%
     as.matrix()

#add row names and column names
rownames(y)<- y[,1]
y <- y[,-1]

movie_titles <- movielens %>% 
  dplyr::select(movieId, title) %>%
  distinct()

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])
#remove column and row averages
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE)) #summary stats which needs to swept away, rowMeans removed
y <- sweep(y, 2, colMeans(y, na.rm=TRUE)) #summary stats which needs to swept away, colMeans removed

#Now y is a matrix of residuals
library(gridExtra)

#plot residuals and compare
m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
p1 <- qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
p2 <- qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
p3 <- qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

grid.arrange(p1, p2 ,p3, ncol = 3)

cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") %>% 
     knitr::kable()

#factor analysis

set.seed(1988)
options(digits = 2)
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)

X <- jitter(P%*%t(Q))
X <- round(X, 1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(aling="c")

P

set.seed(1988)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(aling="c")

P

six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")

#SVD (singular value decomposition) and PCA (principal component analysis)
#first ensure there are no NAs in the data set.  Turn them into zeros
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y) #prcomp performs pca on the given matrix
dim(pca$rotation) #vectors q are the principal components, stored in this matrix:
dim(pca$x) #vectors p are the user effects, stored in this matrix:
plot(pca$sdev) #prcomp returns a component with the variability of each of the principal components, access it like this and plot it:
var_explained <-cumsum(pca$sdev^2/sum(pca$sdev^2)) #a large percent of the data is explained with just a few principal components
plot(var_explained) # with 50 princomps, about half of the variability is explained
#total of about 300 princomps


library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10) #PC1 has critically acclaimed movies on one side, blockbusters on the other

pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10) #PC2 separates artsy vs nerd favorites

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)

