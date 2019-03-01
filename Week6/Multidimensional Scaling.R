# Letter Recognition and Confusion. The table below is from Wolford and Hollingsworth (1974), 
# where each entry shows the frequency with which each letter was mistakenly called something else.
D = matrix(0, 8, 8)
letters = c("C", "D", "G", "H", "M", "N", "Q", "W")
colnames(D) = letters
rownames(D) = letters
D[2:8, 1] = c(5, 12, 2, 2, 2, 9, 1)
D[3:8, 2] = c(2, 4, 3, 4, 20, 5)
D[4:8, 3] = c(3, 2, 1, 9, 2)
D[5:8, 4] = c(19, 18, 1, 5)
D[6:8, 5] = c(16, 2, 18)
D[7:8, 6] = c(8, 13)
D[8, 7] = 4
D = (D+t(D))
D

# Change the similarity matrix to distance matrix.

D0 = 21 - D
diag(D0) = 0
tmp = cmdscale(D0)
par(mfrow=c(1,2))
plot(tmp[, 1], tmp[, 2], type="n", xlab="", ylab="", 
     xlim = c(-15, 15), ylim=c(-15, 15))
text(tmp[, 1], tmp[, 2], label = letters)

D1 = 41 - D
diag(D1) = 0
tmp = cmdscale(D1)
plot(tmp[, 1], tmp[, 2], type="n", xlab="", ylab="", 
     xlim = c(-20, 20), ylim=c(-20, 20))
text(tmp[, 1], tmp[, 2], label = letters)
