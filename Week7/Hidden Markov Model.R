library(HMM)
dishonestCasino()

# A Simulated Example
# Specify the following HMM.

A=matrix(c(0.95,0.05,0.05,0.95),2,2);
B=matrix(c(1/3, 0.1, 1/3, 0.2, 1/3,0.7),2,3);
hmm=initHMM(c("A", "B"), c(1, 2, 3), 
            startProbs=c(1/2, 1/2),
            transProbs=A, emissionProbs=B)
print(hmm)

# Generate n=500 obs from this HMM.

n=500;
data=simHMM(hmm, n)
names(data)
cbind(data$states[1:5], data$observation[1:5])

## Fit the data with two hidden states --------------
# Use BaumWelch (i.e., EM) algorithm to estimate the model parameters.
tmphmm=initHMM(c("A", "B"), c(1, 2, 3))
myfit2 = baumWelch(hmm, data$obs)
names(myfit2)

print(myfit2$hmm)
# Compute the (marginal) conditional distribution of the hidden state for each 
# observation: P(Z_i = k | X_1, ..., X_n). Then classify Z_i to be the most probable 
# state.
mypost = posterior(myfit2$hmm, data$obs)
# Use the Viterbi algorithm to compute the most probable hidden sequence
vit.out = viterbi(myfit2$hmm, data$obs)
# display the result
# plot the data
plot(data$obs, ylim = c(-6, 3), pch = 3, 
     xlab = "", ylab = "", bty = "n", yaxt = "n")
axis(2, at = 1:3)

# display true hidden states
text(0, -1.2, adj = 0, cex = 0.8, col = "black", "True: green = A")
for (i in 1:n) {
  if (data$states[i] == "A")
    rect(i, -1, i + 1, 0, col = "green", border = NA)
  else rect(i, -1, i + 1, 0, col = "red", border = NA)
}

# display the post probable path
text(0, -3.2, adj = 0, cex = 0.8, col = "black", "Most probable path")
for (i in 1:n) {
  if (vit.out[i] == "A")
    rect(i, -3, i + 1, -2, col = "green", border = NA)
  else rect(i, -3, i + 1, -2, col = "red", border = NA)
}

# display the marginal most probable state
text(0, -5.2, adj = 0, cex = 0.8, col = "black", "Marginal most probable state")
for (i in 1:n) {
  if (mypost[1,i] > 0.5)
    rect(i, -5, i + 1, -4, col = "green", border = NA)
  else rect(i, -5, i + 1, -4, col = "red", border = NA)
}

## Fit the data with three hidden states --------------
# The number of hidden states is a tuning parameter. We can try a range of hidden 
# states and then use AIC/BIC to select K, the number of hidden states.
tmphmm=initHMM(c("A", "B", "C"), c(1, 2, 3))
myfit3 = baumWelch(tmphmm, data$obs)
print(myfit3$hmm)

# Note that the likelihood of the data (integrated over all possible hidden sequences) 
# could be computed using the forward command.
# log-likelihood for K=2
f=forward(myfit2$hmm, data$obs) # f: 2xn
A = f[1,n]; B = f[2,n]
A + log(1 + exp(B-A))

# log-likelihood for K=3
f=forward(myfit3$hmm, data$obs) # f: 3xn
A = f[1,n]; B = f[-1,n]
A + log(1 + sum(exp(B-A)))
