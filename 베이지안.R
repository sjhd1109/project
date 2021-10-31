setwd('E:/대학원/Business Analytics/2021_1학기/베이지안 연구방법론/final')
#train_df_sim <- read.table('train.csv', sep=',', header =T)
#test_df_sim <- read.table('test.csv', sep=',', header =T)
data <- read.table('final_data.csv', sep=',', header =T)
#data = rbind(train_df_sim,test_df_sim)

#budo = data$budo
#data = cbind(budo, data[,-c(11,11)])
summary(data)
library(rjags)
install.packages("magrittr")
library(magrittr)

X = scale(data[, -1], center = T, scale = T)

mod_string = " model {
  for (i in 1:length(y)) {
    y[i] ~ dbern(p[i])
    logit(p[i]) = b0 + b[1]*KK043[i] + b[2]*KK106[i] + b[3]*KK039[i] + b[4]*KK018[i] + b[5]*KK015[i] + b[6]*KK057[i]
    + b[7]*KK060[i] + b[8]*KK030[i] + b[9]*KK025[i] + b[10]*KK081[i]
  }

  b0 ~ dnorm(0.0, 1.0/25.0)

  for (j in 1:10) {
    b[j] ~ ddexp(0.0, sqrt(2.0)) # has variance 1.0
  }
} "

data_jags = list(y = data$budo, 
                 
                 KK043 = X[,'KK043'], 
                 KK106 = X[,'KK106'], 
                 KK039 = X[,'KK039'],
                 KK018 = X[,'KK018'],
                 KK015 = X[,'KK015'],
                 KK057 = X[,'KK057'],
                 KK060 = X[,'KK060'],
                 KK030 = X[,'KK030'],
                 KK025 = X[,'KK025'],
                 KK081 = X[,'KK081'])

params = c('b0', 'b')

mod = jags.model(textConnection(mod_string), data = data_jags, n.chains = 3)

update(mod, 1e3)


mod_sim = coda.samples(model = mod,
                       variable.names = params,
                       n.iter = 1e4)


mod_comb_sim = as.mcmc(do.call(rbind, mod_sim))

plot(mod_sim)
gelman.diag(mod_sim)


mean(unlist(as.vector(mod_sim[,2]))) %>%
  round(5) %>% 
  format(5) %>%
  paste("Posterior mean of b1 =",.)%>% cat(sep = "\n")



fitstats <- mcmcRocPrc(object = mod,
                       yname  = "budo",
                       xnames = c("KK043", "neuroticism", "extraversion"),
                       curves = TRUE,
                       fullsims = FALSE)


#모델 예측
post_coef = colMeans(mod_comb_sim)
post_Xb = post_coef['b0'] + X %*% post_coef[-10]

phat = 1.0 / (1.0 + exp(-post_Xb))

plot(phat, jitter(data$budo))

sum((data$budo[phat >= 0.5] == 1), (data$budo[phat < 0.5] == 0))/nrow(data)

#2439 0 is true, 2093 0 is false -> 1
# 9 is false 56is true 

#compare the codfficient -> mcmc logit 으로


#ROC
install.packages("R2jags")
library("R2jags")
install.packages("MCMCpack")
install.packages("BayesPostEst")
library(MCMCpack)
library(BayesPostEst)
fitstats <- mcmcRocPrcGen(object = mod,
                       yname  = "budo",
                       xnames = c("KK043","KK103","KK015","KK057","KK060","KK022","KK030","KK025","KK137","KK081"),
                       curves = TRUE,
                       fullsims = FALSE)


fit.jags <- jags(data = data, inits = inits.jags, 
                 parameters.to.save = params.jags, n.chains = 4, n.iter = 2000, 
                 n.burnin = 1000, model.file = "mod.jags")

