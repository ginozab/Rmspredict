library("Rmspredict", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("devtools")

registerDoMC(2)

all.methods.minus.random <- list("majority", "avNNet", "svmRadial", "parRF", "rf", "C5.0", "gbm")

all.programs <- list("bpmail", "netweaver","diebierse","geogoogle","hftbomberman","inspirento","jnfe","jniinchi","lagoon","lavalamp","schemaspy","xisemele")

time1 <- system.time(test <- Rmspredict::benchmark(trials = 1, method = "svmRadial", program = "bpmail", data = totalCDCatMan))

time2 <- system.time(test1 <- Rmspredict::benchmark(trials = 1, method = "svmRadial", program = "bpmail", data = totalCDCatQMan))


print(time1)
print(time2)

print(summary(test))
print(summary(test1))

print("Starting 30 trials with 3 categories 1 rep each")

time.man.cat3.rep1 <- system.time(Man.cat3.rep1 <- Rmspredict::benchmark(trials = 30, method = all.methods.minus.random, program = all.programs, data = totalCDCatMan))

print("Starting 30 trials with 6 categories 1 rep each")

time.man.cat6.rep1 <- system.time(Man.cat6.rep1 <- Rmspredict::benchmark(trials = 30, method = all.methods.minus.random, program = all.programs, data = totalCDCatQMan))

print("Time for cat 3 rep 1")
print(time.man.cat3.rep1)
print("Time for cat 6 rep 1")
print(time.man.cat6.rep1)

print("Summary of cat 3 rep 1")
print(summary(Man.cat3.rep1))
print("Summary of cat 6 rep 1")
print(summary(Man.cat6.rep1))
