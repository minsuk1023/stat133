# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)
  
  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p
  DoctorMatrix <- matrix(nrow= n.doctors, ncol = n.days)
  DoctorMatrix[,1] <- initial.doctors
  
  for(i in 2:n.days){
    DoctorMatrix[,i] <- DoctorMatrix[, i-1]
    doctor.chosen <- sample(1:n.doctors, 2, replace = FALSE)
    doctor1.adopts <- DoctorMatrix[doctor.chosen[1],i-1]
    doctor2.adopts <- DoctorMatrix[doctor.chosen[2],i-1]
    if(doctor1.adopts == 1 & doctor2.adopts == 0){
      DoctorMatrix[doctor.chosen[2], i] <- sample(0:1, 1, replace = FALSE, prob = c(1-p,p))
    }
    if(doctor1.adopts == 0 & doctor2.adopts == 1){
      DoctorMatrix[doctor.chosen[1], i] <- sample(0:1, 1, replace = FALSE, prob = c(1-p,p))
    }
  }  
  return(DoctorMatrix)
}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)
initial.doctors <- sample(0:1, 10, replace = TRUE, prob =c(0.9,0.1))

s1 <- sim.doctors(initial.doctors, 10, 10, 0.1)
s2 <- sim.doctors(initial.doctors, 10, 10, 0.3)
s3 <- sim.doctors(initial.doctors, 10, 10, 0.5)
s4 <- sim.doctors(initial.doctors, 10, 10, 0.7)
s5 <- sim.doctors(initial.doctors, 10, 10, 0.9)


plot(x= c(1:10), y= colSums(s1),ylim= c(0,10), xlab = "days", ylab = "the number of doctors that have already adopted the drug, on that day", 
     main = "Relationship between P and doctors adoption of drug")
lines(x= c(1:10), y= colSums(s2), col = "green")
lines(x= c(1:10), y= colSums(s3), col = "red")
lines(x= c(1:10), y= colSums(s4), col = "blue")
lines(x= c(1:10), y= colSums(s5), col = "yellow")


