#Name:Xingran Wang   Hengyuan Chang
#This is a discount amortization. The nominal interest rate is 0.04 and convertible semiannually effective rate is 0.02. The face value is 10000 with 0.06 annual coupons and the redemation value is also 10000. We compute the bond discount and fill out the full bond amortization schedule below:
#Set values
n <- 10 
i <- 0.04
C <- 10000
r <- 0.06
term <- n*2
j <- i/2
semi_coupon <- r/2
BV <- C*semi_coupon*((1-(1/(1+j)^term))/j)+C*(1/(1+j)^term)
#Calculate coupon size
Coupon_size <- C*semi_coupon

#Bond amortization table
first_row <- c(0,0,0,BV)
bond_amortization_table <- matrix(ncol=4, nrow=1+term)
bond_amortization_table[1,] <- first_row
for(time in 1: term) {
  Coupon_size
  interest_paid <- BV*j
  Amort_of_premium <- Coupon_size-interest_paid
  BV <- BV-Amort_of_premium
  bond_amortization_table[time+1,] <- c(Coupon_size,interest_paid,Amort_of_premium,BV)
}

#label
rownames (bond_amortization_table) = c (0,1:term)
colnames (bond_amortization_table) = c("Coupon","interest paid","Amortization of premium","Book Value")
bond_amortization_table


