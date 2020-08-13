

#1a)
sequence <- function(n) {
  #create empty list and initialize base case values at n=1 and n=2
  value<-c()
  value[1]<-1
  value[2]<-1
  
  if (n==1) {
    print(value[1])
  }
  else if (n==2) {
    print(value[2])
  }
  else {
    # create a for loop that calculates the sequence value through formula given
    #using n-1 and n-2 values for each val starting from 3 until we reach n        
    
    for (i in 3:n) {
      value[i]= value[i-1]+value[i-2]+2*i #given sequence formula
    }
  }
  print(value[i])
}

sequence(36)


#1b)
binom_coeff<- function(n,k){
  c=1
  if(k>n){
    return(0)
  } # 0 combination when k>n since k<=n 
  else if(n==k){
    return(1)
  } #only one way to chose when n == k
  else if(k>n-k){
    k=n-k
  } #Everything is symmetric around n-k, so it is quicker to iterate 
  #over a smaller k than a larger one                              
  
  for (i in 1:k){ #iterate through k to get the number of combinations using the above property
    c=c*n
    n=n-1
    c=c/i}
  return(c)
}

binom_coeff(88,44)


#1c)
#greatest common divisor
gcd<- function(x,y){
  if(x>y){
    upper_bound=y #set y as upper bound if x>y
  }
  
  else{
    upper_bound=x #else set x as upper bound
  }  
  
  while(TRUE){
    if((x%%upper_bound==0)&(y%%upper_bound==0)){ #when modulus of 2 input values divided by upperbound 
      #is zero,the upperbound is the gcd
      gcd=upper_bound
      print(paste("greatest common divisor is",gcd))
      break
    }
    
    upper_bound=upper_bound-1 
  }
}
gcd(12306,32148)

#least common multiple
lcm<-function(x,y){
  if(x>y){
    lower_bound=x #set x as lower bound if x>y
  }
  else{
    lower_bound=y #else set y as lower bound 
  }
  
  while(TRUE){
    if((lower_bound%%x==0)&(lower_bound%%y==0)){ #when modulus of lower_bound divided by 2 input values 
      #is zero,the lower_bound is the lcm
      lcm=lower_bound
      print(paste("least common multiple is",lcm))
      break
    }
    
    lower_bound=lower_bound+1 
    
  }
}

lcm(12306,32148)




#2a)
#reads file into R
WHO = read.csv("WHO.csv")
summary(WHO)
print("variables that have at least three missing values include FertilityRate,CellularSubscribers,LiteracyRate,GNI,PrimarySchoolEnrollmentMale and PrimarySchoolEnrollmentFemale.")

#2b)

WHO$Country[which.max(WHO$FertilityRate)]
print("the country with the highest fertility rate is Niger")
WHO$Country[which.min(WHO$FertilityRate)]
print("the country with the lowest fertility rate is Bosnia and Herzegovina")

#2c)
tapply(WHO$GNI, WHO$Region, sd, na.rm=TRUE)
print("South-East Asia has the minimum variation in Gross National Income")
print("the standard deviation of GNI in South-East Asia is 2477.34")


#2d)
#subset of rows of rich countries
Rich_countreis = subset(WHO, GNI>20000)
mean(Rich_countreis$ChildMortality)
print("the mean child mortality in rich countries is approx. 7.448649.")

#2e)
#plots income level v.s life expectancy
plot(WHO$GNI,WHO$LifeExpectancy,xlab = "Income Level",ylab="Life Expectancy",main="Income Level and Life Expectancy")













#2a
#loading the file into R
WHO<-read.csv("C:/Users/123/Desktop/WHO.csv")
names(WHO)
summary(WHO)
print("The variables that have at least three missing values include FertilityRate,CellularSubscribers,LiteracyRate,GNI,PrimarySchoolEnrollmentMale and PrimarySchoolEnrollmentFemale.")

#2b
#highest fertility rate
WHO$Country[which.max(WHO$FertilityRate)]
print("The country with the highest fertility rate is Niger")
#lowest fertility rate
WHO$Country[which.min(WHO$FertilityRate)]
print("the country with the lowest fertility rate is Bosnia and Herzegovina")

#2c
tapply(WHO$GNI,WHO$Region,sd,na.rm=TRUE)
print("South-East Asia has the minimum variation in Gross National Income")
print("The standard deviation of GNI in South-East Asia is 2477.34")

#2d
#subsetting the rows of only rich countries
RichCountry<-subset(WHO,GNI>20000,na.rm=TRUE)
mean(RichCountry$ChildMortality,na.rm=TRUE)
print("The mean child mortality of the rich countries is 7.448649.")

#2e
plot(WHO$GNI,WHO$LifeExpectancy,xlab = "Income Level",ylab="Life Expectancy",main="Income Level and Life Expectancy")
