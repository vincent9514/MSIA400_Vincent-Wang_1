##Vincent Wang
##Q1

#load the data from txt
tensile = read.table("Tensile.txt", header=T)
View(tensile)

#transform the data into vector
resp <-c(t(as.matrix(tensile)))
resp

#create the labels and generate the factor level
treats <- c("HC5","HC10","HC15","HC20")
length(treats)
k <-4
n <-6
tm <- gl(k,1,n*k,factor(treats))
tm
length(tm)
View(tm)

#use the built-in anova
myANOVA <-aov(resp ~ tm)
myANOVA
summary(myANOVA)


#create self-anova function
data<-resp
group_labels<-tm


anova_function<-function(data,group_labels){
  df<- length(levels(group_labels))-1 #degree of freedom
  df_errors<- length(data)-df-1 #df of errors
  
  total<-length(data)-1 #n
  xi_bar<-aggregate(data ~ group_labels, FUN=mean)
  xi_bar<-as.vector(xi_bar$data) #xi_bar
  x_bar<-mean(data) #x_bar
  
  sum_of_squares_residuals<-sum((xi_bar-x_bar)^2)*(length(data)/length(levels(group_labels))) #SSR
  sum_of_squares_total<-sum((data-x_bar)^2) #SST
  sum_of_squares_error<-sum_of_squares_total-sum_of_squares_residuals #SSE
  mean_squares_residuals<-sum_of_squares_residuals/df #MSR
  mean_squares_total<-sum_of_squares_total/total #MST
  mean_squares_error<-sum_of_squares_error/df_errors #MSE
  f_value<-mean_squares_residuals/mean_squares_error #F
  f_test<-qf(0.99,df1=df,df2 =df_errors) #F_TEST
  
  if (f_value>f_test){
    result<-"Reject H0"
  }else{
    result<-"Not Reject H0"
  }
  return(result)
}

anova_function(data,group_labels)
