#1
#method 1: shell get the less value of total-tolls that we need for one file
#filename is the path of the file and its name that we need
sdif = function (filename)
{
  shellCMD = paste("cut -f 10,11 -d,", filename,  "|awk -F, 'NR>=2{printf(\"%s\\n\", $2-$1)}'")
  less = as.numeric(system(shellCMD, intern=TRUE))
  #change it to be data frame to further merge table of 12 files
  as.data.frame(table(less), stringsAsFactors = FALSE)
}

#get the 12 files
paths = list.files(path ="/home/data/NYCTaxis", pattern = "trip_fare.*.csv", recursive = TRUE)
filename = lapply(paths, function(x) paste("/home/data/NYCTaxis/", x, sep = ""))

#parallel for all 12 files
library("parallel", lib.loc = "~/Rpackages")
cl = makeCluster(12, "FORK")
#Stime = replicate(5, system.time(clusterApply(cl, filename, sdif)))
clusterApply(cl, filename, function(x) system.time(sdif(x)))
stopCluster(cl)

#method 2 : R block + parallel
#use block to read the file and calculate less value that we need
dif = function(filepath)
{
  f = pipe(filepath, "r")  # open standard input
  #make the blockSize to be 1000000 because I find that the length of each file is at 10^7 level
  blockSize = 1000000
  lines = readLines(f, 1) #remove the first row which is the name of variables
  i = 1
  diff = list() #I use a list not vector because list can give me more accurate velue
  #diff is a list which is 
  
  #run for each block
  while(TRUE) {
    lines = readLines(f, blockSize) #This command use lots of time
    
    #means if read at the end of the file, break the loop
    if(length(lines) == 0)
      break
    #diff is a list,  the ithe is the less value of tolls and total
    diff[[i]] = sapply( strsplit(lines, ","), function(x) as.numeric(x[2]) - as.numeric(x[1]))
    i = i+1
  }
  close(f)
  #make in the results as a data frame in order to merge the 12 tables 
  as.data.frame(table(unlist(diff)), stringsAsFactors = FALSE)
}

#get the path of the files
paths = list.files(path ="/home/data/NYCTaxis", pattern = "trip_fare.*.csv", recursive = TRUE)
file_shell = sapply(paths, function(x) paste("cut -f 10,11 -d, /home/data/NYCTaxis/", x, sep = ""))

#parallel
cl = makeCluster(12, "FORK")
#Rtime = replicate(5, system.time(clusterApply(cl, file_shell, dif))) # run the dif function for all files
results = clusterApply(cl, file_shell, dif)#run the dif function for each file
stopCluster(cl)

###the merge function is to merge the tables that we get in each file together, x, y is the table that we wnat to merge
#and the result will give one table which merge table x and table y together
merge = function(x, y) {
  combine = rbind(x,y)
  #sum the frequncy which have the same name(same less value)
  freq = tapply(combine$Freq, combine$Var1, sum, na.rm=TRUE) 
  #make the result as a table in order to interation for merge
  one_table = data.frame(Var1 = names(freq), Freq = freq, row.names = NULL)
}
f = Reduce(merge, results)#apply merge function to the 12 files table
f$Var1 = as.numeric(as.character(f$Var1)) 
#because use merge, the value will because charactor, in order to sort them and get the decile, they should be changed to be numeric

#C version for calculate the less value of tolls and total
#the filename have the same means as the shell version
cdif = function(filename)
{
  dyn.load("cdif.so")
  #calculate the length of the file
  length =as.numeric(strsplit(x = system (paste("wc -l", filename),intern=TRUE), split = " +")[[1]][1])-1
  #connect with C give the filename that we want to calculate the less time and details in C script
  cresult = .C("cdif", filename = as.character(filename), less = as.double(rep(0,length)))
  as.data.frame(table(cresult$less),stringsAsFactors=FALSE ) #give the same results as dif function in R and also can save memory
}

#get the path, which is the same as before
#parallel for the 12 files
library("parallel", lib.loc = "~/Rpackages")
cl = makeCluster(12, "FORK")  #make a cluster 
results = clusterApply(cl, filename, cdif)
#Ctime = replicate(5, system.time(clusterApply(cl, filename, cdif))) #calculate the running time
stopCluster(cl)

########################analysis of the results
f = read.table("freq.txt") #freq.txt is the frequency table of total - tolls, the first col V1 is frequency, the second col V2 is the corresponding value
n=subset(f,V2<0) #the number of less values that little than 0

#order them according to the less value f$Var1 is the less value calculated before
order_diff = f[order(f$Var1),]
#get the decile(0.1 posibility of whole sum)
order_diff$Var1[which(cumsum(order_diff$Freq)>sum(order_diff$Freq)*0.1)[1]]

#plot the density 
f_freq= as.vector(rep(f$V2, f$V1))
dens = density(f_freq)
plot(dens, xlab = "USD", main = "Distribution of the less value for tolls and total")
#######################plot
library(reshape)
library(ggplot2)
library(Hmisc)

#plot for the user time, system time and real time for the three methods
times = data.frame(time =c("system", "user", "real"), shell = c(0.003, 0.039, 187.532), R = c(0.004, 0.041, 563.929), C = c(0.003, 0.057, 40.008))

#reshape time in order to use ggplot 
times= melt(times, id ='time', variable_name="methods")
ggplot(times,aes(methods,value,fill=time))+
  geom_bar(stat="identity",position='dodge')+scale_fill_manual(values=c( "#D55E00", "#0072B2","#F0E442"))+ #find the color properly in http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/#a-colorblind-friendly-palette
  xlab("method")+ylab("time/seconds")+ggtitle("Comparation of running time in shell, R and C")

#the plot without real time
ggplot(times[-c(3,6,9),],aes(methods,value,fill=time))+
  geom_bar(stat="identity",position='dodge')+scale_fill_manual(values=c("#0072B2","#F0E442"))+
  xlab("method")+ylab("time/seconds")+ggtitle("Comparation of running time in shell, R and C")
# times[-c(3,6,9),] means exclude the real time

#i =1,2,3 for user time, sys time and elapsed time repectely, can use lapply to plot each one
plot(1:5, Stime[i,], col = "red",type = "l", ylim = range(0.01, 0.09), ylab = "time/seconds", xlab = "times", main = "Compare stable of user time of shell, R and C")
lines(Rtime[i,], col = "black", lty = 2)
lines(Ctime[i,], col = "blue", lty = 4)
legend("topright", legend=c("shell","R","C"),col=c('red', 'black', 'blue'), lty=c(1,2,4), cex=0.5)

################################################################
##2
###chaeck wether the trip_fare and trip_data match
##get the shell command for fare data
fname = list.files(path ="/home/data/NYCTaxis", pattern = "trip_fare.*.csv", recursive = TRUE)
f_all = sapply(fname, function(x) paste("cut -f 1,4 -d, /home/data/NYCTaxis/", x, "|sed 1d", sep = ""))
#get the shell command for trip_data
dname = list.files(path ="/home/data/NYCTaxis", pattern = "trip_data.*.csv", recursive = TRUE)
d_all = sapply(dname, function(x) paste("cut -f 1,6 -d, /home/data/NYCTaxis/", x, "|sed 1d", sep = ""))
#mix them together
filename = as.data.frame(rbind(f_all, d_all),stringsAsFactors = FALSE)

#match function is to check whether the trip_fare match the reip_data (whether the same line describe the same thing)
#it returns a string, which tells you wether they match
match = function(filename) #filename[1] is the data from trip_fare, filename[2] is the data from trip_data
{ 
  fdata = system(filename[1], intern = TRUE) #use system to connect 
  ddata = system(filename[2], intern = TRUE)
  
  #check whether they decribe the same thing in the same line, fdata is for fare, ddata is for trip_data
  if(!all(fdata == ddata))
  {ans = "They are not matched"}else
    ans = "They are matched"
  
  ans
}

library("parallel", lib.loc = "~/Rpackages")
cl = makeCluster(12, "FORK")  
whetherMatch = clusterApply(cl, filename, match)
#system.time(clusterApply(cl, filename, match)) # run the dif function for all files
stopCluster(cl)
whetherMatch

###########################calculate the regression coefficient
#method 1 R block+parallel
#calculate the sufficient statistics of bata1 and bata0
suffic = function(file) #calcuate the total amount less the tolls in fare, and the trip time in data
{ 
  farec = pipe(file[1], "r")  # open standard input
  datac = pipe(file[2], "r")
  
  #connection the file and intialized the variable
  blockSize = 1000000
  linesF = readLines(farec, 1)
  linesD = readLines(datac, 1)#remove the first row which is the name of variables
  sumx = 0; sumy = 0; sumx2 = 0; sumxy = 0
  
  #run the file by block
  while(TRUE) {
    linesF = readLines(farec, blockSize)
    linesD = readLines(datac, blockSize)#This command use lots of time
    
    if(length(linesF) == 0)
      break
    
    #if it is not end of the file, then calculate the sufficient statistics for beta0 and beta1, use vectorlized to develop the running speed
    y = sapply( strsplit(linesF, ","), function(x) as.numeric(x[2]) - as.numeric(x[1])) #the totoal -toll value
    x = as.numeric(linesD)
    sumx = sumx + sum(x)
    sumy = sumy + sum(y)
    sumx2 = sumx2 + sum(x^2)
    sumxy = sumxy +sum(x*y)
  }
  #  close(farec); close(datac)
  length =as.numeric(system (paste(file[1], "|wc -l", sep = ""),intern=TRUE))-1  
  list(sumx, sumy, sumx2, sumxy, length) #I use list not vector becuase list gives more memory to make the results more accurate
}

#get the path of 12 shells which is similar below 
cl = makeCluster(12, "FORK") 
#results = clusterApply(cl, file_shell, suffic)
system.time(clusterApply(cl, file_shell, suffic)) # run the dif function for all files
stopCluster(cl)

#Method 2 C
suff = function(file)
{
  dyn.load("suf.so")
  length =as.numeric(strsplit(x = system (paste("wc -l", file[1]),intern=TRUE), split = " +")[[1]][1])-1
  #length =as.numeric(system (paste("wc -l", file[1]),intern=TRUE))-1 
  cresult = .C("suff", fare = as.character(file[1]), data = as.character(file[2]), x = as.double(0), y = as.double(0), xy = as.double(0), x2 = as.double(0))
  list(cresult$x, cresult$y, cresult$x2, cresult$xy, length)
}


path_fare = list.files(path ="/home/data/NYCTaxis", pattern = "trip_fare.*.csv", recursive = TRUE)
#path_fare = path_fare[!path_fare%in%"trip_fare_8.csv"] #do not include file 8
file_fare = sapply(path_fare, function(x) paste("/home/data/NYCTaxis/", x, sep = ""))
path_data = list.files(path ="/home/data/NYCTaxis", pattern = "trip_data.*.csv", recursive = TRUE)
#path_data = path_data[!path_data%in%"trip_data_8.csv"] #do not include file 8
file_data = sapply(path_data, function(x) paste("/home/data/NYCTaxis/", x, sep = ""))
file_shell = as.data.frame(rbind(file_fare, file_data, deparse.level = 0),stringsAsFactors = FALSE)
#the other way way is to put those command in the function, but I choose here becaue in this way I do not need to repeat it in two functions
#and there are only 12 files, so sapply did not takes much time. So there is not need to put one file in the function and paralell them. Morever,
#in this way, the function also can be used directly if the path of the file change.

cl = makeCluster(12, "FORK")  
results = clusterApply(cl, file_shell, suff) # run the dif function for all files
#system.time(clusterApply(cl, file_shell, suff))
stopCluster(cl)

#############calculate the beta1 and beta2
ef = as.data.frame(matrix(unlist(results), nrow = 5)) 
#ef means each file result, it is a data frame with 12 col and 5 rows, each col represent a file result, each row represents sumx, sumy, sumx2, sumxy, length respectly for each file
#use the formular which mentioned in the report
beta1 = (sum(ef[4,]) - 1/sum(ef[5,])*sum(ef[1,])*sum(ef[2,]))/(sum(ef[3,]) - 1/sum(ef[5,])*sum(ef[1,])^2)
beta0 = 1/sum(ef[5,])*sum(ef[2,]) - beta1*1/sum(ef[5,])*sum(ef[1,])

########method 2: shell + C
getData = function(filename)
{
  ddata = system(paste(filename), intern = TRUE)
  as.numeric(ddata)
}
fname = list.files(path ="/home/data/NYCTaxis", pattern = "trip_fare.*.csv", recursive = TRUE)
f_all = sapply(fname, function(x) paste("cut -f 7 -d, /home/data/NYCTaxis/", x, "|sed 1d", sep = ""))

library("parallel", lib.loc = "~/Rpackages")
cl = makeCluster(12, "FORK")  
x = clusterApply(cl, d_all, getData) # run the dif function for all files
#system.time(clusterApply(cl, f_all, getData))
stopCluster(cl)
#y for less value do the similar thing as bove, the y value can be download in my git

regression = function (x, y)
{
  dyn.load("wff.so")
  dyn.load("mycov.so")
  
  #calculate the variance of x
  varx = .C("welf_var", x = as.double(x), length = as.integer(length(x)), var = as.double(0))$var
  #calculate the covariance of y and x, and the mean od x, mean of y 
  xyResult =  .C("mycov", x = as.double(x), y = as.double(y), length = as.integer(length(x)), cov = as.double(0), meanx = as.double(0), meany = as.double(0))
  
  #the value that needed to calculate beta1 beta0 mentioned in the report part 2.2.1
  covxy = xyResult$cov
  meanx = xyResult$meanx
  meany = xyResult$meany
  beta1 = covxy/varx
  list(beta0 = meany - meanx*beta1, beta1 = beta1)
}

#put the value of x(trip_time), y(less time) in the function 
regres = regression(x,y)
wtime = system(regression(x,y))#welford time

########################analysis of the results plot
#plot the user, system and real time for the question 2, same as question1
times = data.frame(time =c("system", "user", "real"), R = c(0.004, 0.001, 658.619), C = c(0.005, 0.001, 72.778), Welford= c(7.320, 4.335, 12.293))
times= melt(times, id ='time', variable_name="methods")
ggplot(times,aes(methods,value,fill=time))+
  geom_bar(stat="identity",position='dodge')+scale_fill_manual(values=c( "#D55E00", "#0072B2","#F0E442"))+ #find the color properly in http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/#a-colorblind-friendly-palette
  xlab("method")+ylab("time/seconds")+ggtitle("Comparation of running time in R, C and Welford")
ggplot(times[-c(3,6,9),],aes(methods,value,fill=time))+
  geom_bar(stat="identity",position='dodge')+scale_fill_manual(values=c("#0072B2","#F0E442"))+
  xlab("method")+ylab("time/seconds")+ggtitle("Comparation of running time in R, C and Welford")
# times[-c(3,6,9),] means exclude the real time

#############3
library(biglm)
#I calculate the less time(y), trip_time(x) and sucharge(x2) which can see in my git, just dowload it and use the command below

data = as.data.frame(cbind(y=y, x=x, x2=x2))
f = y~x+x2
chunk = list()
# similar with the example of http://cran.r-project.org/web/packages/biglm/biglm.pdf
chunk[[1]] = data[1:10000000,]
fit = biglm(f, chunk[[1]], na.action=na.omit)

for (i in 1 :17)#because the number of row in data is 173179759
{chunk[[i+1]] = data[10000000*i+1:10000000*(i+1), ];
 fit = updata(fit, chunk[[i+1]])
}



