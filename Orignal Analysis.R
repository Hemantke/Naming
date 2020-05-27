data<-read.csv("Naming.csv",header = T,stringsAsFactors = F)
#This dataset "Naming" was downloaded on 26 May 2020, 04:24:51 PM From the form responses
#The dataset uploaded on github will have the Your Name and email column empty for privacy reasons


#Set seed for random data for reproducibility
set.seed(42)

control<-data[50,]          #my entry which will be the control

data<-data[c(-41,-50),]     #Remove 41st entry that was erroneous, and mine(50th) which will be the control


#To separate out the characters of the data
bychar=list("Haliax"=data[,4:11],
            "Vin"=data[,12:19],
            "Kaladin"=data[,20:27],
            "Hoid"=data[,28:35],
            "Jasnah.Kholin"=data[,36:43],
            "Kvothe"=data[,44:51],
            "Odium"=data[52:59],
            "Shallan.Davar"=data[60:67],
            "Dalinar"=data[68:75],
            "Kelsier"=data[76:83],
            "Moash"=data[84:91],
            "Szeth"=data[92:99])


#Cleaning control data, separate characters
control<-list("Haliax"=control[,4:11],
              "Vin"=control[,12:19],
              "Kaladin"=control[,20:27],
              "Hoid"=control[,28:35],
              "Jasnah.Kholin"=control[,36:43],
              "Kvothe"=control[,44:51],
              "Odium"=control[52:59],
              "Shallan.Davar"=control[60:67],
              "Dalinar"=control[68:75],
              "Kelsier"=control[76:83],
              "Moash"=control[84:91],
              "Szeth"=control[92:99])

#fixing and shortening headers in the bychar and control
headers=c("Intentions","Competence","Confidence","Happy.or.Sad","Steps.Up","Honest.or.Treacherous","Financial.Background","Remark")

for(i in 1:12)
{
    names(bychar[[i]])<-headers
    names(control[[i]])<-headers
}

#Storing info of the people who filled
people<-data[,c(1:3,100:102)]
names(people)<-c("Timestamp","Name","Reading.Frequency","Email","Another.form?","Suggestions")

#Cleaning out junk and empty names by Anonymous
people$Name[which(people$Name == "")]<-"Anonymous"

people$Name[c(2,26)]<-"Anonymous"




#Now the analysis begins


wts<-c(0.25,0.25,0.2,0.1,0.1,0.1,0)
results<-data.frame()

for(i in 1:12)
{
    pred<-bychar[[i]]
    tru<-control[[i]]
    accuracies=numeric(length = 8)
    for(j in 1:7)
    {
        x<-tru[,j]
        trait<-pred[,j]
        
        t=table(trait)
        modes<-which(t==t[which.max(t)])    #list of modes
        
        if(length(modes)==1)
            y<-modes            #If only one mode, directly assign to y
        else
            y<-sample(modes,1)  #If there's tie, select one among them randomly
        
        #Now that we have x and y for the respective character
        #let's calculate SLE, E and A
        SLE=((abs(x-1))+(abs(x-2))+(abs(x-3))+(abs(x-4))+(abs(x-5)))/5
        E=(abs(x-y))
        A=(E/SLE)
        accuracies[j]=A
        
    }
    accuracies[8]=wts %*% accuracies[1:7]
    results<-rbind(results,accuracies)
}

names(results)<-c(headers[1:7],"Weighted.Mean")
results<-rbind(results,sapply(results,mean))
row.names(results)<-c("Haliax",
                      "Vin",
                      "Kaladin",
                      "Hoid",
                      "Jasnah.Kholin",
                      "Kvothe",
                      "Odium",
                      "Shallan.Davar",
                      "Dalinar",
                      "Kelsier",
                      "Moash",
                      "Szeth",
                      "Mean")

results
