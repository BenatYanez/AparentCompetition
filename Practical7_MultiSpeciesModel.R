rm(list = ls())
graphics.off()
quartz()

library(ggplot2)


#############
##Model skeleton
#############

##set up parameter values
sP <- 0.5
Kp <- 240
lP <- 2.68275
hp <- 300
sL <- 0.07
Kl <- 870
lL <- 2.68275
hl <- 400
sH <- 0.5
Kh <- 970
lH <- 3.1682
hh <- 1000
fC <- 1
eC <- 1.85
mC <- 
fM <- 1.5
eM <- 0.6
mM <- ...
mW <- ...
b <- 0.8
g <- 0.38
dC <- 460
dM <- 46



nsteps <- 200
pop.df.0 <- data.frame(time=1:nsteps,P=numeric(nsteps),L=numeric(nsteps),H=numeric(nsteps),C=numeric(nsteps),M=numeric(nsteps),W=numeric(nsteps))

pop.df.0 <- within(pop.df.0,{
  P[1] <- 240
  L[1] <- 870
  H[1] <- 970
  C[1] <- 7
  M[1] <- 25
  W[1] <- 8
})
discretePop_functionTrophic1 <- function(p,s,k,a,l,h) {
  z <- p +s*p*(1-(p/k))-(a*l*p/(h+p)) #P=population size of trophic lvl 1, s=growth rate, k=carry capacity, a=predator population, h=rate of intake
  return(z)
}

discretePop_functionTrophic2C <- function(p,f,a1,h1,a2,h2,w,e,d) {
  z <- p +p*f*(a1/(a1+h1))*(a2/(a2+h2))-p*(1-a1/(a1+h1))*(1-a2/(a2+h2))-((w*e*p)/(p+d))
  return(z)
}
discretePop_functionTrophic2M <- function(p,f,a1,h1,w,e,d) {
  z <- p +p*f*(a1/(a1+h1))-p*(1-a1/(a1+h1))-((w*e*p)/(p+d))
  return(z)
}

discretePop_functionTrophic3 <- function(p,b,a1,d1,a2,d2,g) {
  z<-p+p*b*((a1/(a1+d1))+(a2/(a2+d2)))-p*g
}

for(t in 2:nsteps){
  pop.df.0 <- within(pop.df.0,{
     ##plants consumed by Caribou
    P[t]<- max(0,discretePop_functionTrophic1(P[t-1],sP,Kp,C[t-1],lP,hp))
    L[t] <- max(0,discretePop_functionTrophic1(L[t-1],sL,Kl,C[t-1],lL,hl))
    H[t] <- max(0,discretePop_functionTrophic1(H[t-1],sH,Kh,M[t-1],lH,hh))
    
    C[t] <- max(0,discretePop_functionTrophic2C(C[t-1],fC,L[t-1],hl,P[t-1],hp,W[t-1],eC,dC))
    
    M[t] <- max(0,discretePop_functionTrophic2M(M[t-1],fM,H[t-1],hh,W[t-1],eM,dM))
    
    W[t] <- max(0,discretePop_functionTrophic3(W[t-1],b,C[t-1],dC,M[t-1],dM,g))
  })
}


colors <- c("Plants"="brown","Lichen"="orange","Shrubs"="green","Caribou"="purple","Moose"="blue","Wolf"="red")
ggplot(data = pop.df.0)+
  geom_line(mapping=aes(x=time,y=P,color="Plants")) +
  geom_line(mapping=aes(x=time,y=L,color="Lichen")) +
  geom_line(mapping=aes(x=time,y=H,color="Shrubs")) +
  geom_line(mapping=aes(x=time,y=C,color="Caribou")) +
  geom_line(mapping=aes(x=time,y=M,color="Moose")) +
  geom_line(mapping=aes(x=time,y=W,color="Wolf")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "Densities", color="Legend")+
  scale_color_manual(values = colors)
#There is a cycling of shrub population while lichen and plants decrease intially and end up staying stable
#The is a cycling of moose and wold population in a similar fashion to a prey-predator system 
#Caribou remain at a cinstant population size at very low number

#############
##Model with limit on wolf population to a max of 10
#############
nsteps <- 200
pop.df.2 <- data.frame(time=1:nsteps,P=numeric(nsteps),L=numeric(nsteps),H=numeric(nsteps),C=numeric(nsteps),M=numeric(nsteps),W=numeric(nsteps))

pop.df.2 <- within(pop.df.2,{
  P[1] <- 240
  L[1] <- 870
  H[1] <- 970
  C[1] <- 7
  M[1] <- 25
  W[1] <- 8
})


for(t in 2:nsteps){
  pop.df.2 <- within(pop.df.2,{
    ##plants consumed by Caribou
    P[t]<- max(0,discretePop_functionTrophic1(P[t-1],sP,Kp,C[t-1],lP,hp))
    L[t] <- max(0,discretePop_functionTrophic1(L[t-1],sL,Kl,C[t-1],lL,hl))
    H[t] <- max(0,discretePop_functionTrophic1(H[t-1],sH,Kh,M[t-1],lH,hh))
    
    C[t] <- max(0,discretePop_functionTrophic2C(C[t-1],fC,L[t-1],hl,P[t-1],hp,W[t-1],eC,dC))
    
    M[t] <- max(0,discretePop_functionTrophic2M(M[t-1],fM,H[t-1],hh,W[t-1],eM,dM))
    
    W[t] <- min(max(0,discretePop_functionTrophic3(W[t-1],b,C[t-1],dC,M[t-1],dM,g)),10) #Limit wolf to max of 10
  })
}



colors <- c("Plants"="brown","Lichen"="orange","Shrubs"="green","Caribou"="purple","Moose"="blue","Wolf"="red")
ggplot(data = pop.df.2)+
  geom_line(mapping=aes(x=time,y=P,color="Plants")) +
  geom_line(mapping=aes(x=time,y=L,color="Lichen")) +
  geom_line(mapping=aes(x=time,y=H,color="Shrubs")) +
  geom_line(mapping=aes(x=time,y=C,color="Caribou")) +
  geom_line(mapping=aes(x=time,y=M,color="Moose")) +
  geom_line(mapping=aes(x=time,y=W,color="Wolf")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "Densities", color="Legend")+
  scale_color_manual(values = colors)

#The moose population stops oscillating, it remains constant at a certian point
#so do wolf and caribou populations, but a much smaller number

#############
##Model with limit on moose population
#############
nsteps <- 200
pop.df.3 <- data.frame(time=1:nsteps,P=numeric(nsteps),L=numeric(nsteps),H=numeric(nsteps),C=numeric(nsteps),M=numeric(nsteps),W=numeric(nsteps))

pop.df.3 <- within(pop.df.3,{
  P[1] <- 240
  L[1] <- 870
  H[1] <- 970
  C[1] <- 7
  M[1] <- 25
  W[1] <- 8
})


for(t in 2:nsteps){
  pop.df.3 <- within(pop.df.3,{
    ##plants consumed by Caribou
    P[t]<- max(0,discretePop_functionTrophic1(P[t-1],sP,Kp,C[t-1],lP,hp))
    L[t] <- max(0,discretePop_functionTrophic1(L[t-1],sL,Kl,C[t-1],lL,hl))
    H[t] <- max(0,discretePop_functionTrophic1(H[t-1],sH,Kh,M[t-1],lH,hh))
    
    C[t] <- max(0,discretePop_functionTrophic2C(C[t-1],fC,L[t-1],hl,P[t-1],hp,W[t-1],eC,dC))
    
    M[t] <- min(max(0,discretePop_functionTrophic2M(M[t-1],fM,H[t-1],hh,W[t-1],eM,dM)),30) #Limit moose to max of 30
    
    W[t] <- max(0,discretePop_functionTrophic3(W[t-1],b,C[t-1],dC,M[t-1],dM,g)) 
  })
}
colors <- c("Plants"="brown","Lichen"="orange","Shrubs"="green","Caribou"="purple","Moose"="blue","Wolf"="red")
ggplot(data = pop.df.3)+
  geom_line(mapping=aes(x=time,y=P,color="Plants")) +
  geom_line(mapping=aes(x=time,y=L,color="Lichen")) +
  geom_line(mapping=aes(x=time,y=H,color="Shrubs")) +
  geom_line(mapping=aes(x=time,y=C,color="Caribou")) +
  geom_line(mapping=aes(x=time,y=M,color="Moose")) +
  geom_line(mapping=aes(x=time,y=W,color="Wolf")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "Densities", color="Legend")+
  scale_color_manual(values = colors)
#Moose remain constant at population of 30, caribou remain constant at a low population while wolf go extinctbefore time 50

#############
##Model with wolf hunting
#############
mW <- 0.1
discretePop_functionTrophic3Hunting <- function(p,b,a1,d1,a2,d2,g,m) {
  z<-p+p*b*((a1/(a1+d1))+(a2/(a2+d2)))-p*g-p*m
}
pop.df.4 <- data.frame(time=1:nsteps,P=numeric(nsteps),L=numeric(nsteps),H=numeric(nsteps),C=numeric(nsteps),M=numeric(nsteps),W=numeric(nsteps))

pop.df.4 <- within(pop.df.4,{
  P[1] <- 240
  L[1] <- 870
  H[1] <- 970
  C[1] <- 7
  M[1] <- 25
  W[1] <- 8
})

for(t in 2:nsteps){
  pop.df.4 <- within(pop.df.4,{
    ##plants consumed by Caribou
    P[t]<- max(0,discretePop_functionTrophic1(P[t-1],sP,Kp,C[t-1],lP,hp))
    L[t] <- max(0,discretePop_functionTrophic1(L[t-1],sL,Kl,C[t-1],lL,hl))
    H[t] <- max(0,discretePop_functionTrophic1(H[t-1],sH,Kh,M[t-1],lH,hh))
    
    C[t] <- max(0,discretePop_functionTrophic2C(C[t-1],fC,L[t-1],hl,P[t-1],hp,W[t-1],eC,dC))
    
    M[t] <- max(0,discretePop_functionTrophic2M(M[t-1],fM,H[t-1],hh,W[t-1],eM,dM))
    
    W[t] <- max(0,discretePop_functionTrophic3Hunting(W[t-1],b,C[t-1],dC,M[t-1],dM,g,mW)) 
  })
}
ggplot(data = pop.df.4)+
  geom_line(mapping=aes(x=time,y=P,color="Plants")) +
  geom_line(mapping=aes(x=time,y=L,color="Lichen")) +
  geom_line(mapping=aes(x=time,y=H,color="Shrubs")) +
  geom_line(mapping=aes(x=time,y=C,color="Caribou")) +
  geom_line(mapping=aes(x=time,y=M,color="Moose")) +
  geom_line(mapping=aes(x=time,y=W,color="Wolf")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "Densities", color="Legend")+
  scale_color_manual(values = colors)
#Moose population remains constant at much higher population than cariou and wolfs, which stays at a higher population than caribou after the first few generations

#############
##Model with moose hunting
#############

mM <- 0.1
discretePop_functionTrophic2MHunting <- function(p,f,a1,h1,w,e,d,m) {
  z <- p +p*f*(a1/(a1+h1))-p*(1-a1/(a1+h1))-((w*e*p)/(p+d))-p*m
  return(z)
}
pop.df.5 <- data.frame(time=1:nsteps,P=numeric(nsteps),L=numeric(nsteps),H=numeric(nsteps),C=numeric(nsteps),M=numeric(nsteps),W=numeric(nsteps))

pop.df.5 <- within(pop.df.5,{
  P[1] <- 240
  L[1] <- 870
  H[1] <- 970
  C[1] <- 7
  M[1] <- 25
  W[1] <- 8
})

for(t in 2:nsteps){
  pop.df.5 <- within(pop.df.5,{
    ##plants consumed by Caribou
    P[t]<- max(0,discretePop_functionTrophic1(P[t-1],sP,Kp,C[t-1],lP,hp))
    L[t] <- max(0,discretePop_functionTrophic1(L[t-1],sL,Kl,C[t-1],lL,hl))
    H[t] <- max(0,discretePop_functionTrophic1(H[t-1],sH,Kh,M[t-1],lH,hh))
    
    C[t] <- max(0,discretePop_functionTrophic2C(C[t-1],fC,L[t-1],hl,P[t-1],hp,W[t-1],eC,dC))
    
    M[t] <- max(0,discretePop_functionTrophic2MHunting(M[t-1],fM,H[t-1],hh,W[t-1],eM,dM,mM))
    
    W[t] <- max(0,discretePop_functionTrophic3(W[t-1],b,C[t-1],dC,M[t-1],dM,g)) 
  })
}
ggplot(data = pop.df.5)+
  geom_line(mapping=aes(x=time,y=P,color="Plants")) +
  geom_line(mapping=aes(x=time,y=L,color="Lichen")) +
  geom_line(mapping=aes(x=time,y=H,color="Shrubs")) +
  geom_line(mapping=aes(x=time,y=C,color="Caribou")) +
  geom_line(mapping=aes(x=time,y=M,color="Moose")) +
  geom_line(mapping=aes(x=time,y=W,color="Wolf")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "Densities", color="Legend")+
  scale_color_manual(values = colors)
#The moose population ramains constant, both caribou and wold remain at very low populations