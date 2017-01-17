source("Auxiliary_functions.R")

# The quantity-quality trade-off
Ip = 1000 # total energy invested by the parent (arbitrary units)

curve(1000/x,from=0,to=25,ylim=c(0,300),xlab="Number of offspring (Ny)",ylab="Effort per offspring (Iy)",lty=2)
curve(2000/x,from=0,to=25,ylim=c(0,300),xlab="Number of offspring (Ny)",ylab="Effort per offspring (Iy)",add=T,lty=1)
curve(500/x,from=0,to=25,ylim=c(0,300),xlab="Number of offspring (Ny)",ylab="Effort per offspring (Iy)",add=T,lty=3)
legend(x=20,y=300,title="Ip",legend=c("2000","1000","5000"),lty=1:3,bty="n")

# Plot isoclines of parent fitness
Wp = c(5,10,20) # parental fitness

plot(0:25,0:25,ylim=c(0,0.3),type="n",xlab="",ylab="Wy",las=1,xaxs="i",yaxs="i")
mtext(side=1,at=c(-2,seq(0,25,5)),c("Ny","inf",200,100,67,50,40),line=2)
mtext(side=1,at=-2,line=1,"Iy")


abline(a=0,b=Wp[1]/Ip,col="grey")
abline(a=0,b=Wp[2]/Ip,col="grey")
abline(a=0,b=Wp[3]/Ip,col="grey")
text(22,.12,Wp[1],srt=180/pi*atan(Wp[1]/Ip*getCurrentAspect()),col="grey")
text(21,.22,Wp[2],srt=180/pi*atan(Wp[2]/Ip*getCurrentAspect()),col="grey")
text(12,.26,paste("Wp = ",Wp[3]),srt=180/pi*atan(Wp[3]/Ip*getCurrentAspect()),col="grey")


# Idem, but in a more replicable way using contour plot
#Iys = seq(0,25,1)
#Wys = c(0,0.3)
#Nys = Ip/Iys
#Wps = outer(Nys,Wys)
#contour(z=Wps,x=Iys,y=Wys,levels=1:20,xaxs="i",yaxs="i",method="edge")
# FIX!

# Add offspring fitness (Wy) curve

Iymin = 3 # minimum effort per offspring to make them viable 
Wymax = 0.6 # maximum survival probability for each offspring
Wy = function(Iy,Iymin=Iymin,Iymax=Ip,Wymax=Wymax){log(Iy/Iymin)*Wymax/log(Iymax/Iymin)}
Wy(3,Iymin,Ip,Wymax)
Wy(5,Iymin,Ip,Wymax)
Wy(1000,Iymin,Ip,Wymax)

curve(Wy(Iy=x,Iymin,Ip,Wymax),from=0,to=25,add=T,col=2,lwd=2)
curve(Wy(Iy=x,Iymin,Ip,0.3),from=0,to=25,add=T,col=3,lwd=2) # Case 1: decrease Wymax to 0.3
curve(Wy(Iy=x,1,Ip,Wymax),from=0,to=25,add=T,col=4,lwd=2) # Case 2: decrease Iymin to 1

Iyo = exp(1)*Iymin # Optimum effort per offspring
Iyo2 = exp(1)*1 # Optimum effort per offspring for case 2

Wyo = Wy(Iy=exp(1)*Iymin,Iymin,Ip,Wymax) # Survival of Iyo
Wyo1 = Wy(Iy=exp(1)*Iymin,Iymin,Ip,0.3) # Survival of Iyo for case 1
Wyo2 = Wy(Iy=exp(1)*1,1,Ip,Wymax) # Survival of Iyo for case 2

Wpo = Wyo*Ip/Iyo # Maximum parent fitness
Wpo1 = Wyo1*Ip/Iyo # Maximum parent fitness for case 1
Wpo2 = Wyo2*Ip/Iyo2 # Maximum parent fitness for case 2

abline(a=0,b=Wyo/Iyo,col=2,lty=2)
abline(a=0,b=Wyo1/Iyo,col=3,lty=2)
abline(a=0,b=Wyo2/Iyo2,col=4,lty=2)

points(x=Iyo,y=Wyo,pch=16)
points(x=Iyo,y=Wyo1,pch=16)
points(x=Iyo2,y=Wyo2,pch=16)

text(16,.215,round(Wpo),srt=180/pi*atan(Wpo/Ip*getCurrentAspect()),col=2)
text(20,.135,round(Wpo1),srt=180/pi*atan(Wpo1/Ip*getCurrentAspect()),col=3)
text(7,.24,round(Wpo2),srt=180/pi*atan(Wpo2/Ip*getCurrentAspect()),col=4)

segments(x0=Iyo,y0=0,x1=Iyo,y1=Wyo,lty=3)
segments(x0=Iyo2,y0=0,x1=Iyo2,y1=Wyo2,lty=3)

mtext(side=1,c(round(Iyo),round(Ip/Iyo)),line=1:2,at=Iyo,col=2)
mtext(side=1,line=1:2,c(round(Iyo2),round(Ip/Iyo2)),at=Iyo2,col=4)


# Plot parents fitness (Wp)

plot(0:25,0:25,type="n",xlab="",ylab="Wp",las=1,xaxs="i",yaxs="i",ylim=c(0,35))
mtext(side=1,at=c(-2,seq(0,25,5)),c("Ny","inf",200,100,67,50,40),line=2)
mtext(side=1,at=-2,line=1,"Iy")

curve(Wy(Iy=x,Iymin,Ip,Wymax)*Ip/x,from=0,to=25,add=T,col=2,lwd=2)
curve(Wy(Iy=x,Iymin,Ip,0.3)*Ip/x,from=0,to=25,add=T,col=3,lwd=2)
curve(Wy(Iy=x,1,Ip,Wymax)*Ip/x,from=0,to=25,add=T,col=4,lwd=2)

points(x=Iyo,y=Wpo,pch=16)
points(x=Iyo,y=Wpo1,pch=16)
points(x=Iyo2,y=Wpo2,pch=16)

abline(h=Wpo,col=2,lty=2)
abline(h=Wpo1,col=3,lty=2)
abline(h=Wpo2,col=4,lty=2)

segments(x0=Iyo,y0=0,x1=Iyo,y1=Wyo*Ip/Iyo,lty=3)
segments(x0=Iyo2,y0=0,x1=Iyo2,y1=Wyo2*Ip/Iyo2,lty=3)

mtext(side=1,c(round(Iyo),round(Ip/Iyo)),line=1:2,at=Iyo,col=2)
mtext(side=1,line=1:2,c(round(Iyo2),round(Ip/Iyo2)),at=Iyo2,col=4)

legend(x=18,y=25,col=2:4,legend=c("3 ; 0.6","3 ; 0.3","1 ; 0.6"),lty=1,lwd=2,bty="n")

text(x=c(20,20),y=c(28,26),c("Minimum seed size;","maximum Wy"))


# When Ny optimum is a fraction
Nyo4 = 1.5 # case 4: when the optimum Ny is 12.5
Iyo4 = Ip/Nyo4
Iymin4 = Iyo4/exp(1)
Wyo4 = Wy(Iyo4,Iymin4,Ip,Wymax)
Wpo4 = Nyo4*Wyo4

# Closest integers
Ny4a = 1
Ny4b = 2

Iy4a = Ip/Ny4a
Iy4b = Ip/Ny4b
Wy4a = Wy(Iy4a,Iyo4/exp(1),Ip,Wymax)
Wy4b = Wy(Iy4b,Iyo4/exp(1),Ip,Wymax)
Wp4a = Ny4a*Wy4a
Wp4b = Ny4b*Wy4b

plot(0:25,0:25,type="n",xlab="",ylab="Wp",las=1,xaxs="i",yaxs="i",ylim=c(.59,.65),xlim=c(490,1010))
#mtext(side=1,at=c(-2,seq(0,25,5)),c("Ny","inf",200,100,67,50,40),line=2)
#mtext(side=1,at=-2,line=1,"Iy")
curve(Wy(Iy=x,Iymin4,Ip,Wymax)*Ip/x,from=400,to=1100,add=T,col=2,lwd=2)
abline(h=Wpo4,col=2,lty=2)
points(x=Iyo4,y=Wpo4,pch=16)
segments(x0=Iyo4,y0=0,x1=Iyo4,y1=Wpo4,lty=3)
mtext(side=1,c(round(Iyo4),round(Ip/Iyo4,1)),line=1:2,at=Iyo4,col=2)

#plot(0:25,0:25,type="n",xlab="",ylab="Wp",las=1,xaxs="i",yaxs="i",ylim=c(8,9),xlim=c(12,14),xaxt="n")
#mtext(side=1,at=c(11,10:20),c("Ny",round(1000/(10:20))),line=2)
#axis(at=10:20,labels=10:20,side=1)
#mtext(side=1,at=11,line=1,"Iy")
#curve(Wy(Iy=x,Iymin4,Ip,Wymax)*Ip/x,from=0,to=25,add=T,col=2,lwd=2)
#points(x=Iyo4,y=Wpo4,pch=16)
#segments(x0=Iyo4,y0=0,x1=Iyo4,y1=Wyo4*Ip/Iyo4,lty=3)
#mtext(side=1,c(round(Iyo4),round(Ip/Iyo4,1)),line=1:2,at=Iyo4,col=2)
points(x=Iy4a,y=Wp4a,pch=16,col=3)
segments(x0=Iy4a,y0=0,x1=Iy4a,y1=Wp4a,lty=3,col=3)
segments(x0=0,y0=Wp4a,x1=Iy4a,y1=Wp4a,lty=3,col=3)
mtext(side=1,c(round(Iy4a),round(Ny4a,1)),line=1:2,at=Iy4a,col=3)
points(x=Iy4b,y=Wp4b,pch=16,col=4)
segments(x0=Iy4b,y0=0,x1=Iy4b,y1=Wp4b,lty=3,col=4)
segments(x0=0,y0=Wp4a,x1=Iy4b,y1=Wp4b,lty=3,col=4)
mtext(side=1,c(round(Iy4b),round(Ny4b,1)),line=1:2,at=Iy4b,col=4)



