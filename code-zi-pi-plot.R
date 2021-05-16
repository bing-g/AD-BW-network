#  calculate Pi, Zi
library(igraph) 

#igraph.group1
seqdeg=degree(igraph.group1)
Nnodes=length(seqdeg)
Z=seqdeg
Z[]=0
P=Z

Membership=membership(fc.group1)
Seq=seq(1:Nnodes)
for(i in 1:Nnodes){
  L=Membership==Membership[i]         
  neighbs=neighbors(igraph.group1,i)               
  Kis=sum(L[neighbs])
  SUM=0
  SUMsq=0	
  SUMP=0
  Miv=Seq[L]
  for(j in 1:sum(L)){
    neighbsj=neighbors(igraph.group1,Miv[j])
    Kjs=sum(L[neighbsj])
    SUM=SUM+Kjs
    SUMsq=SUMsq+Kjs^2
  }
  Z[i]=(Kis-SUM/sum(L))/sqrt(SUMsq/sum(L)-(SUM/sum(L))^2)
  if(Kis-SUM/sum(L)==0){Z[i]=0}
  for(k in 1:max(Membership)){
    Lp=Membership==k
    Kisp=sum(Lp[neighbs])
    SUMP=SUMP+(Kisp/seqdeg[i])^2}
  P[i]=1-SUMP
}
attribute_node.group1=cbind(degree=seqdeg,module=Membership,Pi=P,Zi=Z)


#igraph.group2
seqdeg=degree(igraph.group2)
Nnodes=length(seqdeg)
Z=seqdeg
Z[]=0
P=Z

Membership=membership(fc.group2)
Seq=seq(1:Nnodes)
for(i in 1:Nnodes){
  L=Membership==Membership[i]         
  neighbs=neighbors(igraph.group2,i)               
  Kis=sum(L[neighbs])
  SUM=0
  SUMsq=0	
  SUMP=0
  Miv=Seq[L]
  for(j in 1:sum(L)){
    neighbsj=neighbors(igraph.group2,Miv[j])
    Kjs=sum(L[neighbsj])
    SUM=SUM+Kjs
    SUMsq=SUMsq+Kjs^2
  }
  Z[i]=(Kis-SUM/sum(L))/sqrt(SUMsq/sum(L)-(SUM/sum(L))^2)
  if(Kis-SUM/sum(L)==0){Z[i]=0}
  for(k in 1:max(Membership)){
    Lp=Membership==k
    Kisp=sum(Lp[neighbs])
    SUMP=SUMP+(Kisp/seqdeg[i])^2}
  P[i]=1-SUMP
}
attribute_node.group2=cbind(degree=seqdeg,module=Membership,Pi=P,Zi=Z)


#igraph.group3
seqdeg=degree(igraph.group3)
Nnodes=length(seqdeg)
Z=seqdeg
Z[]=0
P=Z

Membership=membership(fc.group3)
Seq=seq(1:Nnodes)
for(i in 1:Nnodes){
  L=Membership==Membership[i]         
  neighbs=neighbors(igraph.group3,i)               
  Kis=sum(L[neighbs])
  SUM=0
  SUMsq=0	
  SUMP=0
  Miv=Seq[L]
  for(j in 1:sum(L)){
    neighbsj=neighbors(igraph.group3,Miv[j])
    Kjs=sum(L[neighbsj])
    SUM=SUM+Kjs
    SUMsq=SUMsq+Kjs^2
  }
  Z[i]=(Kis-SUM/sum(L))/sqrt(SUMsq/sum(L)-(SUM/sum(L))^2)
  if(Kis-SUM/sum(L)==0){Z[i]=0}
  for(k in 1:max(Membership)){
    Lp=Membership==k
    Kisp=sum(Lp[neighbs])
    SUMP=SUMP+(Kisp/seqdeg[i])^2}
  P[i]=1-SUMP
}
attribute_node.group3=cbind(degree=seqdeg,module=Membership,Pi=P,Zi=Z)


#igraph.group4
seqdeg=degree(igraph.group4)
Nnodes=length(seqdeg)
Z=seqdeg
Z[]=0
P=Z

Membership=membership(fc.group4)
Seq=seq(1:Nnodes)
for(i in 1:Nnodes){
  L=Membership==Membership[i]         
  neighbs=neighbors(igraph.group4,i)               
  Kis=sum(L[neighbs])
  SUM=0
  SUMsq=0	
  SUMP=0
  Miv=Seq[L]
  for(j in 1:sum(L)){
    neighbsj=neighbors(igraph.group4,Miv[j])
    Kjs=sum(L[neighbsj])
    SUM=SUM+Kjs
    SUMsq=SUMsq+Kjs^2
  }
  Z[i]=(Kis-SUM/sum(L))/sqrt(SUMsq/sum(L)-(SUM/sum(L))^2)
  if(Kis-SUM/sum(L)==0){Z[i]=0}
  for(k in 1:max(Membership)){
    Lp=Membership==k
    Kisp=sum(Lp[neighbs])
    SUMP=SUMP+(Kisp/seqdeg[i])^2}
  P[i]=1-SUMP
}
attribute_node.group4=cbind(degree=seqdeg,module=Membership,Pi=P,Zi=Z)

#igraph.group5
seqdeg=degree(igraph.group5)
Nnodes=length(seqdeg)
Z=seqdeg
Z[]=0
P=Z

Membership=membership(fc.group5)
Seq=seq(1:Nnodes)
for(i in 1:Nnodes){
  L=Membership==Membership[i]         
  neighbs=neighbors(igraph.group5,i)               
  Kis=sum(L[neighbs])
  SUM=0
  SUMsq=0	
  SUMP=0
  Miv=Seq[L]
  for(j in 1:sum(L)){
    neighbsj=neighbors(igraph.group5,Miv[j])
    Kjs=sum(L[neighbsj])
    SUM=SUM+Kjs
    SUMsq=SUMsq+Kjs^2
  }
  Z[i]=(Kis-SUM/sum(L))/sqrt(SUMsq/sum(L)-(SUM/sum(L))^2)
  if(Kis-SUM/sum(L)==0){Z[i]=0}
  for(k in 1:max(Membership)){
    Lp=Membership==k
    Kisp=sum(Lp[neighbs])
    SUMP=SUMP+(Kisp/seqdeg[i])^2}
  P[i]=1-SUMP
}
attribute_node.group5=cbind(degree=seqdeg,module=Membership,Pi=P,Zi=Z)

###################################################################
#zi pi graph on one plot
par(mfrow=c(1,1),mar=c(4,4,2,8))
plot(attribute_node.group1[,3],attribute_node.group1[,4]
     ,xlim=c(0,1),ylim=c(-4,4),xlab="Among-module connectivity Pi",ylab=("Within-module connectivity Zi"),col=2,pch=1,cex=0.8)
abline(v=0.62,h=2.5,col=8)
points(attribute_node.group5[,3],attribute_node.group5[,4],
       col="grey27",pch=0,cex=0.8)

points(attribute_node.group4[,3],attribute_node.group4[,4],
       col="cadetblue1",pch=5,cex=0.8)

points(attribute_node.group2[,3],attribute_node.group2[,4],
       col=3,pch=6,cex=0.8)
points(attribute_node.group3[,3],attribute_node.group3[,4],
       col=4,pch=2,cex=0.8)
text(0.15,4,"Module hubs")
text(0.8,4,"Network hubs")
text(0.15,-4,"Peripherals")
text(0.8,-4,"Connectors")

legend(1.05,4,legend=c("Ambient","Mesophilic Low-solid","Mesophilic","Mesophilic Co-digestion","Thermo"),
       pch=c(0,2,5,6,1),col=c("grey27",4,"cadetblue1",3,2),xpd=T,bty="n",pt.lwd = 2)





