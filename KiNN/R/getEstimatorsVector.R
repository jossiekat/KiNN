getEstimatorsVector <-
function (g,simv,Y,C,alpha)
{
  y2d<-list()
  neighborsInGraph(g,simv)->ng
  ent<-0
  probv<-list()
  t(apply(ng,1,function(x) (x*Y)))->ngvy
  t(apply(ng,1,function(x) (x*C)))->ngvc
  for (i in 1:nrow(ngvc))
  {
    ngvc[i,]->line
    line[line > 0]->line
    ent[i]<-entropy2d(line)  
    probv[[i]]<-prob2d(line)
  }
 
  
  for (i in 1:nrow(ngvc))
  {
    for (j  in 1:max(C))
    {
      Y[ngvc[i,]==j]->y2d[[j]]       
    }
    if (ent[i]>0.5)
        sig<-"*"
    else
      if (ent[i]>0.2)
        sig<-"."
    else
        sig<-" "
    
    ye<-vector()
    if (probv[[i]]>0) 
    {  k=1
      for (j  in 1:max(ngvc[i,]))
      {  
      if (length(y2d[[j]] ) > 0)
      {
      ye[k]<-mean(y2d[[j]])
      k=k+1
      }
      }

      
      if (ent[i]==0)
      {
        s<-paste0("p=",round(probv[[i]],3),",yhat=",round(as.numeric(ye),3))
        cat(sig," ","entropy=",ent[i],s,"\n")
      }
      else
      {
      s<-paste0("(p=",round(probv[[i]],3),",yhat=",round(ye,3),")")
      cat(sig," ","entropy=",ent[i],s,"\n")
      }
    }
    else
    { sig=" "
      s<-paste0("p=",1)
      cat(sig," ","entropy=",0,s,"-no neighbors in graph.\n")
      
     }
  }
  #print(ngvc)
  return (ngvc)
  
  
}
