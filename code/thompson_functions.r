### Thompson functions


# function for calculating mid-range values -------------------------
pxt.recording=function(pci,pxt){pci+(1-pci)*pxt}
pxt.survey=function(eps, pi, pr, pxt){(1-eps*pi*pr)*pxt}

px.mid = function()
{
  
  
  
  PXt = NULL
  PXt.min = NULL
  PXt.max = NULL
  sd=NULL
  PX0 = 1 # species is extant at year 0 P(X0)=1
  
  # first year t=1
  rec = recordings[recordings[,1]==years[1],]
  pci.mid = (rec[,2] + rec[,3]) / 2
  pci.min=rec[,2]
  pci.max=rec[,3]
  
  PXt[1]=pxt.recording(pci.mid, PX0)
  PXt.min[1]=pxt.recording(pci.min, PX0)
  PXt.max[1]=pxt.recording(pci.max, PX0)
  
  
  n=10000 #number of samples
  pxt.sam=rep(PX0,n)
  stdev=(rec[,3] - rec[,2])/4
  pci.sam = rnorm(n,pci.mid,stdev )
  pxt.sam = pxt.recording(pci.sam, pxt.sam)
  
  sd[1]=sd(pxt.sam)
  
  # t = 8
  
  for (t in 2:length(years)) {
    
    #calculating rj
    if (rec.year[t,2]) #if recording
    {
      
      rec = recordings[recordings[,1]==years[t],]
      
      #get mid point estimate
      pci.mid = (rec[,2] + rec[,3]) / 2
      
      pci.min=rec[,2]
      pci.max=rec[,3]
      
      
      
      PXt[t]=pxt.recording(pci.mid, PXt[t-1])
      
      PXt.min[t]=pxt.recording(pci.min, PXt.min[t-1])
      PXt.max[t]=pxt.recording(pci.max, PXt.max[t-1])
      
      #sample to get min and max bounds
      stdev=(rec[,3] - rec[,2])/4
      pci.sam = rnorm(n,pci.mid,stdev )
      pxt.sam = pxt.recording(pci.sam, pxt.sam)
      sd[t]=sd(pxt.sam)
      
      
      
      
    }  else #if survey
    {
      sur=surveys[surveys[,1]==years[t],]
      eps.mid = (sur[,2] + sur[,3]) / 2
      pi.mid = (sur[,4] + sur[,5]) / 2
      pr.mid = (sur[,6] + sur[,7]) / 2
      
      
      eps.min = sur[,2]
      pi.min = sur[,4]
      pr.min = sur[,6]
      
      eps.max = sur[,3]
      pi.max = sur[,5]
      pr.max = sur[,7]
      
      
      PXt[t]=pxt.survey(eps.mid, pi.mid, pr.mid, PXt[t-1])
      PXt.min[t] = pxt.survey(eps.max, pi.max, pr.max, PXt.min[t - 1])
      PXt.max[t] = pxt.survey(eps.min, pi.min, pr.min, PXt.max[t - 1])
      
      
      eps.sam = rnorm(n,eps.mid,(sur[,3] - sur[,2]) / 4)
      pi.sam = rnorm(n,pi.mid,(sur[,5] - sur[,4]) / 4)
      pr.sam = rnorm(n,pi.mid,(sur[,7] - sur[,6]) / 4)
      pxt.sam = pxt.survey(eps.sam, pi.sam, pr.sam, pxt.sam)
      sd[t]=sd(pxt.sam)
    }
    
  }
  # print(cbind(PXt-3*sd,PXt,PXt+3*sd))
  return (cbind(PXt-3*sd,PXt,PXt+3*sd, PXt.min, PXt.max))
  
}

