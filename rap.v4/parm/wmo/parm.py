import csv
import os
import re
fhour=['00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21']
#fhour=['00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21']
#fhour=['02']
#fletter=['R']
#fletter=['A','B','C','D','E','F','G','H','I','J','K','L','M','Z','Z','Z','Z','Z','N','Z','Z','O']
#fletter=['R','R','R','R','R','R','R','R','R','R','R','L','M','Z','Z','Z','Z','Z','N','Z','Z','O']
fletter=[]
for q in xrange(len(fhour)):
  fletter.append('86')
for i in xrange(len(fhour)):
  g = open("tgrib2_awprap13f"+fhour[i],"w")
  #with open('/meso/save/Corey.Guastini/nwprod/rap.v3.0.0/util/parm/grib2_awprap200f'+fhour[i],'r') as f:
  #with open('/meso/save/Geoffrey.Manikin/nwprod/rap.v3.0.0/util/parm2/grib2_awprap200f'+fhour[i],'r') as f:
  with open('grib2_awprap13f'+fhour[i],'r') as f:
    for row in f:
      matchspfh=re.search(r'NCPCP',row)
      matchhybrid=re.search(r'Surface',row)
      #match=re.search(r'L[A-Z]D[A-Z]',row)
      match=re.search(r'[A-Z][A-Z][A-Z][A-Z][0-9][0-9]',row)
      #if match and (not matchspfh):
      if match and matchspfh and matchhybrid:
        #extracted=list(match.group())
        extracted=row.split(" ")
        print extracted
        #extracted[-2]=fletter[i]
        extracted[13]='8'
        extracted[2]='APCP '
        #extracted[-2]='8'
        #extracted[-1]='6'
        extracted=" ".join(extracted)
        #print fhour[i]
        #print extracted
        #print row
        #newrow=re.sub(r'L[A-Z]D[A-Z]',extracted,row)
        #newrow=re.sub(r'[A-Z][A-Z][A-Z][A-Z][0-9][0-9]',extracted,row)
        newrow=extracted
        #print newrow
      else:
        newrow=row
      #print newrow
      g.write(newrow)
  g.close()
  os.rename("tgrib2_awprap13f"+fhour[i], "grib2_awprap13f"+fhour[i])
