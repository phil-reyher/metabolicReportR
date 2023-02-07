#import modules
import pandas as pd
import xlrd
import os
import re

#path
path=r'C:/Users/reyhe/Documents/Parvo/Data/Parvo_Report_Single/'                           #the "r" in front of the path is important, don't ask me why
fnames = os.listdir(path)                                                   #file names as a list
len=len(fnames)                                                             #length of file name list
oname=fnames
repl="csv"
subs="xls"
compiled = re.compile(re.escape(subs), re.IGNORECASE)


for i in range(0,len):                                                      #python list index starts at 0
    fpath = os.path.join(path+fnames[i])                            
    file = xlrd.open_workbook(fpath)             #encoder magic to open shitty xls files
    df = pd.read_excel(file,engine="xlrd")                          
    oname[i] =compiled.sub(repl,oname[i])                               #replace filenames so its .csv
    df.to_csv(r'C:/Users/reyhe/Documents/Parvo/Data/Parvo_Report_Single/'+oname[i],index=False)    #exschport