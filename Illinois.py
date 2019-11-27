# -*- coding: utf-8 -*-
"""
Created on Sat Sep 23 14:27:35 2017

@author: sushbiswal
"""

from bs4 import BeautifulSoup
import re
import pandas as pd
import numpy as np
import re
import os
import sys
import requests
import random


import urllib2
request = urllib2.Request('ftp://www.ilga.gov/JCAR/AdminCode/titles.html')
response = urllib2.urlopen(request)


html = urllib2.urlopen(request).read()
soup = BeautifulSoup(html)


import time
prepend='ftp://www.ilga.gov/JCAR/AdminCode/'

badlinks=[]
masterdf=pd.DataFrame()
for i in soup.findAll('a',{'class': "links"})[24:]:
    
    title = re.sub(r'[\x80-\xff-\n-\r]','',i.text.encode('utf-8'))
    href=i['href']
    link =prepend+href
    print link
    request = urllib2.Request(link)
    time.sleep(random.randint(2,5))
    html = urllib2.urlopen(request).read()
    sp = BeautifulSoup(html)
    try:
        for j in  sp.findAll('a',{'class': "links"})[42:]:
            
            sections_links= prepend+href.split('/')[0]+'/'+j['href']
            
            print sections_links
            
            request = urllib2.Request(sections_links)
            time.sleep(random.randint(2,5))
            html = urllib2.urlopen(request,timeout=90).read()
            sp1 = BeautifulSoup(html)
            Part =re.sub(r'[\x80-\xff-\n-\r]','',j.text.encode('utf-8'))
            for i in sp1.find('p', {'class':'heading'}):
                
                if 'SUBTITLE'== str(i)[:8] :
                    SUBTITLE= i
                else:
                    SUBTITLE= 'None'
                if 'CHAPTER'== str(i)[:7]:
                    CHAPTER=i
                
            
            #print Part
            if sp1.findAll('p', {'class':'subheading'}):
                        for t in (sp1.findAll('p', {'class':'subheading'})):
                             subpart = t.text
                             #print subpart
            else:
                subpart='None'
            if sp1.findAll('a',{'class': "links"}):
                        for o in sp1.findAll('a',{'class': "links"}):
                            links=sections_links.rsplit('/',1)[0]+'/'+o['href']
                            url=links
                            print links
                            
                            request = urllib2.Request(links)
                            time.sleep(random.randint(3,5))
                            html = urllib2.urlopen(request,timeout=90).read()
                            sp2 = BeautifulSoup(html)
                            
                            for k in  sp2.findAll('div',{'class':'WordSection1'}):
                                section= re.sub(r'[\x80-\xff-\n-\r]','',(k.find('b').text).encode('utf-8'))
                                if k.find('b').find_next('p').find_next('p'):
                                    section_text= re.sub(r'[\x80-\xff-\n-\r]','',k.find('b').find_next('p').find_next('p').text.encode('utf-8'))
                                if k.find('p',{'class':'JCARSourceNote'}):
                                    Citation= re.sub(r'[\x80-\xff-\n-\r]','',k.find('p',{'class':'JCARSourceNote'}).text.encode('utf-8'))
                                else:
                                    Citation='None'
                            
                                tmpdf = pd.DataFrame({'title':title,
                                                          'part':Part,
                                                          'chapter':CHAPTER,
                                                          'subpart':subpart,
                                                          'section':section,
                                                          'section_text':section_text,
                                                          'chapter':CHAPTER,
                                                          'citation':Citation,
                                                          'Subtitle':SUBTITLE,
                                                          'url':url}, index = [0])
                                        # append it to master dataframe
                                masterdf = masterdf.append(tmpdf)
                                print len(masterdf)
        
    except Exception, e:
        badlinks.append(links)
        time.sleep(5)
        continue
    
       
from sqlalchemy import create_engine

engine = create_engine('mssql+pyodbc://DBEditor_Regs:R-14914@Regs', encoding="utf-8", convert_unicode=True)
masterdf.to_sql('dbo.stateRegulationsIllinois1', engine, if_exists = 'append', index=None, chunksize=100)

masterdf.to_csv('C:/regreform/Illinois_part2new.csv',encoding="utf-8")

import time
prepend='ftp://www.ilga.gov/JCAR/AdminCode/'
masterdf=pd.DataFrame()
links=[]

for i in soup.findAll('a',{'class': "links"})[1:]:
    title = re.sub(r'[\x80-\xff-\n-\r]','',i.text.encode('utf-8'))
    href=i['href']
    link =prepend+href
    print link
    request = urllib2.Request(link)
    html = urllib2.urlopen(request).read()
    sp = BeautifulSoup(html)
    for j in  sp.findAll('a',{'class': "links"}):
        
        sections_links= prepend+href.split('/')[0]+'/'+j['href']
        print sections_links
        time.sleep(random.randint(2,4))
        request = urllib2.Request(sections_links)
        html = urllib2.urlopen(request,timeout=90).read()
        sp1 = BeautifulSoup(html)
        Chapter =sp1.find('p', {'class':'heading'}).text.split('\r')[1]
        Part =re.sub(r'[\x80-\xff-\n-\r]','',sp1.find('p', {'class':'heading'}).text.split('\r')[2].encode('utf-8'))
        #print Part
        if sp1.findAll('p', {'class':'subheading'}):
                    for t in (sp1.findAll('p', {'class':'subheading'})):
                         subpart = t.text
                         #print subpart
        if sp1.findAll('a',{'class': "links"}):
                    for o in sp1.findAll('a',{'class': "links"}):
                        links=sections_links.rsplit('/',1)[0]+'/'+o['href']
                        
                        
                        tmpdf = pd.DataFrame({'title':title,
                                                          'part':Part,
                                                          'chapter':Chapter,
                                                          'subpart':subpart,
                                                          'url':links
                                                          }, index = [0])
                        # append it to master dataframe
                        masterdf = masterdf.append(tmpdf)
                        print len(masterdf)
                        time.sleep(random.randint(3,9))
                