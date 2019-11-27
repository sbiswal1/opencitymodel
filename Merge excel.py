# -*- coding: utf-8 -*-
"""
Created on Sat Mar 11 10:36:40 2017

@author: sushbiswal
"""

import pandas as pd
import numpy as np

import glob
glob.glob("C:/regreform/cleaned/Pages*.xlsx")

f =0

all_data = pd.DataFrame()
for f in glob.glob("C:/regreform/cleaned/Pages*.xlsx"):
    df = pd.read_excel(f)
    all_data = all_data.append(df,ignore_index=True)
    
all_data.describe()    

e= pd.read_excel("C:/regreform/cleaned/Pages from draft_2016_cost_benefit_report_12_14_2016_2-2.xlsx", skiprows= 2)
t = pd.read_excel("C:/regreform/cleaned/Pages from draft_2015_cost_benefit_report.xlsx",skiprows= 2)



e.head(10)

e = e.append(t,ignore_index=True)
e.describe()
e
from sqlalchemy import create_engine
engine = create_engine('mssql+pyodbc://DBUser_CyberTheft:Cyb16240@CyberTheft', encoding="utf-8", convert_unicode=True)

e.to_sql('reg_costs',engine,if_exists = 'append', index=None)



e= pd.read_excel("C:/regreform/draft_2010_bc_report11.xlsx", skiprows= 2)
e= pd.read_excel("C:/regreform/draft_2009_cb_report_final.xlsx", skiprows= 2)
e= pd.read_excel("C:/regreform/2008_cb_final_15032015.xlsx", skiprows= 2)
e= pd.read_excel("C:/regreform/cleaned/2007_cb_final_report_new.xlsx", skiprows= 2)
e= pd.read_excel("C:/regreform/2006_cb_final_report_16032016.xlsx")
e= pd.read_excel("C:/regreform/final_2005_cb_report_20161603.xlsx",skiprows= 2)
e= pd.read_excel("C:/regreform/2004_cb_final_2016173.xlsx")
e= pd.read_excel("C:/regreform/2003_cost-ben_final_rpt_20171703.xlsx")
e= pd.read_excel("C:/regreform/2002_report_to_congress_final _2016203.xlsx")
e= pd.read_excel("C:/regreform/costbenefitreport_2001_final.xlsx")
e= pd.read_excel("C:/regreform/costbenefitreport1998_final.xlsx")

e= pd.read_excel("C:/regreform/reg_left.xlsx")

e.describe()

from sqlalchemy import create_engine
engine = create_engine('mssql+pyodbc://DBUser_CyberTheft:Cyb16240@CyberTheft', encoding="utf-8", convert_unicode=True)

e.to_sql('dbo.Regleft',engine,if_exists = 'append', index=None)

engine
engine = create_engine('mssql+pyodbc://DBUser_Regs:DBu32740@Regs', encoding="utf-8", convert_unicode=True)




