from bs4 import BeautifulSoup as bs

for i in range(1981,2017):
    response = requests.get('https://www.reginfo.gov/public/do/XMLViewFileAction?f=EO_RULE_COMPLETED_'+str(i)+'.xml')
    bs_obj = bs(response.content,'xml')   
    print bs_obj


details=[]
field_list = ['AGENCY_CODE','RIN','TITLE','STAGE','DATE_COMPLETED','ECONOMICALLY_SIGNIFICANT','DATE_RECEIVED','LEGAL_DEADLINE',
             'DATE_COMPLETED','DECISION','DATE_PUBLISHED','HEALTH_CARE_ACT','DODD_FRANK_ACT','INTERNATIONAL_IMPACTS']
for i in range(1981,2017):
   
    response = requests.get('https://www.reginfo.gov/public/do/XMLViewFileAction?f=EO_RULE_COMPLETED_'+str(i)+'.xml')
    bs_obj = bs(response.content,'xml')
    for j in bs_obj.find_all("REGACT"):
        det_dict = {} 
        for f in field_list:
            try:
                det_dict[f] = j.find(f).text
            except:
                det_dict[f] = ""
        details.append(det_dict)

import pandas as pd
final = pd.DataFrame(details)