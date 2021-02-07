import pandas as pd
import re

dd = pd.read_csv('~/Dropbox/UZH/BSC-THESIS/DATA/idflakies.csv')

vm9 = pd.read_csv('/home/christian/Dropbox/UZH/BSC-THESIS/DATA/RAW/vm9-no-gc-merged.csv')
vm11 = pd.read_csv('/home/christian/Dropbox/UZH/BSC-THESIS/DATA/RAW/vm11-no-gc-merged.csv')

p = re.compile('[a-zA-Z0-9_-]+$')

projects = []

for url in dd['URL']:
    #print(url)
    m = p.search(url)
    name = m.group()
    #print(name)
    if name not in projects: projects.append(name)
    
print(projects)

idflakies = pd.DataFrame(projects, columns=['ProjectName'])


vm9_proj_working = idflakies.merge(vm9['ProjectName']).drop_duplicates(ignore_index=True)
vm11_proj_working = idflakies.merge(vm11['ProjectName']).drop_duplicates(ignore_index=True)

not_working = idflakies.merge(vm9_proj_working, how='left', indicator=True)
not_working = not_working.loc[ not_working['_merge'] == 'left_only' , 'ProjectName'].drop_duplicates()
not_working = not_working.reset_index(drop=True)
