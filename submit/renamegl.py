#!/usr/bin/env python
import glob, os, sys
comps = ['ctj', 'grc', 'ctn', 'orx', 'sfj', 'csc', 'sfi', 'qtm', 'hky', 'krs', 'kgi']
months = ['Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb']
comp = sys.argv[1]
month = sys.argv[2]
def rename (comp, month):
  for file in glob.glob('/ctools/gl/%sgl*' % (comp)):
    file2 = file.replace('%sgl' % (comp), '%s%s' % (comp, month))
    os.rename(file, file2)
if month in months and comp == 'ALL':
  for comp in comps:
	  rename(comp, month)
elif comp in comps and month in months:
  rename(comp, month)
else:
  print sys.argv
  print 'Please supply a valid Company (or ALL) & Valid Month Name'
