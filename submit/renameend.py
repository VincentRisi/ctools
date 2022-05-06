#!/usr/bin/env python
# The renaming of Payslyp 'END' Files to 'End' & add in the month & year for filing
import glob, os, sys
comps = ['CTJ', 'GRC', 'CTN', 'ORX', 'SFJ', 'CSC', 'SFI', 'QTM', 'HKY', 'KRS']
months = ['Mar21', 'Apr21', 'May21', 'Jun21', 'Jul21', 'Aug21', 'Sep21', 'Oct21', 'Nov21', 'Dec21', 'Jan22', 'Feb22']
comp = sys.argv[1]
month = sys.argv[2]
def rename (comp, month):
  for file in glob.glob('/ctools/temp/%sEND*' % (comp)):
    file2 = file.replace('%sEND' % (comp), '%sEnd%s' % (month, comp))
    os.rename(file, file2)
if month in months and comp == 'ALL':
  for comp in comps:
	  rename(comp, month)
elif comp in comps and month in months:
  rename(comp, month)
else:
  print sys.argv
  print 'Please supply a valid Company (or ALL) & Valid Month Name'
