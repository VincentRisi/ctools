#!/usr/bin/env python
# The renaming of W/Mans Comp 'COID' Files to 'WMans' & add in the month & year for filing
import glob, os, sys
comps = ['CTJ', 'GRC', 'CTN', 'ORX', 'SFJ', 'CSC', 'SFI', 'QTM', 'HKY', 'KRS']
months = ['Feb16', 'Feb17', 'Feb18', 'Feb19', 'Feb20', 'Feb21', 'Feb22', 'Feb23']
comp = sys.argv[1]
month = sys.argv[2]
def rename (comp, month):
#  for file in glob.glob('/ctools/temp/%sCOID1.6*' % (comp)):
#    file2 = file.replace('%sCOID1.6' % (comp), '%sWMans%s' % (month, comp))
  for file in glob.glob('/ctools/temp/%sCOID*' % (comp)):
    file2 = file.replace('%sCOID' % (comp), '%sWMans%s' % (month, comp))
    os.rename(file, file2)
if month in months and comp == 'ALL':
  for comp in comps:
	  rename(comp, month)
elif comp in comps and month in months:
  rename(comp, month)
else:
  print sys.argv
  print 'Please supply a valid Company (or ALL) & Valid Month Name'
