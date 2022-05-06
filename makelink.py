#!/usr/bin/env python
import sys, os, os.path
from optparse import OptionParser
parser = OptionParser()
if sys.platform == 'win32':
  parser.add_option("-m", "--maindir",  dest="maindir",  default=r"c:/ctools",     help="main directory")
else:
  parser.add_option("-m", "--maindir",  dest="maindir",  default=r"/ctools/dev",   help="main directory")
(options, args) = parser.parse_args()
maindir = options.maindir.replace('\\','/')
if os.path.exists(maindir) == False:
  print '%s does not exist. Please create it with the correct permissions and run again.' % (maindir)  
  exit(1)
if sys.platform == 'win32':
  command = 'mklink /J %s/source .' % (maindir)
else:
  command = 'ln -sf %s %s/source' % (os.getcwd(), maindir)
print command  
os.system(command)
