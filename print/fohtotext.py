#!/usr/bin/env python
import sys, os, os.path
from optparse import OptionParser

parser = OptionParser()
parser.add_option("-x", "--xml",     dest="toxml",  action="store_true",  default=False, help="to xml")
parser.add_option('-X', '--xmlfile', dest='xmlfile',                      default='', help='output file for xml');
parser.add_option("-t", "--text",    dest="totext", action="store_false", default=True,  help="to text")
parser.add_option('-T', '--txtfile', dest='txtfile',                      default='', help='output file for text');
parser.add_option('-r', '--rule',    dest='rule',                         default='invoice', help='rule to use for text');
(options, args) = parser.parse_args()

if len(args) < 1:
  print 'usage: python fohtotext.py [options] filename'
  parser.print_usage()
  exit(1)
filename = args[0]
fpath, fname = os.path.split(filename)
fnode, fext  = os.path.splitext(fname)
  
def cleanup(line):
  result = ''
  for i, ch in enumerate(line):
    if ord(ch) >= 32 and ord(ch) < 128:
      result += ch
  if len(result) > 0 and result[0] == ' ':
    result = result[1:]    
  return result    
  
tags = []  
data = []

def read_file(filename):
  infile = open(filename, 'rt')
  lines = infile.readlines()
  infile.close()
  first = True
  skip = False
  notags = 0
  lineno = 0
  pageno = 0
  for line in lines:
    if ord(line[0]) == 0xb4: 
      if first == True:
        skip = True
        first = False
      else:
        skip = False  
    if skip == True:
      tags.append(cleanup(line))
      notags += 1
    else:   
      if lineno == 0:
        data.append([]); 
      data[pageno].append(cleanup(line))
      lineno += 1
      if lineno == notags:
        lineno = 0
        pageno += 1

def to_xml():
  if len(options.xmlfile) == 0:
    xmlfilename = '%s.xml' % (filename)
  else: 
    xmlfilename = options.xmlfile
  xmlfile = open(xmlfilename, 'wt')
  xmlfile.write('<%s>\n' % fnode)
  for p, page in enumerate(data):
    xmlfile.write('  <Page%03d>\n' % (p+1))
    for i, tag in enumerate(tags):
      xmlfile.write('    <%s><![CDATA[%s]]></%s>\n' % (tag, page[i], tag))
    xmlfile.write('  </Page%03d>\n' % (p+1))
  xmlfile.write('</%s>\n' % fnode)
  xmlfile.close()
 
rules = {} # tag supplies starting line and starting column zero based
rules['invoice'] = {}
rules['invoice']['SuppLine01'] = (0, 50)
rules['invoice']['SuppLine02'] = (1,  0)
rules['invoice']['TermLine01'] = (14, 0)
rules['invoice']['BodyLine01'] = (18, 0)
rules['invoice']['TotlLine01'] = (40, 0)
rules['creditnote'] = {}
rules['creditnote']['SuppLine01'] = (0, 50)
rules['creditnote']['SuppLine02'] = (1,  0)
rules['creditnote']['TermLine01'] = (14, 0)
rules['creditnote']['BodyLine01'] = (18, 0)
rules['creditnote']['TotlLine01'] = (39, 0)
rules['statement'] = {}
rules['statement']['SuppLine01'] = (0,  2)
rules['statement']['DebtLine01'] = (10, 2)
rules['statement']['MessLine01'] = (19, 0)
rules['statement']['BodyLine01'] = (23, 0)
rules['statement']['PerdLine01'] = (49, 0)
rules['statement']['PerdLine02'] = (51, 0)
rules['statement']['RemtLine01'] = (58, 0)
  
def to_text():  
  if not options.rule in rules:
    print('Rule %s not in current text production rules' % (options.rule))
    return
  rule = rules[options.rule]
  if len(options.txtfile) == 0:
    txtfilename = '%s.txt' % (filename)
  else: 
    txtfilename = options.txtfile
  txtfile = open(txtfilename, 'wt')
  for p, page in enumerate(data):
    currline = 0  
    currcol = 0
    for i, tag in enumerate(tags):
      if tag in rule:
        currcol = rule[tag][1]
        while currline < rule[tag][0]:
          txtfile.write('\n')
          currline += 1
      if currcol > 0:
        txtfile.write(' '*currcol)    
      txtfile.write('%s\n' % page[i])
      currline += 1
    txtfile.write('\f');  
  txtfile.close()

def main():
  read_file(filename)
  if options.toxml == True:
    to_xml()
  if options.totext == True:
    to_text()  
  
if __name__ == '__main__':
  exit(main())
