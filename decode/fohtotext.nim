import parseopt2, os, streams, queues

var
  filename = ""
  xmlfile = ""
  txtfile = ""
  rule = ""
  toxml = false
  totext = true

proc writeHelp() =
  echo("usage: fohtotext [options] filename") 
  echo(" filename: ", filename)
  echo(" --xml     or -x: ", toxml)
  echo(" --xmlfile or -X: ", xmlfile)
  echo(" --text    or -t: ", totext)
  echo(" --txtfile or -T: ", txtfile)
  echo(" --rule    or -r: ", rule)

proc getArgs(): int =  
  var result = 0
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      filename = key
    of cmdLongOption, cmdShortOption:
      case key
      of "help",    "h": result = 1
      of "xml",     "x": toxml = true
      of "xmlfile", "X": xmlfile = val
      of "text",    "t": totext = false
      of "txtfile", "T": txtfile = val
      of "rule",    "r": rule = val
      else: 
        echo("Invalid option: ", key, val)
        result = 2
    of cmdEnd: assert(false) # cannot happen
    else: discard
  if filename == "":
    result = 3
  elif existsFile(filename) == false:
    result = 4
  if result != 0:  
    echo("result: ", result)
    writeHelp()
  return result

var (fpath, fnode, fext) = splitFile(filename)

proc cleanup(line: string) : string =
  var 
    result = ""
    i = 0
    ch : char
  while i < line.len:
    ch = line[i]
    if ord(ch) >= 32 and ord(ch) < 128:
      result = result & ch
    inc(i)  
  if result.len > 0 and result[0] == ' ':
    result = result[1..high(result)]  
  return result

type
  Page = object
    data: Queue[string](32)

var
  tags  = initQueue[string](32)
  pages = initQueue[Page](24)

proc initPage : Page
  result = Page(data = initQueue[string](32))

proc readFOHFile(filename) =
  var
    line   = ""
    first  = true
    skip   = false
    notags = 0
    lineno = 0
    pageno = 0
    infile = newFileStream(filename, fmRead) 
  open(infilenim c )   
  defer: close(infile)
  while readline(infile, line) == true:
    var cl = cleanup(line)
    if ord(line[0]) == 0xb4:
      if first == true:
        skip = true
        first = false
      else:
        skip = false  
      if skip == true:
        tags.add(cl)
      else:
        if lineno == 0:
          var page = initPage()
          pages.add(page)
        page.data.add(cl) 
        if page.data.len == tags.len: 
          lineno = 0

echo("getargs")
var result = getArgs() 
if result != 0:
  quit("Result not zero " & result)  
readFOHFile(filename)            

# 
# def to_xml():
#   if len(options.xmlfile) == 0:
#     xmlfilename = '%s.xml' % (filename)
#   else: 
#     xmlfilename = options.xmlfile
#   xmlfile = open(xmlfilename, 'wt')
#   xmlfile.write('<%s>\n' % fnode)
#   for p, page in enumerate(data):
#     xmlfile.write('  <Page%03d>\n' % (p+1))
#     for i, tag in enumerate(tags):
#       xmlfile.write('    <%s><![CDATA[%s]]></%s>\n' % (tag, page[i], tag))
#     xmlfile.write('  </Page%03d>\n' % (p+1))
#   xmlfile.write('</%s>\n' % fnode)
#   xmlfile.close()
#  
# rules = {} # tag supplies starting line and starting column zero based
# rules['invoice'] = {}
# rules['invoice']['SuppLine01'] = (0, 87)
# rules['invoice']['SuppLine02'] = (1,  0)
# rules['invoice']['TermLine01'] = (14, 0)
# rules['invoice']['BodyLine01'] = (18, 0)
# rules['invoice']['TotlLine01'] = (40, 0)
# rules['creditnote'] = {}
# rules['creditnote']['SuppLine01'] = (0, 87)
# rules['creditnote']['SuppLine02'] = (1,  0)
# rules['creditnote']['TermLine01'] = (14, 0)
# rules['creditnote']['BodyLine01'] = (18, 0)
# rules['creditnote']['TotlLine01'] = (40, 0)
# rules['statement'] = {}
# rules['statement']['SuppLine01'] = (0,  2)
# rules['statement']['DebtLine01'] = (10, 2)
# rules['statement']['MessLine01'] = (19, 0)
# rules['statement']['BodyLine01'] = (23, 0)
# rules['statement']['PerdLine01'] = (49, 0)
# rules['statement']['PerdLine02'] = (51, 0)
# rules['statement']['RemtLine01'] = (58, 0)
#   
# def to_text():  
#   if not options.rule in rules:
#     print('Rule %s not in current text production rules' % (options.rule))
#     return
#   rule = rules[options.rule]
#   if len(options.txtfile) == 0:
#     txtfilename = '%s.txt' % (filename)
#   else: 
#     txtfilename = options.txtfile
#   txtfile = open(txtfilename, 'wt'# )
#   for p, page in enumerate(data):
#     currline = 0  
#     currcol = 0
#     for i, tag in enumerate(tags):
#       if tag in rule:
#         currcol = rule[tag][1]
#         while currline < rule[tag][0]:
#           txtfile.write('\n')
#           currline += 1
#       if currcol > 0:
#         txtfile.write(' '*currcol)    
#       txtfile.write('%s\n' % page[i])
#       currline += 1
#     txtfile.write('\f');  
#   txtfile.close()
# 
# def main():
#   read_file(filename)
#   if options.toxml == True:
#     to_xml()
#   if options.totext == True:
#     to_text()  
#   
# if __name__ == '__main__':
#   exit(main())
