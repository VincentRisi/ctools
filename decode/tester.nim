import strutils, queues

#echo "fred $2$1$2 $3" % ["dog", "horse", "3"]

type
  Page = object
    data: Queue[string]

var 
  tags = initQueue[string](32)
  pages = initQueue[Page](32)

proc addPage : Page =  
  result = Page(data : initQueue[string](32))

var x: string = "ts"
tags.add(x)
tags.add("raspberry pi")
echo tags.len
var 
  pageno = pages.len
  page = addPage()
pages.add(page)
page.data.add("ffff")
