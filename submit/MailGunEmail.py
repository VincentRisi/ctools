#!/usr/local/bin/python
import requests
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('-A', '--API',      default='api')
parser.add_argument('-a', '--auth',     default='key-14a564ed4dc9242de3b4f09b9fd72587')
parser.add_argument('-b', '--bcc',      default='pos2021@christensen.co.za')
parser.add_argument('-f', '--file',     default='Invoice.pdf')
parser.add_argument('-F', '--From',     default='CHRISTENSEN TOOLS LTD<sales@christensen.co.za>')
parser.add_argument('-r', '--reply_to', default='sales@christensen.co.za')
parser.add_argument('-s', '--subject',  default='Invoice 140931 ex Saftec JHB')
parser.add_argument('-t', '--template', default='MailGunMessage.html')
parser.add_argument('-T', '--To',       default='sc@christensen.co.za; Steve@Christensen.co.za')
parser.add_argument('-u', '--url',      default='https://api.mailgun.net/v3/pos.christensen.co.za/messages')
args = parser.parse_args()

template_file = args.template
post_url = args.url
post_auth = (args.API, args.auth)
post_files = dict()
files = args.file.split('|')
for i, file in enumerate(files):
    post_files['attachment[%d]' % (i)] = open(file, 'rb')
post_data = dict()
post_data['from'] = args.From
post_data['to'] = args.To
post_data['bcc'] = args.bcc
post_data['h:reply-to'] = args.reply_to
post_data['subject'] = args.subject

with open(template_file, 'r') as f:
    post_data['html'] = f.read()
    requests.post(post_url, auth=post_auth, files=post_files, data=post_data)
for file in post_files:
    post_files[file].close()   
	
#You can add extra argparse switch based arguments if you need something else that you want to default and be able to override.
#
#line 3 imports argparse
#line 5 creates an instance called parser
#lines 6-13 we add a bunch of switch based arguments
#
#line 14 gets the parser to return the list of arguments in an object called args
#
#line 16 we assign a variable call template_file = args.template which is --template at line 11
#
#line 17 we use args.url to create a variable called post_url (it could have been any name - I just liked the idea of post_...)
#
#lines 20-22 is to allow for 1 or more files to be put into post files, using a pipe delimited arg to be split into a list
#
#line 28 uses a with open of the template_file which will automatically close after running the indented block of lines 29 and 30
#
#lines 32 and 33 with close any files we opened at line 22
#
#we have 3 indented by 4 spaces blocks - ie line 22, lines 29 and 30 and line 33
#
	
