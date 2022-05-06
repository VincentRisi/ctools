curl -s --user 'api:key-14a564ed4dc9242de3b4f09b9fd72587' -X POST \
https://api.mailgun.net/v3/pos.christensen.co.za/templates \
-F name='MailGunMessage.html' \
-F description='Test template for Mailgun' \
-F template='MailGunMessage.html' 
