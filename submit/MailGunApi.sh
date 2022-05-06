htmlContent=$(</ctools/bin/MailGunMessage.html); curl -s --user api:key-14a564ed4dc9242de3b4f09b9fd72587 -X POST https://api.mailgun.net/v3/pos.christensen.co.za/messages --form-string template="$htmlContent" \
-F from='Linux Dev <sc@christensen.co.za>' \
-F to=$1 \
-F subject=$2 \
-F attachment=@/ctools/spl/I$3 \
-F tempate=$htmlContent
