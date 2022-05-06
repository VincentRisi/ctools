template_attachment = "@/ctools/bin/MailGunMessage.html"
def send_complex_message():
    return requests.post(
        "https://api.mailgun.net/v3/pos.christensen.co.za/messages",
        auth=("api", "key-14a564ed4dc9242de3b4f09b9fd72587"),
        data={
              "from": "Development Linux <sc@Christensen.co.za>",
              "to": "Steve@Christensen.co.za",
              "subject": "Test of Mailgun from python",
              "text": "Testing some Mailgun an Python",
			  "attachment": "@/ctools/spl/Invoice.pdf",
              "html": template_attachment})
