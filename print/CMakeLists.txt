project (ctools_print)

set (copy_list
  fohtotext.py  
  PrintCredit
  PrintCreditPdfOnly
  PrintCreditPdfEmail
  PrintDrOverdue1
  PrintDrOverdue2
  PrintInvoice
  PrintInvoicePdfOnly
  PrintInvoicePdfEmail 
  PrintMergeDrOverdue
  PrintMergePOrder 
  PrintMergeQuote
  PrintMergeRemit
  PrintPOrder1
  PrintPOrder2 
  PrintQuote1
  PrintQuote2
  PrintRemit1
  PrintRemit2
  PrintRemitPdfEmailOnly
  PrintStatement
  PrintStatementNoMail
  PrintStatementPdfOnly
  PrintStatementPdfEmailOnly
#  creditnote01
#  invoice01
#  statement01
)

foreach (name ${copy_list})
  file (COPY ${name} DESTINATION /ctools/bin FILE_PERMISSIONS OWNER_WRITE OWNER_READ OWNER_EXECUTE GROUP_WRITE GROUP_READ GROUP_EXECUTE WORLD_READ)
endforeach ()
