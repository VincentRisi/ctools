x = '''
CONTMENU.FRP  DR3MENU.FRP   DRMASTER.FRP  SLINVOIC.FRP	SLQUOTE.FRP  ST3MENU.FRP   STMASTMT.FRP
DR1MENU.FRP   DR4MENU.FRP   DRPAYMNT.FRP  SLORDERS.FRP	ST1MENU.FRP  ST4MENU.FRP   STREIMMT.FRP
DR2MENU.FRP   DRACSTIQ.FRP  MAINCONT.FRP  SLPSLPIQ.FRP	ST2MENU.FRP  STMASTIQ.FRP  STRELOMT.FRP
'''

files = x.split()

print 'set (frp_source'
for file in files:
	print '  ${CTOS_SOURCE_DIR}/data/%s' % (file)
print ')'

print 'set (bin_files'
for file in files:
	print '  ${CTOS_BINARY_DIR}/data/%s' % (file.replace('.FRP', '.bin'))
print ')'

print 'set (cob_files'
for file in files:
	print '  ${CTOS_BINARY_DIR}/cob/%s' % (file.replace('.FRP', '.cob'))
print ')'

print 'set (h_files'
for file in files:
	print '  ${CTOS_BINARY_DIR}/c/%s' % (file.replace('.FRP', '.h').lower())
print ')'


 
