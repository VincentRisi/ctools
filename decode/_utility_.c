#ifndef _utility_c_
#define _utility_c_

static int _differs_(char *a, char *b)
{
  int i, n=0;
  for (i=0; i<strlen(a) && n == 0; i++)
    n = toupper(a[i]) - toupper(b[i]);
  return n;
}

static char* _format_(char* work, int len, const char *fmt, ...)
{
  va_list  args;
  va_start(args, fmt);
  vsnprintf(work, len+1, fmt, args);
  va_end(args);
  return work;
}

static ctos_form* _getform_(INT *erc, CHAR* form)
{
  b20_form_block* fb = (b20_form_block*) form;
  if (strcmp(fb->signature, FORM_BLOCK_SIGNATURE) != 0)
  {
    *erc = ERC_FORM_IS_NOT_OPEN;
    return 0;
  }
  *erc = 0;
  return fb->form;
}

static void _getformfield_(INT *erc, ctos_form** cfrm, ctos_field** cfld, CHAR* form, CHAR* fieldname, INT fieldnamelen, INT index) 
{
  *cfrm = _getform_(erc, form);
  if (*erc != 0 || cfrm == 0) 
    return;
  WINDOW* win = _getwin_(form);
  char work[256];
  strncpy(work, (char*)fieldname, fieldnamelen);
  work[fieldnamelen] = 0;
  *cfld = ctos_form_getfield(*cfrm, work, index);
  if (*cfld == 0)
  {
    *erc = ERC_FIELD_NOT_IN_FORM;
    return;
  }
}

static WINDOW* _getwin_(CHAR* form)
{
  b20_form_block* fb = (b20_form_block*) form;
  return fb->win;
}

static void _setwin_(CHAR* form, WINDOW* win)
{
  b20_form_block* fb = (b20_form_block*) form;
  fb->win = win;
}

static int _showerror_(int error_rc, const char *fmt, ...)
{
  va_list  args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
  return error_rc;
}

#endif