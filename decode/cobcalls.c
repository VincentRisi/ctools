#include "b20forms.h"

#ifdef __cplusplus
extern "C" {
#endif

extern void ACCEPT(INT* erc, INT* row, INT* col, CHAR* data, CHAR* datalen, INT* color, CHAR* attr, CHAR* key, CHAR* filter)
{
  INT _key;
  Accept(erc, *row, *col, data, *datalen, *color, *attr, &_key, filter, 0);
  *key = (CHAR)_key;
}

extern void ACCEPTPWD(INT* erc, INT* row, INT* col, CHAR* data, CHAR* datalen, INT* color, CHAR* attr, CHAR* key, CHAR* filter)
{
  INT _key;
  Accept(erc, *row, *col, data, *datalen, *color, *attr, &_key, filter, 1);
  *key = (CHAR)_key;
}

extern void ANYCHAR(INT* erc, CHAR *ch)
{
  AnyChar(erc, ch);
}

extern void ANYCODE(INT* erc, CHAR *ch)
{
  AnyCode(erc, ch);
}

extern void CLOSEFILE(INT* erc, INT* file)
{
  CloseFile(erc, *file);
}

extern void CLOSEFORM(INT* erc, CHAR* form)
{
  CloseForm(erc, form);
}

extern void SETSYSID(CHAR* sysid)
{
  INT sysidlen;
  for (sysidlen = MAX_SYSID; sysidlen > 1; sysidlen--)
  {
    if (sysid[sysidlen-1] == ' ')
      continue;
    break;
  }
  CHAR work[MAX_SYSID+1];
  if (sysidlen > MAX_SYSID) sysidlen = MAX_SYSID;
  strncpy(work, sysid, sysidlen);
  SetSysId(work, sysidlen);
}

extern void DATEMAILCO(CHAR* company_name)
{
  INT companynamelen;
  for (companynamelen = MAX_TITLE; companynamelen > 1; companynamelen--)
  {
    if (company_name[companynamelen-1] == ' ')
      continue;
    break;  
  }
  CHAR work[43];
  if (companynamelen > 40) companynamelen = 40;
  strcpy(work, " ");
  strncat(work, company_name, companynamelen);
  strcat(work, " ");
  DateMailCo(work, companynamelen+2);
}

extern void DEFAULTFIELD(INT* erc, CHAR* form, CHAR* fieldname, CHAR* fieldnamelen, CHAR* index)
{
  Defaultfield(erc, form, fieldname, *fieldnamelen, *index);
}

extern void DEFAULTFORM(INT* erc, CHAR* form)
{
  Defaultform(erc, form);
}

extern void DISPLAY(INT* erc, INT* row, INT* col, CHAR* data, CHAR* datalen, INT* color, CHAR* attr)
{
  Display(erc, *row, *col, data, *datalen, *color, *attr, 0);
}

extern void DISPLAYPWD(INT* erc, INT* row, INT* col, CHAR* data, CHAR* datalen, INT* color, CHAR* attr)
{
  Display(erc, *row, *col, data, *datalen, *color, *attr, 1);
}

extern void DISPLAYFORM(INT* erc, CHAR* form, INT *frame, INT *column, INT *line)
{
  Displayform(erc, form, *frame, *column, *line);
}

extern void GETFIELDINFO(INT* erc, CHAR* form, CHAR* fieldname, CHAR* fieldnamelen, FIELDINFO* fieldinfo, CHAR* fieldinfomax)
{
  GetFieldInfo(erc, form, fieldname, *fieldnamelen, fieldinfo, *fieldinfomax);
}

extern void LOCKKBD(INT* erc)
{
  LockKbd(erc);
}

extern void OPENFILE(INT* erc, INT* file, CHAR* filename, INT* filenamelen, CHAR* password, INT* passwordlen, CHAR* mode)
{ 
  INT int_mode = (mode[0] << 8) | mode[1];
  OpenFile(erc, file, filename, *filenamelen, password, *passwordlen, int_mode);
}

extern void OPENFORM(INT* erc, INT* file, CHAR* formname, INT* formnamelen, CHAR* form, INT* max)
{
  OpenForm(erc, *file, formname, *formnamelen, form, *max);
}

extern void READALL(INT* erc, CHAR* form, CHAR* block)
{
  ReadAll(erc, form, block);
}

extern void READFIELD(INT* erc, CHAR* form, CHAR* fieldname, CHAR* fieldnamelen, CHAR* index, CHAR* ret, CHAR* max, INT* sizeret, TYPE* type)
{
  ReadField(erc, form, fieldname, *fieldnamelen, *index, ret, *max, sizeret, type);
}

extern void SETFIELDATTRS(INT* erc, CHAR* form, CHAR* fieldname, CHAR* fieldnamelen, CHAR* index, CHAR* attr)
{
  SetFieldAttrs(erc, form, fieldname, *fieldnamelen, *index, *attr);
}

extern void UNDISPLAYFORM(INT* erc, CHAR* form)
{
  UndisplayForm(erc, form);
}

extern void USERFILLFIELD(INT* erc, CHAR* form, CHAR* fieldname, CHAR* fieldnamelen, CHAR* index, INITSTATE* initstate, EXITSTATE* exitstate)
{
  UserFillField(erc, form, fieldname, *fieldnamelen, *index, initstate, exitstate);
}

extern void WRITEALL(INT* erc, CHAR* form, CHAR* block)
{
  WriteAll(erc, form, block);
}

extern void WRITEFIELD(INT* erc, CHAR* form, CHAR* fieldname, CHAR* fieldnamelen, CHAR* index, CHAR* data, CHAR* datalen, TYPE* type)
{
  WriteField(erc, form, fieldname, *fieldnamelen, *index, data, *datalen, type);
}

extern void WRITETO(INT* erc, CHAR* form, INT* column, INT* line, CHAR* data, INT* datalen, INT* color, CHAR* attr)
{
  WriteTo(erc, form, *column, *line, data, *datalen, *color, *attr);
}

extern void XE4()
{
  Clear();
}

extern void ZOOMBOX(INT* erc)
{
  ZoomBox(erc);
}

extern void CLONE(INT* erc)
{
  Clone(erc);
}
//INT* erc, CHAR* form, CHAR* fieldname, CHAR* fieldnamelen

extern void EXEC(INT* erc, CHAR* command, INT* commandlen)
{
  Exec(erc, command, *commandlen);
}

extern void SAVESCREEN()
{
  SaveScreen();
}

extern void RESTORESCREEN()
{
  RestoreScreen();
}

#ifdef __cplusplus
};
#endif
