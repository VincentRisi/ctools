#include "b20forms.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <pthread.h>
#include "ctosio.h"

#define MAX_FILES         20
#define MAX_FRAMES        20
#define MAX_FILENAME_SIZE 256
#define MAX_FILTER_SIZE   16
#define MAX_NO_FIELDS     80
#define MAX_FILE_SIZE     256
#define B20CODES          "B20CODES"
#define B20SUBMIT         "_B20_SUBMIT_"
#define B20RECORD         "_B20_RECORD_"

#define DO_THREADING_OFF

#define DEBUG_LOGGING_ON
#ifndef DEBUG_LOGGING_ON
  #define LOG(x)
#else
  static FILE* _logfile_;
  
  static void _log_(char* line)
  {
    if (_logfile_ == 0)
      _logfile_ = fopen("b20forms.log", "at");
    if (_logfile_ == 0) return;
    fprintf(_logfile_, "%s\n", line);
    fflush(_logfile_);
  }
  
  static char _log_buffer_[4196];
  #define LOG(x) _log_(x);
#endif

#include "_globals_.c"
#include "_statics_.c"
#include "_mutex_.c"
#include "_show_clock_.c"
#include "_windowing_.c"
#include "_keyboard_.c"
#include "_accept_.c"
#include "_utility_.c"

void Accept(INT* erc, INT row, INT col, CHAR* data, INT datalen, INT color, CHAR attr, INT *key, CHAR* filter, INT secret)
{
  *erc = ERC_OK; 
  ACQUIRE();
  if (_started_ == 0) 
    _start_screen_();
  WINDOW* win = _mainwin_;
  int i;
  char work[128];
  for (i=0; i<strlen((char*)filter); i++)
    sprintf(work+(i*2), "%02x", filter[i]);
  _accept_edit_(erc, win, row, col, data, datalen, attr, color, key, filter, secret);
  refresh();
  RELEASE();
}

void AnyChar(INT* erc, CHAR *ch)
{
  int started = _started_;
  ACQUIRE();
  *erc = ERC_OK; 
  if (started == 0) _start_screen_();
  WINDOW* win = _mainwin_;
  *ch = _get_code_(erc, win);
  if (started == 0) _end_screen_();
  refresh();
  RELEASE();
}

void AnyCode(INT* erc, CHAR *ch)
{
  int started = _started_;
  ACQUIRE();
  *erc = ERC_OK; 
  if (started == 0) _start_screen_();
  CHAR code;
  WINDOW* win = _mainwin_;
  do {
    code = _get_code_(erc, win);
  } while (code >= ' ' && code < 127);    
  *ch = code;
  if (started == 0) _end_screen_();
  refresh();
  RELEASE();
}

void Clear()
{
  ACQUIRE();
  _clear_();
  refresh();
  RELEASE();
}

void CloseFile(INT* erc, INT file)
{ 
  ACQUIRE();
  if (file >= MAX_FILES || file < 0)
  {
    *erc = ERC_FILE_HANDLE_OUT_OF_RANGE;
    goto goback;
  }
  if (_files_used_[file] == 1)
  {
    fclose(_fh_[file]);
    _fh_[file] = 0;
    _files_used_[file] = 0;
    *erc = ERC_OK; 
  }
  else
    *erc = ERC_FILE_ALREADY_CLOSED;
  goback: RELEASE();  
}

void CloseForm(INT* erc, CHAR* form)
{
  b20_form_block* fb = (b20_form_block*) form;
  ACQUIRE();
  if (strcmp(fb->signature, FORM_BLOCK_SIGNATURE) == 0)
  {
    strcpy(fb->signature, "");
    strcpy(fb->formname, "");
    ctos_free_form(fb->form);
    fb->form = 0;
    _clear_();
    refresh();
  }
  *erc = ERC_OK; 
  RELEASE();  
}

void DateMailCo(CHAR* title, INT titlelen)
{
  int rc, len;
  CHAR w_title[41];
  ACQUIRE();
  if (_started_ == 0) 
    _start_screen_();
  len = titlelen > 40 ? 40 : titlelen;
#ifdef DO_THREADING_ON
  memcpy(_thread_data_.title, title, len);
  _thread_data_.draw = true;
  RELEASE();
  if (_thread_ == 0)
  {
    rc = pthread_attr_init(&_thread_attr_);
    rc = pthread_attr_setstacksize(&_thread_attr_, 4096);
    rc = pthread_create(&_thread_, &_thread_attr_, (void*) _show_clock_, (void*) &_thread_data_);
  }
#else
  memcpy(w_title, title, len);
  w_title[len] = 0;
  _show_header_(_topwin_, w_title);
#endif  
}

void Defaultfield(INT* erc, CHAR* form, CHAR* fieldname, INT fieldnamelen, INT index) 
{ 
  *erc = ERC_OK; 
  if (_started_ == 0) 
    _start_screen_();
  ctos_form* cfrm;
  ctos_field* cfld;
  _getformfield_(erc, &cfrm, &cfld, form, fieldname, fieldnamelen, index);
  if (*erc != 0) 
    goto goback;
  WINDOW* win = _getwin_(form);
  _defaultField_(erc, win, cfrm, cfld);
  goback: return;  
}

void Defaultform(INT* erc, CHAR* form) 
{ 
  int i;
  *erc = ERC_OK; 
  if (_started_ == 0) 
    _start_screen_();
  ctos_form* cfrm = _getform_(erc, form);
  if (*erc != 0 || cfrm == 0) 
    goto goback;
  WINDOW* win = _getwin_(form);
  _lineout_(win, cfrm);
  wrefresh(win);
  for (i = 0; i < cfrm->no_fields; i++)
    _defaultField_(erc, win, cfrm, &cfrm->fields[i]);
  goback: return;  
}

void Display(INT* erc, INT row, INT col, CHAR* data, INT datalen, INT color, CHAR attr, INT secret)
{
  *erc = ERC_OK; 
  ACQUIRE();
  if (_started_ == 0) 
    _start_screen_();
  WINDOW* win = _mainwin_;
  _accept_write_(erc, win, row, col, data, datalen, attr, color, secret);
  RELEASE();
}

void Displayform(INT* erc, CHAR* form, INT frame, INT column, INT line) 
{ 
  ACQUIRE();
  *erc = ERC_OK; 
  if (_started_ == 0) 
    _start_screen_();
  ctos_form* cfrm = _getform_(erc, form);
  if (*erc != 0 || cfrm == 0) 
    goto goback;
  if (_started_ == 0) 
    _start_screen_();
  WINDOW *win = _mainwin_;
  if (frame != 0)
    win = newwin(cfrm->height, cfrm->width, line, column);
  _setwin_(form, win);
  Defaultform(erc, form);
  *erc = ERC_OK; 
  goback: RELEASE();  
}

void GetFieldInfo(INT* erc, CHAR* form, CHAR* fieldname, INT fieldnamelen, FIELDINFO* fieldinfo, INT fieldinfomax) 
{ 
  int max;
  ctos_form* cfrm;
  ctos_field* cfld;
  ACQUIRE();
  _getformfield_(erc, &cfrm, &cfld, form, fieldname, fieldnamelen, 0);
  if (*erc != 0) 
    goto goback;
  if (fieldinfomax >=  2) fieldinfo->col = cfld->column;
  if (fieldinfomax >=  4) fieldinfo->line = cfld->row;
  if (fieldinfomax >=  6) fieldinfo->width = cfld->width;
  if (fieldinfomax >=  8) fieldinfo->showDefault = cfld->show_default;
  if (fieldinfomax >= 10) fieldinfo->autoExit = cfld->auto_exit;
  if (fieldinfomax >= 12) fieldinfo->repeating = cfld->repeating;
  if (fieldinfomax >= 14) fieldinfo->attrSel = cfld->selected;
  if (fieldinfomax >= 16) fieldinfo->attrUnsel = cfld->unselected;
  if (fieldinfomax >= 18) fieldinfo->indexFirst = cfld->first;
  if (fieldinfomax >= 20) fieldinfo->indexLast = cfld->last;
  if (fieldinfomax >= 30) memset(fieldinfo->reserved, 0, 10);
  if (fieldinfomax >= 32) fieldinfo->defaultLen = strlen(cfld->default_value);
  if (fieldinfomax >  32) 
  {
    int max = fieldinfomax - 32;
    if (strlen(cfld->default_value) < max)
      max = strlen(cfld->default_value);
    if (max > 80)
      max = 80;
    strncpy((char*)fieldinfo->defaultValue, cfld->default_value, max);
  }
  *erc = ERC_OK; 
  goback: RELEASE();  
} 

void LockKbd(INT* erc) 
{ 
  int started = _started_;
  ACQUIRE();
  *erc = ERC_OK; 
  if (started == 0) 
    _start_screen_();
  WINDOW* win = _mainwin_;
  CHAR ch;
  do
  {
    ch = _get_code_(erc, win);
    if (*erc != 0) 
      break; 
    if (ch != 0x07) 
      beep();
  } while(ch != 0x07);
  if (started == 0) _end_screen_();
  refresh();
  goback: RELEASE();  
}

void OpenFile(INT* erc, INT* file, CHAR* filename, INT filenamelen, CHAR* password, INT passwordlen, INT mode)
{ 
  int i;
  char work[MAX_FILENAME_SIZE];
  char openmode[3];
  ACQUIRE();
  if ((mode & 0xFF00) >> 8 == 'm') 
  {
    if ((mode & 0x00FF) == 'm')
      strcpy(openmode, "wb");
    else
      strcpy(openmode, "rb");
  }
  if (filenamelen >= MAX_FILENAME_SIZE)
  {
    *erc = ERC_FILE_NAME_TOO_LONG;
    goto goback;
  }
  *erc = ERC_NO_FILE_HANDLES;
  for (i = 0; i < MAX_FILES; i++)
  {
    if (_files_used_[i] == 0)
    {
      *file = i;
      *erc = ERC_OK;
      break;
    }
  }
  if (*erc != ERC_OK)
  {
    goto goback;
  }
  strncpy(work, (const char*)filename, filenamelen);
  work[filenamelen] = 0;
  _fh_[*file] = fopen(work, openmode);
  if (_fh_[*file] == 0)
  {
    *erc = ERC_FILE_OPEN_ERROR;
    goto goback;
  }
  _files_used_[*file] = 1;
  *erc = ERC_OK; 
  goback: RELEASE();  
}

void OpenForm(INT* erc, INT file, CHAR* formname, INT formnamelen, CHAR* form, INT max) 
{ 
  ACQUIRE();
  if (file >= MAX_FILES || file < 0)
  {
    *erc = ERC_FILE_HANDLE_OUT_OF_RANGE;
    goto goback;
  }
  if (_files_used_[file] != 1)
  {
    *erc = ERC_NO_FILE_IN_USE;
    goto goback;
  }
  b20_form_block* fb = (b20_form_block*) form;
  if (strcmp(fb->signature, FORM_BLOCK_SIGNATURE) == 0)
  {
    if (strlen(fb->formname) != formnamelen || _differs_(fb->formname, (char *)formname) != 0)
      CloseForm(erc, form);
    else
    { 
      *erc = ERC_OK;
      goto goback;
    }
  }
  strcpy(fb->signature, FORM_BLOCK_SIGNATURE);
  strncpy(fb->formname, (char*)formname, formnamelen);
  fb->formname[formnamelen] = 0;
  fb->form = ctos_load_form(_fh_[file], fb->formname);
  fb->win = 0;
  if (fb->form == 0)
  {
    *erc = ERC_NOT_A_VALID_FORM;
    goto goback;
  }
  *erc = ERC_OK; 
  goback: RELEASE();  
}

void ReadField(INT* erc, CHAR* form, CHAR* fieldname, INT fieldnamelen, INT index, CHAR* ret, INT max, INT* sizeret, TYPE* type) 
{ 
  int i, no;
  char work[256];
  ACQUIRE();
  *erc = ERC_OK; 
  ctos_form* cfrm;
  ctos_field* cfld;
  //LOG(_format_(_log_buffer_, sizeof(_log_buffer_), "ReadField(%d,%p,%s,%d,%d,%p,%d,%p,%p)", *erc, form, fieldname, fieldnamelen, index,ret,max,sizeret,type));
  _getformfield_(erc, &cfrm, &cfld, form, fieldname, fieldnamelen, index);
  strncpy(work, fieldname, fieldnamelen);work[fieldnamelen]=0;
  //LOG(_format_(_log_buffer_, sizeof(_log_buffer_), "ReadField: %s(%d)->%d", work, fieldnamelen, *erc));
  if (*erc != 0) 
    goto goback;
  CHAR* p = (CHAR*)cfrm->data + cfld->offset;
  no = cfld->width;
  if (max < no)
    no = max;
  for (i=0; i<no; i++)
    ret[i] = p[i];
  while(no > 0)
  {
    if (ret[no-1] != ' ' && ret[no-1] != 0)
      break;
    no--;  
  }
  *sizeret = (INT)no;
  //LOG(_format_(_log_buffer_, sizeof(_log_buffer_), "ReadField: p=%s(%d) ret=%s(%d)", p, cfld->width, ret, *sizeret));
  goback: RELEASE();  
}

void SetFieldAttrs(INT* erc, CHAR* form, CHAR* fieldname, INT fieldnamelen, INT index, CHAR attr) 
{ 
  ACQUIRE();
  *erc = ERC_OK; 
  ctos_form* cfrm;
  ctos_field* cfld;
  _getformfield_(erc, &cfrm, &cfld, form, fieldname, fieldnamelen, index);
  if (*erc != 0) 
    goto goback;
  if (attr >= 0 && attr < 16)  
    cfld->selected = 'A' + attr;
  else if (attr >= 'a' && attr <= 'p')  
    cfld->selected = attr - 0x20;
  else if (attr >= 'A' && attr <= 'P')  
    cfld->selected = attr;
  goback: RELEASE();  
}

void SetSysId(CHAR* sysid, INT sysidlen)
{
  int rc, len;
  CHAR w_sysid[21];
  if (_started_ == 0)
    _start_screen_();
  len = sysidlen > 20 ? 20 : sysidlen;
  memcpy(w_sysid, sysid, len);
  w_sysid[len] = 0;
  _set_sysid_(_topwin_, w_sysid);
}

void UndisplayForm(INT* erc, CHAR* form)
{ 
  ACQUIRE();
  *erc = ERC_OK;
  int i,r,c;
  ctos_form* cfrm = _getform_(erc, form);
  WINDOW* win = _getwin_(form);
  if (*erc != 0) 
    goto goback;
  for (i = 0; i < cfrm->data_size; i++)
    cfrm->data[i] = 0;  
  for (r = 0; r < cfrm->height; r++)
    for (c = 0; c < cfrm->width; c++)
      _charout_(win, r, c, ' ', _color_(cfrm->colors[cfrm->caption_color]));
  goback: RELEASE();  
}

void UserFillField(INT* erc, CHAR* form, CHAR* fieldname, INT fieldnamelen, INT index, INITSTATE* initstate, EXITSTATE* exitstate) 
{ 
  ACQUIRE();
  *erc = ERC_OK; 
  ctos_form* cfrm;
  ctos_field* cfld;
  INT pos;
  _getformfield_(erc, &cfrm, &cfld, form, fieldname, fieldnamelen, index);
  if (*erc != 0) 
    goto goback;
  WINDOW* win = _getwin_(form);
  _edit_field_(erc, win, cfrm, cfld, initstate, exitstate);
  goback: RELEASE();  
}

void WriteField(INT* erc, CHAR* form, CHAR* fieldname, INT fieldnamelen, INT index, CHAR* data, INT datalen, TYPE* type) 
{
  ACQUIRE();
  *erc = ERC_OK; 
  ctos_form* cfrm;
  ctos_field* cfld;
  WINDOW* win = _getwin_(form);
  _getformfield_(erc, &cfrm, &cfld, form, fieldname, fieldnamelen, index);
  if (*erc != 0) 
    goto goback;
  _write_field_(erc, win, cfrm, cfld, data, datalen, 0);
  goback: RELEASE();  
}

extern void WriteAll(INT* erc, CHAR* form, CHAR* block)
{
  int i;
  ACQUIRE();
  *erc = ERC_OK;
  ctos_field* cfld;
  ctos_form* cfrm;
  CHAR len;
  CHAR* data; 
  cfrm = _getform_(erc, form);
  WINDOW* win = _getwin_(form);
  for (i=0; i<cfrm->no_fields; i++)
  {
    cfld = &cfrm->fields[i];
    data = (CHAR*)block + cfld->offset;
    _write_field_(erc, win, cfrm, cfld, data, cfld->width, 0);
  }
  RELEASE();
}

extern void ReadAll(INT* erc, CHAR* form, CHAR* block)
{
  int i;
  ACQUIRE();
  *erc = ERC_OK;
  ctos_field* cfld;
  ctos_form* cfrm;
  CHAR len;
  CHAR* data; 
  cfrm = _getform_(erc, form);
  WINDOW* win = _getwin_(form);
  for (i=0; i<cfrm->no_fields; i++)
  {
    cfld = &cfrm->fields[i];
    data = (CHAR*)cfrm->data + cfld->offset;
    memcpy(block+cfld->offset, cfrm->data+cfld->offset, cfld->width);
  }
  RELEASE();
}

extern void WriteTo(INT* erc, CHAR* form, INT column, INT line, CHAR* data, INT datalen, INT color, CHAR attr)
{
  int i, pair;
  ctos_form* cfrm;
  ACQUIRE();
  *erc = ERC_OK;
  cfrm = _getform_(erc, form);
  WINDOW* win = _getwin_(form);
  pair = _color_(cfrm->colors[color]);
  for (i=0; i<datalen; i++)
  {
    char ch = data[i];
    if (ch == 0)
      ch = ' ';
    _attr_(win, attr, 1);
    _charout_(win, line, column+i, ch, pair); 
    _attr_(win, attr, 0);
  }
  RELEASE();
}

extern void ZoomBox(INT* erc)
{
  int status;
  pid_t pid, wpid;
  pid = fork();
  if (pid == 0)
  {
    *erc = 1;
    return;
  }
  *erc = 0;
  wpid = waitpid(pid, &status, 0);
  wrefresh(_topwin_);
  redrawwin(_topwin_);
  wrefresh(_mainwin_);
  redrawwin(_mainwin_);
}

extern void Clone(INT* erc)
{
  int status;
  pid_t pid, wpid;
  pid = fork();
  if (pid == 0)
  {
    *erc = 1;
    return;
  }
  *erc = 0;
  wpid = waitpid(pid, &status, 0);
  wrefresh(_topwin_);
  redrawwin(_topwin_);
  wrefresh(_mainwin_);
  redrawwin(_mainwin_);
}

extern void Exec(INT* erc, CHAR* command, INT commandlen)
{
  int status;
  if (commandlen >= 512)
  {
    *erc = 155;
    return;
  }
  char work[512];
  strncpy(work, command, commandlen);
  work[commandlen] = 0;
  *erc = system(command);
  wrefresh(_topwin_);
  redrawwin(_topwin_);
  wrefresh(_mainwin_);
  redrawwin(_mainwin_);
}

#define SCREENDUMP "./tmp/screendump.dump"

extern void SaveScreen()
{
  int n = mkdir("./tmp", 0777);
  #ifdef USE_SCR_DUMP
  WINDOW *save_curscr = curscr;
  curscr = _mainwin_;
  scr_dump(SCREENDUMP);
  curscr = save_curscr;
  #else
  FILE* outf = fopen(SCREENDUMP, "wb");
  putwin(_mainwin_, outf);
  fclose(outf);
  #endif
  _screensaved = 1;
  wrefresh(_topwin_);
  redrawwin(_topwin_);
  wrefresh(_mainwin_);
  redrawwin(_mainwin_);
}

extern void RestoreScreen()
{
  #ifdef USE_SCR_DUMP
  WINDOW *save_curscr;
  if (_screensaved == 0)
    return;
  save_curscr = curscr;
  curscr = _mainwin_;
  scr_restore(SCREENDUMP);
  doupdate();
  curscr = save_curscr;
  #else
  FILE* inpf = fopen(SCREENDUMP, "rb");
  if (_screensaved == 0)
    return;
  _mainwin_ = getwin(inpf);
  fclose(inpf);
  #endif
  _screensaved = 0;
  wrefresh(_topwin_);
  redrawwin(_topwin_);
  wrefresh(_mainwin_);
  redrawwin(_mainwin_);
}
