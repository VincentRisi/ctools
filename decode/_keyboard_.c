#ifndef _keyboard_c_
#define _keyboard_c_

static CHAR _restore_(CHAR result)
{

  cbreak();
  return result;
}

static CHAR _code_sequence_(INT *erc, WINDOW* win, CHAR * filter)
{
  int ch, ch2, no;
  while (1)
  {
    halfdelay(1);
    noecho();
    do {
      ch = wgetch(win);
      RELEASE();
      ACQUIRE();
      wrefresh(win);
    } while (ch == ERR);
    nocbreak(); 
    if (ch >= ' ' && ch < 127)
      return _restore_((unsigned char)ch);  
    memset(filter, 0, MAX_FILTER_SIZE);
    if (ch == 27)
    {
      no = 0;
      filter[no++] = ch;
      halfdelay(1);
      ch2 = wgetch(win);
      if (ch2 != ERR)
      {
        do {
          if (no < MAX_FILTER_SIZE)
            filter[no++] = ch2;
          ch2 = wgetch(win);
        } while (ch2 != ERR);
        nocbreak(); 
      }
      return _restore_(0);
    }
    filter[0] = ch;   
    return _restore_(0);
  }
}

static void _curs_set_()
{
  int i, pair;
  char attr = 'A';
  char work[4]; 
  WINDOW* win = _topwin_;
  if (_insert_mode_ == 0)
    strcpy(work, "OVR");
  else
    strcpy(work, "INS"); 
  for (i=0; i<strlen(work); i++)
  {
    _attr_(win, attr, 1);
    _charout_(win, 1, i+1, work[i], COLOR_YELLOW); 
    _attr_(win, attr, 0);
  }
  wrefresh(win);
  refresh();
  curs_set(_insert_mode_ ? 2 : 1);    
}

static CHAR _record_(CHAR code)
{
  if (_recorder_ != 0)
  {
    if (code == '\n')
      putc((int)code, _recorder_);
    else if (code == '#' || code == '$' || code == '<')
    {
      putc((int)code, _recorder_);
      putc((int)code, _recorder_);
    }
    else if (code >= 0x20 && code < 0x7F)
      putc((int)code, _recorder_);
    else 
      fprintf(_recorder_, "#%02X\n", code);
    fflush(_recorder_);
  }
  return code;
}

static CHAR _get_code_(INT* erc, WINDOW* win)
{
  int i;
  CHAR code;
  CHAR filter[MAX_FILTER_SIZE];  
  if (_no_fields_ == 0)
  {
    _load_fields_(erc);
    if (*erc != 0) return _record_(0xFF);
  }
  if (_submit_chars_index_ < _submit_chars_len_)
  {
    code = (CHAR)_submit_chars_[_submit_chars_index_];
    _submit_chars_index_++; 
    return _record_(code);
  }
  while (1)
  {  
    code = _code_sequence_(erc, win, filter);
    if (*erc != 0) return _record_(0xFF);
    if (code != 0)
      return _record_(code);
    if (strlen(filter) == 0)
      continue;
    for (i = 0; i < _no_fields_; i++)
    {
      if (strcmp(_field_[i].filter, filter) == 0)
        return _record_(_field_[i].code);      
    }  
  }  
}

static void _edit_field_(INT* erc, WINDOW* win, ctos_form* cfrm, ctos_field* cfld, INITSTATE* initstate, EXITSTATE* exitstate)
{
  int i, n, pos=initstate->ich;
  CHAR code;
  int row = cfld->row;
  int col = cfld->column; 
  CHAR* data = (CHAR*)cfrm->data + cfld->offset;
  INT datalen = cfld->width; 
  int auto_term = cfld->auto_exit == 'Y' ? 1 : 0;
  memset(exitstate, 0, sizeof(EXITSTATE));
  _curs_set_();
  if (cfld->justification == 'R')
    _ltrim_(data, datalen);
  else  
    _rtrim_(data, datalen);
  exitstate->empty = (INT) (strlen((char*)data) == 0);
  while (1)
  {
    _write_field_(erc, win, cfrm, cfld, data, datalen, 1);
    if (*erc != 0) return;
    wmove(win, row, col+pos);  
    code = _get_code_(erc, win);
    if (*erc != 0) return;
    if (code >= ' ' && code < 127)
    {
      exitstate->modified = -1;
      exitstate->empty = 0;
      n = datalen - 1;
      if (_insert_mode_ == 1)
      {
        if (data[n] >= ' ' && data[n] < 127)
        {
          beep();
          continue;          
        }        
        for (i=n; i>=pos; i--)
          data[i] = data[i-1];
      }
      data[pos] = (char)code;
      if (pos < datalen)
        pos++;
      if (auto_term != 0 && data[n] != 0 && pos >= n)
      {
        _write_field_(erc, win, cfrm, cfld, data, datalen, 1);
        if (*erc != 0) return;
        exitstate->autoExit = -1;
        exitstate->ch = 0x12;
        exitstate->ich = pos;
        if (cfld->justification == 'R')
          _lpad_(data, datalen);
        else  
          _rpad_(data, datalen);
        _show_title_(_topwin_);
        _write_field_(erc, win, cfrm, cfld, data, datalen, 0);
        return;
      }        
    }
    else switch (code)
    {
    case 0x08: // backspace
      if (_insert_mode_ == 1)
      {
        if (pos == 0)
          break;
        for (i = pos; i < datalen; i++)
          data[i-1] = data[i];
        data[datalen-1] = 0;
        pos--;        
        break;
      }
      // fallthru and treat it like a left arrow
    case 0x0e: // left
      if (pos > 0)
        pos--;
      break;
    case 0x12: // right
      if (pos < datalen)
        if (data[pos] >= ' ' && data[pos] < 127)
          pos++;
      break;
    case 0xf1: // insert toggle
      _insert_mode_ = _insert_mode_ == 1 ? 0 : 1;
      _curs_set_();
      break;
    case 0xf2: // delete
      if (pos < datalen)
      {
        for (i = pos+1; i < datalen; i++)
          data[i-1] = data[i];
        data[datalen-1] = 0;
      }
      break;
    case 0xf3: // code left
      pos = 0;
      break;
    case 0xf4: // code right
      for (pos = 0; pos < datalen && data[pos] != 0; pos++)
        ;
      break;
    case 0xfe: // code backspace
    case 0xff: // code delete
      pos = 0;
      for (i=0; i<datalen; i++)
        data[i] = 0;
      exitstate->empty = -1;
      break;
    default:
      if (cfld->justification == 'R')
        _lpad_(data, datalen);
      else  
        _rpad_(data, datalen);
      _show_title_(_topwin_);
      _write_field_(erc, win, cfrm, cfld, data, datalen, 0);
      exitstate->ch = code;
      return;
    }
  }
} 

static void _check_record_submit_(INT *erc)
{
  char *RECORDER;
  char *SUBMITTER;
  int size;
  RECORDER = getenv(B20RECORD);
  LOG(_format_(_log_buffer_, sizeof(_log_buffer_), "Record: %s=%s", B20RECORD, RECORDER));
  SUBMITTER = getenv(B20SUBMIT);
  LOG(_format_(_log_buffer_, sizeof(_log_buffer_), "Submit: %s=%s", B20SUBMIT, SUBMITTER));
  if (RECORDER != 0)
  {
    _recorder_ = fopen(RECORDER, "wb");
    if (_recorder_ == 0)
    {
      *erc = ERC_B20RECORD_FILE_CANNOT_BE_OPENED;
      return;
    }
  }
  if (SUBMITTER != 0)
  {
    FILE *submitfile = fopen(SUBMITTER, "rb");
    if (submitfile == 0)
    {
      *erc = ERC_B20SUBMIT_FILE_CANNOT_BE_OPENED;
      return;
    }
    fseek(submitfile, 0l, SEEK_END);
    _submit_chars_len_ = (int)ftell(submitfile);
    fseek(submitfile, 0l, SEEK_SET);
    if (_submit_chars_len_ > 0)
    {
      _submit_chars_ = (unsigned char *) malloc(_submit_chars_len_);
      size = fread(_submit_chars_, 1, _submit_chars_len_, submitfile);
      fclose(submitfile);
      unlink(SUBMITTER);
      if (size != _submit_chars_len_)
      {
        *erc = ERC_B20SUBMIT_INVALID_DATA;
        return;
      }
    }
  }
}

static void _load_fields_(INT *erc)
{
  char *TERM;
  char signature[sizeof(B20CODES)];
  unsigned char code;
  unsigned char filter[MAX_FILTER_SIZE];
  int size;
  TERM = getenv(B20CODES);
  LOG(_format_(_log_buffer_, sizeof(_log_buffer_), "Term: %s=%s", B20CODES, TERM));
  if (TERM == 0)
  {
    *erc = ERC_B20CODES_ENVVAR_MISSING;
    return;
  }
  _check_record_submit_(erc);
  FILE* codefile = fopen(TERM, "rb");
  if (codefile == 0)
  {
    *erc = ERC_B20CODES_FILE_CANNOT_BE_OPENED;
    return;
  }
  fread(signature, sizeof(B20CODES), 1, codefile);
  if (strcmp(signature, B20CODES) != 0)
  {
    *erc = ERC_B20CODES_INVALID_SIGNATURE;
    return;
  }
  while (_no_fields_ < MAX_NO_FIELDS)
  {
    size = fread(&code, 1, 1, codefile);
    if (size == 0)
      return;
    size = fread(filter, 1, MAX_FILTER_SIZE, codefile);
    if (size != MAX_FILTER_SIZE)
    {
      *erc = ERC_B20CODES_INVALID_DATA;
      return;
    }
    _field_[_no_fields_].code = code;
    memcpy(_field_[_no_fields_].filter, filter, MAX_FILTER_SIZE);
    _no_fields_++;
  }
}

static void _lpad_(CHAR* data, INT datalen)
{
  int len;
  _rtrim_(data, datalen);
  len = strlen((char*)data);
  if (len < datalen)
  {
    memcpy((char*)data + datalen - len, (char*)data, len);
    memset(data, ' ', datalen - len);
  }
}

static void _ltrim_(CHAR* data, INT datalen)
{
  while (*data == ' ' || *data == '\t')
  {
    memcpy((char*)data, (char*)data+1, datalen-1);
    data[datalen-1] = 0;
  }
  _rtrim_(data, datalen);
}

static void _rpad_(CHAR* data, INT datalen)
{
  int len;
  len = strlen((char*)data);
  if (len < datalen)
    memset(data + len, ' ', datalen - len);
}

static void _rtrim_(CHAR* data, INT datalen)
{
  int i;
  for (i=datalen-1; i >= 0 && data[i] == ' '; i--)
    data[i] = 0;
}

#endif