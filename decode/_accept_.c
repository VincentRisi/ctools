#ifndef _accept_c_
#define _accept_c_

static void _accept_write_(INT* erc, WINDOW* win, int row, int col, CHAR* data, INT datalen, char attr, int color, int secret)
{
  int i;
  for (i=0; i<datalen; i++)
  {
    char ch = data[i];
    if (ch == 0)
      ch = ' ';
    else if (secret == 1)
      ch = '*';
    _attr_(win, attr, 1);
    _charout_(win, row, col+i, ch, color); 
    _attr_(win, attr, 0);
  }
  wrefresh(win);
  refresh();
}

static void _accept_edit_(INT* erc, WINDOW* win, int row, int col, CHAR* data, INT datalen, CHAR attr, int color, INT* key, CHAR *filter, int secret)
{
  int i, n, pos=0;
  CHAR code;
  _rtrim_(data, datalen);
  *key = 0;
  _curs_set_();
  while (1)
  {
    _accept_write_(erc, win, row, col, data, datalen, attr, color, secret);
    if (*erc != 0) return;
    wmove(win, row, col+pos);  
    code = _get_code_(erc, win);
    if (*erc != 0) return;
    if (code >= ' ' && code < 127)
    {
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
      break;
    default:
      _rpad_(data, datalen);
      _show_title_(_topwin_);
      _accept_write_(erc, win, row, col, data, datalen, attr, color, secret);
      char* p = strchr(filter, (int)code);
      if (p != 0)
        *key = (INT)((p - (char*)filter)+1);
      return;
    }
  }
} 

#endif