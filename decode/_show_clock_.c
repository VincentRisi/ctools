#ifndef _show_clock_c_
#define _show_clock_c_

static char _title_[41];
static void _show_header_(WINDOW* win, char* title)
{
  int i;
  CHAR none  = 'A';
  CHAR ch;
  for (i = 0; i < 79; i++)
  {
    ch = ' ';
    _attr_(win, none, 1);       
    _charout_(win, 0, i, ch, COLOR_GREEN); 
    _attr_(win, none, 0);       
  }
  for (i = 0; i < 79; i++)
  {
    _attr_(win, none, 1);       
    _charout_(win, 1, i, ACS_HLINE, COLOR_YELLOW); 
    _attr_(win, none, 0);       
  }
  wrefresh(win);
  strncpy(_title_, title, 40);  
  _show_title_(win);
  _show_sysid_(win);
}

static char _sysid_[21];
static void _set_sysid_(WINDOW* win, char* sysid)
{
  strncpy(_sysid_, sysid, 20);
  _show_sysid_(win);
}

static void _show_sysid_(WINDOW* win)
{
  int i;
  CHAR none  = 'A';
  CHAR ch;
  wrefresh(win);
  for (i = 0; i < 20; i++)
  {
    _attr_(win, none, 1);       
    _charout_(win, 1, i+5, ACS_HLINE, COLOR_YELLOW); 
    _attr_(win, none, 0);       
  }
  for (i = 0; i < strlen(_sysid_); i++)
  {
    ch = _sysid_[i];
    _attr_(win, none, 1);
    _charout_(win, 1, i+5, ch, COLOR_YELLOW); 
    _attr_(win, none, 0);
  }
}

static void _show_title_(WINDOW* win)
{
  int len, i, beg, pair;
  CHAR reverse = 'F';
  CHAR none  = 'A';
  CHAR ch;
  len = strlen(_title_);
  beg = (50 - len) / 2;
  pair = 2;
  for (i = 0; i < 58; i++)
  {
    ch = ' ';
    _attr_(win, none, 1);       
    _charout_(win, 0, i, ch, COLOR_GREEN); 
    _attr_(win, none, 0);       
  }
  for (i = 0; i < len; i++)
  {
    ch = _title_[i];
    _attr_(win, reverse, 1);       
    _charout_(win, 0, 10+beg+i, ch, COLOR_GREEN); 
    _attr_(win, reverse, 0);       
  }
  wrefresh(win);
  _show_time_(win);
  _show_sysid_(win);
}

static time_t _now_;
static void _show_time_(WINDOW* win)
{
  int i;
  time_t now;
  char timeline[40];
  char reformat[40];
  CHAR none  = 'A';
  CHAR ch;
  now = time(&now);
  _now_ = now;  
  ctime_r(&now, timeline);
  // Mon Jul 16 02:03:55 1987
  // 012345678901234567890123
  //           1         2
  strncpy(reformat+0, timeline, 4);
  strncpy(reformat+4, timeline+8, 3);
  strncpy(reformat+7, timeline+4, 4);
  strncpy(reformat+11, timeline+20, 4);
  reformat[15] = ' ';
  strncpy(reformat+16, timeline+11, 5);
  reformat[21] = 0;
  for (i = 0; i < 21; i++)
  {
    ch = reformat[i];
    _attr_(win, none, 1);       
    _charout_(win, 0, 58+i, ch, COLOR_YELLOW); 
    _attr_(win, none, 0);       
  }
  wrefresh(win);
}

static void _show_clock_(void* arg)
{
  int len, i, beg, end, pair;
  time_t now;
  char timeline[40];
  CHAR reverse = 'C';
  CHAR none  = 'A';
  CHAR ch;
  //b20_thread_data *data = (b20_thread_data *) arg;
  WINDOW* win = _topwin_;
  for (i = 0; i < 79; i++)
  {
    ch = ' ';
    _attr_(win, none, 1);       
    _charout_(win, 0, i, ch, COLOR_CYAN); 
    _attr_(win, none, 0);       
  }
  for (i = 0; i < 79; i++)
  {
    _attr_(win, none, 1);       
    _charout_(win, 1, i, ACS_HLINE, COLOR_YELLOW); 
    _attr_(win, none, 0);       
  }
  while (true)
  {
    ACQUIRE();
    now = time(&now);
    ctime_r(&now, timeline);
    if (_thread_data_.draw == true)
    {
      len = strlen(_thread_data_.title);
      beg = (40 - len) / 2;
      end = beg + len;
      pair = 2;
      for (i = 0; i < 55; i++)
      {
        ch = ' ';
       _attr_(win, none, 1);       
       _charout_(win, 0, i, ch, COLOR_CYAN); 
       _attr_(win, none, 0);       
      }
      for (i = 0; i < len; i++)
      {
        ch = _thread_data_.title[i];
        _attr_(win, reverse, 1);       
        _charout_(win, 0, 10+beg+i, ch, COLOR_CYAN); 
        _attr_(win, reverse, 0);       
      }
      _thread_data_.draw = false;
    }
    for (i = 0; i < strlen(timeline); i++)
    {
      ch = timeline[i];
      _attr_(win, none, 1);       
      _charout_(win, 0, 55+i, ch, COLOR_GREEN); 
      _attr_(win, none, 0);       
    }
    wrefresh(win);
    RELEASE();
    sleep(1);
  }
}

#endif
