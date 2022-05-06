#ifndef _windowing_c_
#define _windowing_c_

#define HALF_BRIGHT 1
#define UNDERLINE   2
#define REVERSE     4
#define BLINKING    8

static void _attr_(WINDOW* win, char ch, int on)
{
  int m = ch - 'A';
  attr_t attr = A_NORMAL;
  if ((m & HALF_BRIGHT) == HALF_BRIGHT) attr |= A_DIM;  
  if ((m & UNDERLINE)   == UNDERLINE)   attr |= A_UNDERLINE;
  if ((m & REVERSE)     == REVERSE)     attr |= A_REVERSE;
  if ((m & BLINKING)    == BLINKING)    attr |= A_BLINK;
  if (on != 0)
    wattrset(win, attr);
  else
    wattroff(win, attr);
}

static void _charout_(WINDOW* win, int row, int col, int ch, int pair)
{
  wattron (win, COLOR_PAIR(pair));
  mvwaddch(win, row, col, ch);
  wattroff(win, COLOR_PAIR(pair));
}

static void _clear_()
{
  wclear(_mainwin_);  
  wrefresh(_mainwin_);
  refresh();
}

static int _color_(char *color)
{
  int no = color[0] > '0' ? 1 : 0;
  no    |= color[1] > '0' ? 2 : 0; 
  no    |= color[2] > '0' ? 4 : 0;
  return no;
}

static void _defaultField_(INT* erc, WINDOW* win, ctos_form* cfrm, ctos_field* cfld)
{
  char work[256];
  memset(work, ' ', 256);
  if (cfld->show_default == 'Y')
  {
    if (cfld->default_value != 0)
      _write_field_(erc, win, cfrm, cfld, (CHAR*)cfld->default_value, (INT)strlen(cfld->default_value), 0);
    else  
      _write_field_(erc, win, cfrm, cfld, (CHAR*)work, (INT)strlen(work), 0);
  }
}

static void _end_screen_()
{
  if (_already_started_ == 0)
    _already_started_ = (stdscr != NULL) ? 1 : -1;
  delwin(_mainwin_);
  delwin(_topwin_);
  if (_already_started_ == -1)
    endwin();
  _mainwin_ = 0;
  _topwin_ = 0;
  _started_ = 0;
}

#define LC_UP    1
#define LC_DOWN  2
#define LC_LEFT  4
#define LC_RIGHT 8

static void _lineout_(WINDOW* win, ctos_form* cfrm)
{
  int col, row, height, width, i;
  int linetype = 0;
  int caption_color = _color_(cfrm->colors[cfrm->caption_color]);
  int line_color    = _color_(cfrm->colors[cfrm->line_color]);
  unsigned ch;
  int reverse = 0;
  CHAR half_bright = 'F';

  height = cfrm->height;
  width  = cfrm->width;
  for (row = 0; row < height; row++)
  {
    reverse = 0;
    for (col = 0; col < width; col++)
    {
      char ch = cfrm->lines[row][col]; 
      if (ch == '_')
        continue;
      if (ch == '{')
      {
        reverse = 1;
        continue;
      }      
      if (ch == '}')
      {
        reverse = 0;
        continue;
      }      
      if (ch != '*' || reverse == 1)
      {
        if (reverse == 1) _attr_(win, half_bright, 1);
        _charout_(win, row, col, ch, caption_color);
        if (reverse == 1) _attr_(win, half_bright, 0);
        continue;
      }
      linetype = 0;
      if (row > 0 && cfrm->lines[row-1][col] == '*')
        linetype |= LC_UP;
      if (row < (height-1) && cfrm->lines[row+1][col] == '*')
        linetype |= LC_DOWN;
      if (col > 0 && cfrm->lines[row][col-1] == '*')
        linetype |= LC_LEFT;
      if (col < (width-1)  && cfrm->lines[row][col+1] == '*')
        linetype |= LC_RIGHT;
      switch (linetype)
      {
      case 1:  
        _charout_(win, row, col, ACS_VLINE, line_color); 
        break;
      case 2:
        _charout_(win, row, col, ACS_VLINE, line_color); 
        break;
      case 3:  
        _charout_(win, row, col, ACS_VLINE, line_color); 
        break;
      case 4:  
        _charout_(win, row, col, ACS_HLINE, line_color); 
        break;
      case 5:  
        _charout_(win, row, col, ACS_LRCORNER, line_color); 
        break;
      case 6:  
        _charout_(win, row, col, ACS_URCORNER, line_color); 
        break;
      case 7:  
        _charout_(win, row, col, ACS_RTEE, line_color); 
        break;
      case 8:  
        _charout_(win, row, col, ACS_HLINE, line_color); 
        break;
      case 9:  
        _charout_(win, row, col, ACS_LLCORNER, line_color); 
        break;
      case 10: 
        _charout_(win, row, col, ACS_ULCORNER, line_color); 
        break;
      case 11: 
        _charout_(win, row, col, ACS_LTEE, line_color); 
        break;
      case 12: 
        _charout_(win, row, col, ACS_HLINE, line_color); 
        break;
      case 13: 
        _charout_(win, row, col, ACS_BTEE, line_color); 
        break;
      case 14:
        _charout_(win, row, col, ACS_TTEE, line_color); 
        break;
      case 15:
        _charout_(win, row, col, ACS_PLUS, line_color); 
        break;
      default: 
        _charout_(win, row, col, ACS_BLOCK, line_color); 
        break;
      }
    }
  }
  wrefresh(win);
  refresh();
}

static void _start_screen_()
{
  int i;
  if (_already_started_ == 0)
    _already_started_ = (stdscr != NULL) ? 1 : -1;
  if (_started_ == 0)
  {
    if (_already_started_ == -1)
      initscr();
    start_color();
    init_color(COLOR_RED, 1000, 0, 0);
    init_color(COLOR_GREEN, 0, 1000, 0);
    init_color(COLOR_BLUE, 0, 0, 1000);
    for (i = 0; i < 8; i++)
      init_pair(i, i, i > 0 ? 0 : 7);
    cbreak();
    noecho();
    refresh();
    _mainwin_ = newwin(32, 80, 2, 0);
    wrefresh(_mainwin_);
    _topwin_ = newwin(2, 80, 0, 0);
    wrefresh(_topwin_);
    _started_ = 1;
  }
}

static void _write_field_(INT* erc, WINDOW* win, ctos_form* cfrm, ctos_field* cfld, CHAR* data, INT datalen, int selected)
{
  int i, pair;
  char attr;
  CHAR* p = (CHAR*)cfrm->data + cfld->offset;
  pair = _color_(cfrm->colors[cfrm->caption_color]);
  if (selected != 0)
  {
    attr = cfld->selected;
    if (cfld->selected_color != 0)
      pair = _color_(cfrm->colors[cfld->selected_color]);
    else
      pair = 2;    
  }
  else
  {
    attr = cfld->unselected;
    if (cfld->unselected_color != 0)
      pair = _color_(cfrm->colors[cfld->unselected_color]);
    else
      pair = 2;    
  }
  if (p != data)
  {
    for (i = 0; i < datalen && i < cfld->width; i++)
    {
      if (i < datalen)
        p[i] = data[i];
      else
        p[i] = ' ';
    }
  }
  for (i=0; i<cfld->width; i++)
  {
    char ch = cfld->secret == 'Y' ? '*' : p[i];
    if (ch == 0)
      ch = ' ';
    _attr_(win, attr, 1);
    _charout_(win, cfld->row, cfld->column+i, ch, pair); 
    _attr_(win, attr, 0);
  }
  wrefresh(win);
  refresh();
}

#endif
