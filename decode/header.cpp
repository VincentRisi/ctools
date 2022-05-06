#include "header.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <curses.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "getargs.h"

static char *title = "Christensen Tools (Pty) Ltd";
static void _attr_(WINDOW* win, char ch, int on);
static void _charout_(WINDOW* win, int row, int col, int ch, int pair);
static void _color_(int pair, char *color);
static void _fork_exec_(WINDOW* win, const char* command);

ARG argtab[] =
{ {'t', STRING, (int*) &title, "Main Title"}
};
#define TABSIZE (sizeof(argtab) / sizeof(ARG))

int main(int argc, char* argv[])
{
  argc = get_args(argc, argv, argtab, TABSIZE);
  if (argc < 2)
  {
    printf("usage: header <options> executable\n");
    printf("use -q (or any other invalid option) to get a list of options\n");
    return 1;
  }
  initscr();
  //cbreak();
  //noecho();
  refresh();
  WINDOW* win = newwin(2, 80, 0, 0);
  _fork_exec_(win, argv[1]);
  endwin();
  return 0;
}

static void _display_(WINDOW* win)
{
  int i, col, row;
  int yellow = 0;  
  int green = 1;  
  _color_(yellow, "110");
  _color_(green,  "010");
  col = (60 - strlen(title)) / 2;
  if (col < 0) col = 0;
  _attr_(win, 'A', 1);
  for (i = 0; i < strlen(title); i++)
    _charout_(win, 0, col+i, title[i], yellow);
  _attr_(win, 'A', 0);
  mvwhline(win, 1, 0, ACS_HLINE, 80);
  wrefresh(win);
}

static void _fork_exec_(WINDOW* win, const char* command)
{
  pid_t pid = fork();
  int status;
  int options = WNOHANG;
  char* null = 0;
  if (pid < 0)
  {
    endwin();
    printf("Fork failed\n");
    exit(1);
  }
  if (pid == 0)
    execlp(command, command, null);
  while (true)
  {  
    pid_t wpid = waitpid(pid, &status, options);
    if (WIFEXITED(status) == true) 
      break;
    _display_(win);  
    sleep(1);    
  }
}

#define HALF_BRIGHT 1
#define UNDERLINE   2
#define REVERSE     4
#define BLINKING    8

static void _attr_(WINDOW* win, char ch, int on)
{
  int m = ch - 'A';
  if (on != 0)
  {
    if (m & HALF_BRIGHT == HALF_BRIGHT)   wattron(win, A_DIM);
    if (m & UNDERLINE   == UNDERLINE)     wattron(win, A_UNDERLINE);
    if (m & REVERSE     == REVERSE)       wattron(win, A_REVERSE);
    if (m & BLINKING    == BLINKING)      wattron(win, A_BLINK);
  } else
  {
    if (m & HALF_BRIGHT == HALF_BRIGHT)   wattroff(win, A_DIM);
    if (m & UNDERLINE   == UNDERLINE)     wattroff(win, A_UNDERLINE);
    if (m & REVERSE     == REVERSE)       wattroff(win, A_REVERSE);
    if (m & BLINKING    == BLINKING)      wattroff(win, A_BLINK);
  }
}

static void _charout_(WINDOW* win, int row, int col, int ch, int color)
{
  wattron (win, COLOR_PAIR(color));
  mvwaddch(win, row, col, ch);
  wattroff(win, COLOR_PAIR(color));
}


static void _color_(int pair, char *color)
{
  int no = color[0] > '0' ? 1 : 0;
  no    |= color[1] > '0' ? 2 : 0; 
  no    |= color[2] > '0' ? 4 : 0;
  switch (no)
  {
  case 0: init_pair(pair, COLOR_BLACK,   COLOR_WHITE); break;
  case 1: init_pair(pair, COLOR_RED,     COLOR_BLACK); break;
  case 2: init_pair(pair, COLOR_GREEN,   COLOR_BLACK); break;
  case 3: init_pair(pair, COLOR_YELLOW,  COLOR_BLACK); break;
  case 4: init_pair(pair, COLOR_BLUE,    COLOR_BLACK); break;
  case 5: init_pair(pair, COLOR_MAGENTA, COLOR_BLACK); break;
  case 6: init_pair(pair, COLOR_CYAN,    COLOR_BLACK); break;
  case 7: init_pair(pair, COLOR_WHITE,   COLOR_BLACK); break;
  }
}

