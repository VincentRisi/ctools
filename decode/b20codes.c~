#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <curses.h>
#include <ctype.h>

#define MAX_FILTER_SIZE 16
#define SIGNATURE "B20CODES"

int get_code_sequence(char * filter)
{
  int ch, ch2, no=0;
  cbreak();
  ch = getch();
  if (ch >= ' ' && ch < 127)
    return ch;  
  memset(filter, 0, MAX_FILTER_SIZE);
  if (ch == 27)
  {
    filter[no++] = ch;
    halfdelay(1);
    ch2 = getch();
    if (ch2 != ERR)
    {
      do {
        filter[no++] = ch2;
        ch2 = getch();
      } while (ch2 != ERR || no >= MAX_FILTER_SIZE);
      nocbreak(); 
    }
    return 0;
  }
  filter[0] = ch;   
  return 0;
}

typedef struct Field
{
  unsigned char code;
  char * name;
  unsigned char filter[16];
  int row, col;
} Field;

static int no_fields;
static Field field[100];

void add_field(unsigned char code, char* name)
{
  field[no_fields].code = code;
  field[no_fields].name = name;
  memset(field[no_fields].filter, 0, 16);
  int row = field[no_fields].row = no_fields / 2;
  int col = field[no_fields].col = no_fields % 2 * 40;
  mvprintw(row, col, "%02X=%s ", code, name); 
  no_fields++;
}

int main(int argc, char *argv[])
{
  int i,j;
  int row;
  char signature[sizeof(SIGNATURE)];
  FILE* codefile;
  if (argc < 2)
  {
    printf("usage: %s codefile\n", argv[0]);
    return 1;
  }
  codefile = fopen(argv[1], "r+b");
  if (codefile == 0)
    codefile = fopen(argv[1], "wb");
  if (codefile == 0)
  {
    printf("codefile %s failed to open\n", argv[1]);
    return 1;
  }
  initscr();
  cbreak();
  noecho();
  refresh();
  add_field(0x00, "HELP        (A-h)");
  add_field(0x01, "UP          (up)");
  add_field(0x02, "MARK        (home)");
  add_field(0x04, "FINISH      (end)");
  add_field(0x05, "PREV-PAGE   (pgup)");
  add_field(0x07, "CANCEL      (esc)");
  add_field(0x08, "BACKSPACE   (backspace)");
  add_field(0x09, "TAB         (tab)");
  add_field(0x0a, "RETURN|NEXT (enter)");
  add_field(0x0b, "DOWN        (down)");
  add_field(0x0c, "NEXT-PAGE   (pgdn)");
  add_field(0x0d, "BOUND       (A-home)");
  add_field(0x0e, "LEFT        (left)");
  add_field(0x0f, "MOVE        (A-m)");
  add_field(0x11, "SCROLL-UP   (F12)");
  add_field(0x12, "RIGHT       (right)");
  add_field(0x13, "SCROLL-DN   (F11)");
  add_field(0x14, "COPY        (A-c)");
  add_field(0x15, "F1          (F1)");
  add_field(0x16, "F2          (F2)");
  add_field(0x17, "F3          (F3)");
  add_field(0x18, "F4          (F4)");
  add_field(0x19, "F5          (F5)");
  add_field(0x1a, "F6          (F6)");
  add_field(0x1b, "GO          (L-A-Return)");
  add_field(0x1b, "GO          (R-A-Return)");
  add_field(0x1c, "F7          (F7)");
  add_field(0x1d, "F8          (F8)");
  add_field(0x1e, "F9          (F9)");
  add_field(0x1f, "F10         (F10)");
  add_field(0x89, "CODE-TAB    (A-tab)");
  add_field(0x9d, "CODE-TAB    (A-F8)");
  add_field(0x8f, "CODE-MOVE   (A-S-M)");
  add_field(0x95, "CODE-F1     (A-F1)");
  add_field(0x96, "CODE-F2     (A-F2)");
  add_field(0x97, "CODE-F3     (A-F3)");
  add_field(0x98, "CODE-F4     (A-F4)");
  add_field(0x99, "CODE-F5     (A-F5)");
  add_field(0x9a, "CODE-F6     (A-F6)");
  add_field(0x9b, "CODE-GO     (A-g)");
  add_field(0xc7, "CODE-GO     (A-G)");
  add_field(0x9c, "CODE-F7     (A-F7)");
  add_field(0x9d, "CODE-F8     (A-F8)");
  add_field(0x9e, "CODE-F9     (A-F9)");
  add_field(0x9f, "CODE-F10    (A-F10)");
  add_field(0xd5, "SHIFT-F1    (S-F1)");
  add_field(0xd6, "CODE-SHIFT-V (A-S-V)");
  add_field(0xd6, "SHIFT-F2    (S-F2)");
  add_field(0xd7, "SHIFT-F3    (S-F3)");
  add_field(0xd8, "SHIFT-F4    (S-F4)");
  add_field(0xd9, "SHIFT-F5    (S-F5)");
  add_field(0xda, "SHIFT-F6    (S-F6)");
  add_field(0xdb, "SHIFT-F7    (S-F7)");
  add_field(0xdd, "SHIFT-F8    (S-F8)");
  add_field(0xde, "SHIFT-F9    (S-F9)");
  add_field(0xdf, "SHIFT-F10   (S-F10)");
  add_field(0xe2, "CODE-B      (A-b)");
  add_field(0xe5, "CODE-E      (A-e)");
  add_field(0xf1, "ins-toggle  (ins)");
  add_field(0xf2, "del-char    (del)");
  add_field(0xf3, "CODE-LEFT   (A-left)");
  add_field(0xf4, "CODE-RIGHT  (A-right)");
  add_field(0xfe, "CODE-BSpace (A-bckspc)");
  add_field(0xff, "CODE-DELETE (A-del)");
  add_field(0x91, "CODE-SCR-UP (A-F12)");
  add_field(0x93, "CODE-SCR-DN (A-F11)");
  add_field(0x87, "CODE-CANCEL (A-Esc)");
  add_field(0x9f, "CODE-CANCEL (A-F10)");
  add_field(0x8a, "CODE-RETURN (A-R)");
  add_field(0x8b, "CODE-DOWN   (A-down)");
  add_field(0xfa, "CODE-z      (A-z)");
  add_field(0xda, "CODE-Z      (A-Z)");
  add_field(0x8C, "CODE-NEXT-PG (A-PgDn)");
  add_field(0x81, "SHIFT-TAB    (S-TAB)");
  add_field(0x85, "CODE-PREV-PG (A-PgUp)");
  add_field(0xf0, "CODE-p       (A-p)");
  add_field(0xd0, "CODE-P       (A-P)");
  //add_field(0x84, "UNUSED      (none)");
  //add_field(0x85, "UNUSED      (none)");
  //add_field(0x86, "UNUSED      (none)");
  //add_field(0x87, "UNUSED      (none)");
  //add_field(0x88, "UNUSED      (none)");
  //add_field(0x8a, "UNUSED      (none)");
  //add_field(0x8b, "UNUSED      (none)");
  //add_field(0x8d, "UNUSED      (none)");
  //add_field(0x8e, "UNUSED      (none)");
  //add_field(0x90, "UNUSED      (none)");
  //add_field(0x91, "UNUSED      (none)");
  //add_field(0x92, "UNUSED      (none)");
  //add_field(0x93, "UNUSED      (none)");
  //add_field(0x94, "UNUSED      (none)");
  //add_field(0xc0, "UNUSED      (none)");
  //add_field(0xc1, "UNUSED      (none)");
  //add_field(0xc2, "UNUSED      (none)");
  //add_field(0xc3, "UNUSED      (none)");
  //add_field(0xc4, "UNUSED      (none)");
  //add_field(0xc5, "UNUSED      (none)");
  //add_field(0xc6, "UNUSED      (none)");
  //add_field(0xc7, "UNUSED      (none)");
  //add_field(0xc8, "UNUSED      (none)");
  //add_field(0xc9, "UNUSED      (none)");
  //add_field(0xca, "UNUSED      (none)");
  //add_field(0xcb, "UNUSED      (none)");
  //add_field(0xcc, "UNUSED      (none)");
  //add_field(0xcd, "UNUSED      (none)");
  //add_field(0xce, "UNUSED      (none)");
  //add_field(0xcf, "UNUSED      (none)");
  //add_field(0xd0, "UNUSED      (none)");
  //add_field(0xd1, "UNUSED      (none)");
  //add_field(0xd2, "UNUSED      (none)");
  //add_field(0xd3, "UNUSED      (none)");
  //add_field(0xd4, "UNUSED      (none)");
  //add_field(0xe0, "UNUSED      (none)");
  //add_field(0xe1, "UNUSED      (none)");
  //add_field(0xe3, "UNUSED      (none)");
  //add_field(0xe4, "UNUSED      (none)");
  //add_field(0xe6, "UNUSED      (none)");
  //add_field(0xe7, "UNUSED      (none)");
  //add_field(0xe8, "UNUSED      (none)");
  //add_field(0xe9, "UNUSED      (none)");
  //add_field(0xea, "UNUSED      (none)");
  //add_field(0xeb, "UNUSED      (none)");
  //add_field(0xec, "UNUSED      (none)");
  //add_field(0xed, "UNUSED      (none)");
  //add_field(0xee, "UNUSED      (none)");
  //add_field(0xef, "UNUSED      (none)");
  //add_field(0xf5, "UNUSED      (none)");
  //add_field(0xf6, "UNUSED      (none)");
  //add_field(0xf7, "UNUSED      (none)");
  //add_field(0xf8, "UNUSED      (none)");
  //add_field(0xf9, "UNUSED      (none)");
  //add_field(0xfa, "UNUSED      (none)");
  //add_field(0xfb, "UNUSED      (none)");
  //add_field(0xfc, "UNUSED      (none)");
  fseek(codefile, 0, SEEK_SET);
  fread(signature, sizeof(SIGNATURE), 1, codefile);
  if (strcmp(signature, SIGNATURE) == 0)
  {
    for (i=0; i<no_fields; i++)
    {
      unsigned char code;
      unsigned char filter[16];
      size_t read;
      read = fread(&code, 1, 1, codefile);
      if (read == 0)
        break;
      if (code != field[i].code)
        break;  
      read = fread(filter, 1, MAX_FILTER_SIZE, codefile);
      if (read != MAX_FILTER_SIZE)
        break;
      memcpy(field[i].filter, filter, MAX_FILTER_SIZE);  
      mvprintw(field[i].row, field[i].col+26, ":");
      for (j=0; j<strlen(field[i].filter); j++)
        mvprintw(field[i].row, field[i].col+27+(j*2), "%02X", field[i].filter[j]); 
    }
  }
  row = field[no_fields-1].row+2;
  mvprintw(row, 1, "If F12 for GO does not suit you may choose A-G and AS-G instead.");
  mvprintw(row+1, 1, "Of course this all depends on the TERMCAP for the terminal in use.");
  mvprintw(row+2, 1, "Bear in minds the PC keys in parenthesis are merely suggestions.");
  mvprintw(row+3, 1, "(b:backward f:forward s:save ctrl-c cancel)");
  int ch;
  for (i=0; i<no_fields;)
  {
    mvprintw(field[i].row, field[i].col+26, ":");
    int ch = get_code_sequence(field[i].filter);
    if (ch == 0)
    {
      mvprintw(field[i].row, field[i].col+27, "%12s", " ");
      for (j=0; j<strlen(field[i].filter); j++)
        mvprintw(field[i].row, field[i].col+27+(j*2), "%02X", field[i].filter[j]); 
      i++;
      continue;
    }
    if (ch == 'b' && i > 0)
      i--;
    if (ch == 'f' && i < no_fields)
      i++;        
    if (ch == 's')
      break;    
  }
  endwin();
  fseek(codefile, 0, SEEK_SET);
  fwrite(SIGNATURE, sizeof(SIGNATURE), 1, codefile);
  for (i=0; i<no_fields; i++)
  {
    fwrite(&field[i].code, 1, 1, codefile);
    fwrite(field[i].filter, MAX_FILTER_SIZE, 1, codefile);
  }
  fclose(codefile);
  char work[256];
  FILE* textfile = fopen("b20codes.txt", "wt");
  for (i=0; i<no_fields; i++)
  {
    int p=0;
    p += sprintf(work+p, "%02x: %-24s:", field[i].code, field[i].name);
    for (j=0; j<MAX_FILTER_SIZE; j++)
    {
      char ch2 = field[i].filter[j];
      if (ch2 == 0)
        break;
      if (ch2 == 0x1b)
        p += sprintf(work+p, "\\e");
      else if (ch2 >= 0x20 && ch2 <= 0x7e)  
        p += sprintf(work+p, "%c", ch2);
      else  
        p += sprintf(work+p, "%02x", ch2);
    }
    fprintf(textfile, "%s\n", work);
  }
  fclose(textfile);
}
