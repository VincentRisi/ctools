#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

#define B20SUBMIT         "_B20_SUBMIT_"
#define DEBUG_LOGGING_ON
#ifndef DEBUG_LOGGING_ON
  #define LOG(x)
#else
  static FILE* _logfile_;
  
  static void log_line(char* line)
  {
    if (_logfile_ == 0)
      _logfile_ = fopen("submit.log", "at");
    if (_logfile_ == 0) return;
    fprintf(_logfile_, "%s\n", line);
    fflush(_logfile_);
  }
  static char _log_buffer_[4096];
  #define LOG(x) log_line(x);
#endif

const char *error_message[] =
{ "All Cool"
, "Missing Program And Or Codes To Submit"
, "Program Is Not Executable"
, "Program Is Not Executing"
, "program Closed With Rc"
, "Cannot Open File"
, "Submit Invalid Data"
, "Code Is Not Valid Hex"
, "Code Is Not Valid Parm Token"
, "code Is Not Valid Symbol Token"
};

enum
{ ERC_OK
, ERC_MISSING_PROGRAM_CODES
, ERC_PROGRAM_NOT_EXECUTABLE
, ERC_PROGRAM_NOT_EXECUTING
, ERC_PROGRAM_CLOSED_WITH_RC
, ERC_CANNOT_OPEN_FILE
, ERC_SUBMIT_INVALID_DATA
, ERC_CODE_IS_NOT_VALID_HEX
, ERC_CODE_IS_NOT_VALID_PARM
, ERC_CODE_IS_NOT_VALID_SYMBOL
};

static char* format(char* work, int len, const char *fmt, ...)
{
  va_list  args;
  va_start(args, fmt);
  vsnprintf(work, len+1, fmt, args);
  va_end(args);
  return work;
}

static int show(int erc)
{
  printf("%s\n", error_message[erc]);
  return erc; 
}

static void store_char(int ch, FILE *outfile)
{
  char buffer[128];
  LOG(format(buffer, sizeof(buffer), "%02x", ch));
  fputc(ch, outfile);
}

static void _puts_(const char* s, FILE *outfile)
{
  const char* p;
  for (p=s; p[0] != 0; p++)
    store_char(p[0], outfile);
}

#define HEXCHARS "0123456789abcdefABCDEF"
#define NEWLINES "\r\n"

static void put_hex_code(const char* p, FILE *outfile)
{
  char work[3];
  work[0] = tolower(p[0]);
  work[1] = tolower(p[1]);
  work[2] = 0;
  unsigned char code;
  if (work[0] >= '0' && work[0] <= '9')
    work[0] -= '0';
  else 
    work[0] = (work[0] - 'a') + 10;  
  if (work[1] >= '0' && work[1] <= '9')
    work[1] -= '0';
  else 
    work[1] = (work[1] - 'a') + 10;  
  code = (unsigned char) (work[0] * 16 + work[1]);
  store_char(code, outfile);
}

static void put_parm_code(const char* p, int len, FILE *outfile, int argc, char* argv[])
{
  int no, i;
  char key[512];
  if (len == 1 && p[0] >= '0' && p[0] <= '9')
  {
    no = (p[0] - '0') + 3;
    if (no < argc)
      _puts_(argv[no], outfile);
    return;
  }
  strncpy(key, p, len);
  strcat(key, "=");
  for (i=3; i<argc; i++)
  {
    const char *arg = argv[i];
    if (strncmp(arg, key, len+1) == 0)
    {
      _puts_(arg+len, outfile);
      return;
    }
  }
}

typedef struct Symbol
{
  const char* key;
  int ch;
} Symbol;

const Symbol symbols[] =
{ {"1/2", 0x06}
, {"1/4", 0x10}
, {"ACK", 0x06}
, {"BACKSPACE", 0x08}
, {"BEL", 0x07}
, {"BOUND", 0x0D}
, {"BS", 0x08}
, {"CAN", 0x18}
, {"CANCEL", 0x07}
, {"CODE-1/2", 0x86}
, {"CODE-B", 0xE2}
, {"CODE-BACKSPACE", 0xFE}
, {"CODE-BOUND", 0x8D}
, {"CODE-BS", 0xFE}
, {"CODE-CANCEL", 0x87}
, {"CODE-COPY", 0x94}
, {"CODE-DELETE", 0xFF}
, {"CODE-DOWN", 0x8B}
, {"CODE-E", 0xE5}
, {"CODE-F1", 0x95}
, {"CODE-F10", 0x9F}
, {"CODE-F2", 0x96}
, {"CODE-F3", 0x97}
, {"CODE-F4", 0x98}
, {"CODE-F5", 0x99}
, {"CODE-F6", 0x9A}
, {"CODE-F7", 0x9C}
, {"CODE-F8", 0x9D}
, {"CODE-F9", 0x9E}
, {"CODE-FINISH", 0x84}
, {"CODE-GO", 0x9B}
, {"CODE-HELP", 0x80}
, {"CODE-LEFT", 0xF3}
, {"CODE-MARK", 0x82}
, {"CODE-MOVE", 0x8F}
, {"CODE-NEXT-PAGE", 0x8C}
, {"CODE-PREV-PAGE", 0x85}
, {"CODE-RETURN", 0x8A}
, {"CODE-RIGHT", 0xF4}
, {"CODE-SCR-DN", 0x93}
, {"CODE-SCR-UP", 0x91}
, {"CODE-SCROLL-DOWN", 0x93}
, {"CODE-SCROLL-UP", 0x91}
, {"CODE-SHIFT-1/2", 0x90}
, {"CODE-SHIFT-6", 0x83}
, {"CODE-SHIFT-V", 0xD6}
, {"CODE-TAB", 0x89}
, {"CODE-UP", 0x81}
, {"COPY", 0x14}
, {"CR", 0x0D}
, {"DC1", 0x11}
, {"DC2", 0x12}
, {"DC3", 0x13}
, {"DC4", 0x14}
, {"DEL", 0x7F}
, {"DEL-CHAR", 0xF2}
, {"DELETE", 0x7F}
, {"DLE", 0x10}
, {"DOWN", 0x0B}
, {"EM", 0x19}
, {"ENQ", 0x05}
, {"EOT", 0x04}
, {"ESC", 0x1B}
, {"ETB", 0x17}
, {"ETX", 0x03}
, {"F1", 0x15}
, {"F10", 0x1F}
, {"F2", 0x16}
, {"F3", 0x17}
, {"F4", 0x18}
, {"F5", 0x19}
, {"F6", 0x1A}
, {"F7", 0x1C}
, {"F8", 0x1D}
, {"F9", 0x1E}
, {"FINISH", 0x04}
, {"FS", 0x1C}
, {"GO", 0x1B}
, {"GS", 0x1D}
, {"HELP", 0x00}
, {"HT", 0x09}
, {"INS-TOGGLE", 0xF1}
, {"LEFT", 0x0E}
, {"MARK", 0x02}
, {"MOVE", 0x0F}
, {"NAK", 0x15}
, {"NEXT", 0x0A}
, {"NEXT-PAGE", 0x0C}
, {"NL", 0x0A}
, {"NP", 0x0C}
, {"NUL", 0x00}
, {"PREV-PAGE", 0x05}
, {"RETURN", 0x0A}
, {"RIGHT", 0x12}
, {"RS", 0x1E}
, {"SCROLL-DN", 0x13}
, {"SCROLL-DOWN", 0x13}
, {"SCROLL-UP", 0x11}
, {"SHIFT-1/2", 0x10}
, {"SHIFT-6", 0x03}
, {"SHIFT-F1", 0xD5}
, {"SHIFT-F10", 0xDF}
, {"SHIFT-F2", 0xD6}
, {"SHIFT-F3", 0xD7}
, {"SHIFT-F4", 0xD8}
, {"SHIFT-F5", 0xD9}
, {"SHIFT-F6", 0xDA}
, {"SHIFT-F7", 0xDC}
, {"SHIFT-F8", 0xDD}
, {"SHIFT-F9", 0xDE}
, {"SI", 0x0F}
, {"SO", 0x0E}
, {"SOH", 0x01}
, {"STX", 0x02}
, {"SUB", 0x1A}
, {"SYN", 0x16}
, {"TAB", 0x09}
, {"UP", 0x01}
, {"US", 0x1F}
, {"VT", 0x0B}
};
const int no_symbols = sizeof(symbols) / sizeof(Symbol);

static int compare_symbol(const void* a, const void* b)
{
  const Symbol* symbol_a = a;
  const Symbol* symbol_b = b;
  return strcmp(symbol_a->key, symbol_b->key);
}

static void put_symbol_code(const char* p, int len, FILE *outfile)
{
  int i;
  char key[512];
  Symbol lookup, *found;
  lookup.key = key;
  for (i=0; i < len && p[i] != 0; i++)
    key[i] = toupper(p[i]);
  key[i] = 0;
  found = bsearch(&lookup, symbols, no_symbols, sizeof(Symbol), compare_symbol);
  if (found != 0)
    store_char(found->ch, outfile);
}

static void translate(int *erc, FILE* infile, FILE* outfile, int argc, char* argv[])
{
  char *buffer, *bp;
  int bufferlen, size, no;
  fseek(infile, 0l, SEEK_END);
  bufferlen = (int)ftell(infile);
  fseek(infile, 0l, SEEK_SET);
  if (bufferlen == 0)
    return;
  buffer = calloc(bufferlen+1, 1);
  size = (int)fread(buffer, 1, bufferlen, infile);
  if (size != bufferlen)
  {
    *erc = ERC_SUBMIT_INVALID_DATA;
    return;
  }
  bp = buffer;
  while ((bp - buffer) < bufferlen)
  {
    if (bp[0] == '#' && bp[1] != '#')
    {
      bp++;
      no = strspn(bp, HEXCHARS);
      if (no != 2)
      {
        *erc = ERC_CODE_IS_NOT_VALID_HEX;
        return;
      }
      put_hex_code(bp, outfile);
      if (bp[2] == '\r' && bp[3] == '\n')
        bp += 4;
      else if (bp[2] == '\n')
        bp += 3;
    }
    else if (bp[0] == '$' && bp[1] != '$')
    {
      bp++;
      no = strcspn(bp, NEWLINES);
      if (no == strlen(bp))
      {
        *erc = ERC_CODE_IS_NOT_VALID_PARM;
        return;
      }
      put_parm_code(bp, no, outfile, argc, argv);
      if (bp[no] == '\r' && bp[no+1] == '\n')
        bp += (no+2);
      else if (bp[no] == '\n')
        bp += (no+1);
    }
    else if (bp[0] == '<' && bp[1] != '<')
    {
      bp++;
      no = strcspn(bp, NEWLINES">");
      if (no == strlen(bp))
      {
        *erc = ERC_CODE_IS_NOT_VALID_PARM;
        return;
      }
      put_symbol_code(bp, no, outfile);
      bp += (no);
      if (bp[0] == '>')
        bp++;
    }
    else
    {
      store_char(bp[0], outfile);
      bp++;
    }
    fflush(outfile);
  }
}

static void submit(int *erc, int argc, char* argv[])
{
  const char* program = argv[1];
  const char* submit_codes = argv[2];
  if (*erc != 0) return;  
  if (access(program, X_OK) != 0)
  {
    printf("Executable %s: ", program);
    *erc = ERC_PROGRAM_NOT_EXECUTABLE;
    return;
  }
  FILE* infile = fopen(submit_codes, "rb");
  if (infile == 0)
  {
    printf("Submit infile %s: ", submit_codes);
    *erc = ERC_CANNOT_OPEN_FILE;
    return;
  }
  char tempfile_work[512];
  const char* tempfile_name = tmpnam_r(tempfile_work);
  FILE* outfile = fopen(tempfile_name, "wb");
  if (outfile == 0)
  {
    printf("Codes outfile %s: ", tempfile_name);
    *erc = ERC_CANNOT_OPEN_FILE;
    return;
  }
  translate(erc, infile, outfile, argc, argv);
  fclose(infile);
  fclose(outfile);
  int rc = 0;
  char work[512];
  rc = putenv(format(work, sizeof(work), B20SUBMIT "=%s", tempfile_name));
  if (rc == 0) rc = execl(program, program, NULL);
  if (rc != 0)
  {
    printf("RC %d: ", rc);
    *erc = ERC_PROGRAM_CLOSED_WITH_RC;
  }
}

static int compare_code(const void* a, const void* b)
{
  const Symbol* symbol_a = a;
  const Symbol* symbol_b = b;
  return symbol_a->ch - symbol_b->ch;
}

void print_usage(int *erc, int argc, char* argv[])
{
  int i;
  Symbol *sorted = (Symbol *)malloc(sizeof(symbols));
  memcpy(sorted, &symbols, sizeof(symbols));
  qsort(sorted, no_symbols, sizeof(Symbol), compare_code);
  printf(
    "usage:\n"
    "  %s program codes [parms]\n"
    "  where:\n"
    "    program - example  MainCont\n"
    "    codes   - example  MonthEnd.Sub01\n"
    "    parms   - examples MONTH=JAN used as $MONTH\n"
    "                       JAN       used as $0\n\n"
    " The codes file has the following rules\n"
    " -  Normal printable characters except $, # and < are used as is.\n"
    "    This includes spaces.\n"
    "    $, # and < need to be $$, ## or << to be used asis.\n"
    " -  The newline character is used to delimit $n, $id and #xx else it is used asis.\n"
    " -  $n  n is 0..9 and selects the Nth parm, from above $0 would be JAN\n"
    " -  $id from above MONTH=JAN, $MONTH would be used\n"
    " -  #xx x is 0..9A..F the 2 chars represent the hex code of the char\n"
    " -  <code> below is the table to be used for code\n\n"
    "    Note <NL> becomes a newline and cannot be used as a # or $ delimiter\n\n"
    , argv[0]
    );
  for (i=0; i<no_symbols; i++)
  {
    printf("%02X:%-16s ", sorted[i].ch, sorted[i].key);
    if (i % 4 == 3)
      printf("\n");
  }
  printf("\n");
  free(sorted);
  exit(show(*erc));
}

int main(int argc, char *argv[])
{
  int erc = 0;
  if (argc < 3)
  {
    erc = ERC_MISSING_PROGRAM_CODES;
    print_usage(&erc, argc, argv);
  }
  submit(&erc, argc, argv);
  return show(erc);
}
