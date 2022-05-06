#include "ctosio.h"
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <ctype.h>
#include "getargs.h"
#include "xdir.h"
#include "Parser.h"

//static char *infile = "";
static char *bindir = "";
static char *cobdir = "";
static char *cdir   = "";
static char *binext = ".bin";
static char *cobext = ".cob";
static char *cext   = ".h";
static int cobgen   = 0;
static int cgen     = 0;
static int verbose  = 0;
static int lowerall = 0;

ARG argtab[] =
{ {'x', STRING, (int*) &binext, "bin extension"}
, {'y', STRING, (int*) &cobext, "cob extension"}
, {'z', STRING, (int*) &cext,   "c extension"}
, {'B', STRING, (int*) &bindir, "bin directory"}
, {'C', STRING, (int*) &cobdir, "cob directory"}
, {'H', STRING, (int*) &cdir,   "c directory"}
, {'c', BOOLEAN, &cobgen,       "generate cobol copybook"}
, {'h', BOOLEAN, &cgen,         "generate c header"}
, {'v', BOOLEAN, &verbose,      "verbose"}
, {'l', BOOLEAN, &lowerall,     "all generated files lowercase"}
};
#define TABSIZE (sizeof(argtab) / sizeof(ARG))

static ctos_form* compile(const char* filename)
{
  FILE* infile = fopen(filename, "rb");
  if (infile == 0)
  {
    printf("File %s not opened\n",filename);
    return 0;
  }
  size_t from = ftell(infile);
  ctos_form* form = ctos_alloc_form();
  while (true)
  {
    char work[256];
    if (fgets(work, 256, infile) == 0)
    {
      printf("File %s failed to read screen lines\n",filename);
      ctos_free_form(form);
      return 0;
    }
    char* end = strchr(work, '\r');
    if (end == 0)
      end = strchr(work, '\n');
    if (end != 0)
      *end = 0;
    char *p = work;
    while(*p == '\f') p++;
    if (strncmp(p, "Form", 4) == 0)
      break;
    if (strlen(p) > 0)
      ctos_add_line(form, p);
    from = ftell(infile);  
  }
  fseek(infile, 0l, SEEK_END);
  size_t size = ftell(infile) - from;
  fseek(infile, (long) from, SEEK_SET);
  char *buffer;
  buffer = (char*)malloc(size+1);
  fread(buffer, 1, size, infile);
  buffer[size] = 0;
  fclose(infile);
  Scanner scanner((const unsigned char *)buffer, strlen(buffer));
  Parser parser(&scanner);
  parser.form = form;
  parser.Parse();
  free(buffer);
  return parser.form;
}

static char* _make_target_(char* work, const char* in_dir, const char* in_ext, const char* filename, bool lowercase=false)
{
  int i;
  char dir[DIRMAX_DIR], name[DIRMAX_NAME], ext[DIRMAX_EXT];
  fsplit(filename, dir, name, ext);
  if (strlen(in_dir) > 0) strcpy(dir, in_dir);
  if (strlen(in_ext) > 0) strcpy(ext, in_ext);
  fmerge(work, dir, name, ext);
  if (lowercase == true)
  {
    for (i = 0; i < strlen(work); i++)
      work[i] = tolower(work[i]);
  }
  return work;
}

void process(const char* filename)
{
  printf("Compiling %s\n", filename);
  ctos_form* form = compile(filename);
  if (form != 0)
  {
    bool lowercase = lowerall == 1;
    printf("Compiled\n");
    char work[512];
    FILE* outfile = fopen(_make_target_(work, bindir, binext, filename, lowercase), "wb");
    if (outfile == 0)
    {
      printf("%s file failed to open\n", work);
      return;
    }
    ctos_write_form(form, outfile);
    fclose(outfile);
    ctos_free_form(form);
    free(form);
    FILE* infile = fopen(work, "rb");
    if (infile == 0)
    {
      printf("%s file failed to open\n", work);
      return;
    }
    form = ctos_read_form(infile);
    fclose(infile);
    if (cobgen != 0)
    {
      FILE* cobfile = fopen(_make_target_(work, cobdir, cobext, filename, lowercase), "wb");
      if (cobfile == 0)
      {
        printf("%s file failed to open\n", work);
        return;
      }
      ctos_write_copybook(form, cobfile);
      fclose(cobfile);
    }
    if (cgen != 0)
    {
      FILE* cfile = fopen(_make_target_(work, cdir, cext, filename, true), "wb");
      if (cfile == 0)
      {
        printf("%s file failed to open\n", work);
        return;
      }
      ctos_write_cheader(form, cfile);
      fclose(cfile);
    }
  }
  else
    printf("%s --failed to compile--\n", filename);
}

int main(int argc, char*argv[])
{
  int i;
  argc = get_args(argc, argv, argtab, TABSIZE);
  if (argc < 2)
  {
    printf("usage: ctos_decode <options> formsource ...\n");
    printf("use -q (or any other invalid option) to get a list of options\n");
    printf("many formsources can be supplied.\n");
    return 1;
  }
  for (i=1; i<argc; i++)
  {
    process(argv[i]);
  }
  return 0;
}
