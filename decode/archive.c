#include "archive.h"
#include <stdio.h>
#include <malloc.h>
#include "binio.h"

int main(int argc, char*argv[])
{
  int i, rc = 0;
  size_t size;
  char* buffer = 0;
  FILE* outfile;
  FILE* infile;
  if (argc < 3)
  {
    printf("usage: ctos_archive library_file binfile ...\n");
    return 1;
  }
  outfile = fopen(argv[1], "wb");
  if (outfile == 0)
  {
    printf("%s failed to open for writing\n", argv[1]);
    return 1;
  }
  bin_putstrn("ARCH", 4, outfile);
  for (i=2; i<argc; i++)
  {
    infile = fopen(argv[i], "rb");
    if (infile == 0)
    {
      printf("%s failed to open for reading\n", argv[i]);
      rc = 1;
      continue;
    }
    fseek(infile, 0l, SEEK_END);
    size = (size_t)ftell(infile);
    buffer = (char*)realloc(buffer, size);
    fseek(infile, 0l, SEEK_SET);
    fread(buffer, 1, size, infile);
    fclose(infile);
    if (strncmp(buffer, "CTOS", 4) == 0)
    {
      bin_putint((int)size, outfile);
      fwrite(buffer, 1, size, outfile);
    }
    else
    {
      printf("%s invalid ctos forms file\n", argv[i]);
      rc = 1;
    }
  }
  free(buffer);
  fclose(outfile);
  return rc;
}
