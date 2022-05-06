#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

#define B20RECORD         "_B20_RECORD_"
#define DEBUG_LOGGING_ON
#ifndef DEBUG_LOGGING_ON
  #define LOG(x)
#else
  static FILE* _logfile_;
  
  static void _log_(char* line)
  {
    if (_logfile_ == 0)
      _logfile_ = fopen("submit.log", "at");
    if (_logfile_ == 0) return;
    fprintf(_logfile_, "%s\n", line);
    fflush(_logfile_);
  }
  
  static char _log_buffer_[4196];
  #define LOG(x) _log_(x);
#endif

const char *_errors_[] =
{ "All Cool"
, "Missing program and or file for record"
, "Program is not executable"
, "Program is not executing"
, "Program closed with RC"
, "Code is not valid hex"
};

enum
{ ERC_OK
, ERC_MISSING_PROGRAM_FILE
, ERC_PROGRAM_NOT_EXECUTABLE
, ERC_PROGRAM_NOT_EXECUTING
, ERC_PROGRAM_CLOSED_WITH_RC
, ERC_CODE_IS_NOT_VALID_HEX
};

static char* _format_(char* work, int len, const char *fmt, ...)
{
  va_list  args;
  va_start(args, fmt);
  vsnprintf(work, len+1, fmt, args);
  va_end(args);
  return work;
}

static int _show_(int erc)
{
  printf("%s\n", _errors_[erc]);
  return erc; 
}

static void _record_(int *erc, const char* program, const char* file)
{
  if (*erc != 0) return;  
  if (access(program, X_OK) != 0)
  {
    printf("Testing %s: ", program);
    *erc = ERC_PROGRAM_NOT_EXECUTABLE;
    return;
  }
  char work[512];
  int rc = putenv(_format_(work, sizeof(work), B20RECORD "=%s", file));
  if (rc == 0) rc = execl(program, program, NULL);
  if (rc != 0)
  {
    printf("RC %d: ", rc);
    *erc = ERC_PROGRAM_CLOSED_WITH_RC;
  }
}

int main(int argc, char *argv[])
{
  int erc = 0;
  if (argc < 3)
    return(ERC_MISSING_PROGRAM_FILE);
  _record_(&erc, argv[1], argv[2]);
  return _show_(erc);
}
