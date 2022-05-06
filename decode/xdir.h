#ifndef _XDIR_H_
#define _XDIR_H_ 

#if defined(WIN32)
#include <stdlib.h>
#define DIRMAX_PATH _MAX_PATH
#define DIRMAX_DIR  _MAX_DRIVE + _MAX_DIR
#define DIRMAX_NAME _MAX_FNAME
#define DIRMAX_EXT  _MAX_EXT
#define PATH_DELIM ';'
#else
#include <stdio.h>
#define DIRMAX_PATH FILENAME_MAX
#define DIRMAX_NAME FILENAME_MAX
#define DIRMAX_DIR  FILENAME_MAX
#define DIRMAX_EXT  FILENAME_MAX
#define PATH_DELIM ':'
#endif

void fsplit(const char *path, char *dir, char *name, char *ext);
/*  splits path into user supplied memory for dir, name and ext */

void fmerge(char *path, const char *dir, const char *name, const char *ext);
/*  merges dir, name and ext into user supplied memory for path */

char *fname(const char *path);
/*  returns strdup of name part of path (user must free) */

char *fdir(const char *path);
/*  returns strdup of dir part of path (user must free) */

char *fext(const char *path);
/*  returns strdup of ext part of path (user must free) */

#endif

