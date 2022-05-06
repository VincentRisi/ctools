#include "xdir.h"

#if defined(WIN32)
#include <io.h>
#else
#include <unistd.h>
#endif
#include <string.h>

char *fdir(char *path)
{
  char dir[DIRMAX_DIR];
  char name[DIRMAX_NAME];
  char ext[DIRMAX_EXT];
  fsplit(path, dir, name, ext);
  return strdup(dir);
}

char *fext(char *path)
{
  char dir[DIRMAX_DIR];
  char name[DIRMAX_NAME];
  char ext[DIRMAX_EXT];
  fsplit(path, dir, name, ext);
  return strdup(ext);
}

char *fname(char *path)
{
  char dir[DIRMAX_DIR];
  char name[DIRMAX_NAME];
  char ext[DIRMAX_EXT];
  fsplit(path, dir, name, ext);
  return strdup(name);
}

#if defined(WIN32)
void fmerge(char *path, const char *dir, const char *name, const char *ext)
{
  _makepath(path, "", dir, name, ext);
  for (size_t i=0; i<strlen(path); i++)
    if (path[i] == '/') path[i] = '\\';
}
#else
void fmerge(char *path, const char *dir, const char *name, const char *ext)
{
  int i;
  if (dir)
  {
    strcpy(path, dir);
    if ((i=strlen(dir)) && (dir[i-1] != '/'))
      strcat(path, "/");
  }
  if (name)
    strcat(path, name);
  if (ext)
  {
    if (strlen(ext) > 0 && ext[0] != '.')
      strcat(path, ".");
    strcat(path, ext);
  }
}
#endif

#if defined(WIN32)

void fsplit(const char *path, char *dir, char *name, char *ext)
{
  char drive[_MAX_DRIVE], dir2[_MAX_DIR];
  _splitpath(path, drive, dir2, name, ext);
  strcpy(dir, drive);
  strcat(dir, dir2);
}
#else 
// 0123456789              0123456789
// /make/up.c              up.c
//      p=5                p=*
//      n=(5-0)+1=6
//      dir='/make/'       dir=''
//      p=6                p=0 
//      p2=8               p2=2
//      n=(8-6)=2          n=(2-0)=2
//      name='up'          name='up'
//      ext='.c'           ext='.c'   
void fsplit(const char *path, char *dir, char *name, char *ext)
{
  char *p = strrchr((char*)path, '/');
  char *p2;
  if (p)
  {
    int n = (p-path)+1;
    strncpy(dir, path, n);
    dir[n] = 0;
    p++;
  }
  else
  {
    dir[0] = 0;
    p = (char*)path;
  }
  p2 = strrchr(p, '.');
  if (p2)
  {
    int n = (p2-p);
    strncpy(name, p, n);
    name[n] = 0;
    strcpy(ext, p2);
  }
  else
  {
    strcpy(name, p);
    ext[0] = 0;
  }
}
#endif
