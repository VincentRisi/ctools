#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
 
static int (*foo)(int, int);
 
int main(int argc, char *argv[])
{
  int fred(int a, int b) {return a + b;}
  foo = &fred;
  printf("hello %d\n", fred(12, 13));
  return 0;
}
