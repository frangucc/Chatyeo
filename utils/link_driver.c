#include <stdio.h>
#include <string.h>
#include "link-includes.h"
typedef unsigned char byte;

int read_cmd(byte *buff);
int write_cmd(byte *buff, int len);

int main() {
  int fn, arg1, arg2;
  byte buff[100];
  char *test_return="test-return";
  char *result;

  while (read_cmd(buff) > 0) 
  {
    fn = buff[0];
    
    if (fn == 1)
    {
      char *str = &buff[1];
      //char *result = process(str);
      fprintf(stderr, "Calling process from driver\n");
      result = process(str);
      //shit3("blah");
      int len = strlen(result);
      fprintf(stderr, "Calling process done!, writing: %s : %d\n", result, len);
      write_cmd(result, strlen(result));
      fprintf(stderr, "Calling to write done! Woo!\n");
    }
    else
    {
      fprintf(stderr, "link_drv: Unknown function: ~d", fn);
    }
  }
}
