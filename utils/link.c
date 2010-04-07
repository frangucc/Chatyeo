#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// TODO: do memory more efficiently with malloc for cmd

//int main()
char *process(char *str) 
{
  FILE *in;
  extern FILE *popen();
  char buff[512];
  char cmd[2048];
  char *final_buff;
  int count;

  final_buff = malloc(4096);
  count = 0;

  /* popen creates a pipe so we can read the output
     of the program we are invoking */
  strcpy (cmd, "./link-ext \"");
  //printf("cmd1: %s\n", cmd);
  strncat(cmd, str, 512-strlen(cmd));
  //printf("cmd2: %s\n", cmd);
  strncat(cmd, "\"", 512-strlen(cmd));
  //strncat (cmd, str, 512-strlen(cmd));

  if (!(in = popen(cmd, "r"))) 
  {
    exit(1);
  }

  /* read the output of netstat, one line at a time */
  while (fgets(buff, sizeof(buff), in) != NULL ) 
  {
    strncpy(&final_buff[count], &buff, 4096-count);
    count += strlen(buff);
    printf("Output: %s", buff);
  }

  //printf("FINAL BUFF\n\n\n");
  printf("%s\n", final_buff);
 
  /* close the pipe */
  pclose(in);
  
  return final_buff;
}

/*int main()
{
  process("I am being followed by a moonshadow");
  return 0;
  }*/
