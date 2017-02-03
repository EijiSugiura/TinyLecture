#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>

#define MAX_LINE	(256)

extern int filter(const struct dirent *);

int main()
{
  char buf[MAX_LINE];
  struct dirent **dir;
  int files = scandir(".", &dir, filter, alphasort);
  int index;
  for(index = 0; index < files; ++index){
    int counter = 0;
    FILE *csv = fopen(dir[index]->d_name,"r");
    while(fgets(buf,sizeof(buf),csv)) {
      ++counter;
    }
    fclose(csv);
    printf("%s,%d\n", dir[index]->d_name, counter);
    free(dir[index]);
  }
  free(dir);
}
