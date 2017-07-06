#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>

#define MAX_LINE	(64)

extern int filter(const struct dirent *);

int main()
{
  char string[8];
  memset(string, sizeof(string), '\0');
  strncpy(string, "0123456", sizeof(string)-1);

  size_t size = MAX_LINE;
  char *buf = malloc(size);
  struct dirent **dir;
  int files = scandir(".", &dir, filter, alphasort);
  int index;

  printf("%s\n",string);

  for(index = 0; index < files; ++index){
    int counter = 0;
    FILE *csv = fopen(dir[index]->d_name,"r");
    while(fgets(buf,size,csv)) {
      if(strchr(buf,'\n')){
	++counter;
	printf("%d %s",counter,buf);
      } else {
	printf("before %ld\n", ftell(csv));
	fseek(csv,-(size - 1),SEEK_CUR);
	printf("after %ld\n", ftell(csv));
	free(buf);
	size *= 2;
	buf = malloc(size);
      }
    }
    fclose(csv);
    printf("%s,%d\n", dir[index]->d_name, counter);
    free(dir[index]);
  }
  free(dir);
  free(buf);
}
