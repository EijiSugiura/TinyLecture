#include <string.h>
#include <dirent.h>

int filter(const struct dirent *dir)
{
  char *found = strstr(dir->d_name, ".csv");
  if(found == NULL)
    return 0;
  if(strlen(found) != strlen(".csv"))
    return 0;
  return 1;
}

