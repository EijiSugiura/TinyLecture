#include <iostream>
#include <iterator>
#include <fstream>
#include <vector>
#include <algorithm>
#include <regex>
#include <boost/filesystem.hpp>
#include <boost/range/algorithm.hpp>
using namespace std;
using namespace boost::filesystem;

extern bool notmatch(path p);
  
int main(int argc, char* argv[])
{
  path p(".");
  vector<path> v;

  remove_copy_if(directory_iterator(p), directory_iterator(), back_inserter(v), notmatch);
  boost::sort(v);
  for_each(begin(v), end(v), [](path &x){
      ifstream ifs(x.filename().string());
      ifs.unsetf(ios_base::skipws);
      int counter = count(istream_iterator<char>(ifs),
			  istream_iterator<char>(),'\n');
      cout << x << " " << counter << endl;
    });
  return 0;
}
