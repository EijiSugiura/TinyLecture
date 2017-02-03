#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>
#include <regex>
#include <boost/filesystem.hpp>
#include <boost/range/algorithm.hpp>

using namespace std;
using namespace boost::filesystem;

void dump(vector<path>& v)
{
  for_each(begin(v),end(v),[](path &x){ cout << x << endl; });
}

int main(int argc, char* argv[])
{
  path p(".");
  vector<path> v;

  const regex filter( ".*\\.csv$" );
  for( directory_iterator i( p ); i != directory_iterator(); ++i ){
    smatch what;
    if( !regex_match( i->path().filename().string(), what, filter) ) continue;
    v.push_back( i->path() );
  }

  cout << "Before sort" << std::endl;
  dump(v);
  boost::sort(v);
  cout << "After sort" << std::endl;
  dump(v);

  return 0;
}
