#include <regex>
#include <boost/filesystem.hpp>
using namespace std;
using namespace boost::filesystem;

bool notmatch(path p)
{
  const regex filter( ".*\\.csv$" );
  smatch what;
  return !regex_match( p.filename().string(), what, filter);
}

