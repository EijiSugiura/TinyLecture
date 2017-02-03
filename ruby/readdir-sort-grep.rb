#!/usr/bin/ruby

Dir.open('.').grep(/.*\.csv$/).sort.each do|filename|
  puts filename
end
