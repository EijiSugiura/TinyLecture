#!/usr/bin/ruby

filename="ids-total-2016-01-01.csv"
File.open(filename,"r") do |file|
  while file.gets; end
  puts filename << "," << file.lineno.to_s
end

