#!/usr/bin/ruby

Dir.open('.').grep(/.*\.csv$/).sort.each do |filename|
  File.open(filename,"r") do |file|
    while file.gets; end
    puts filename << "," << file.lineno.to_s
  end
end
