#!/usr/bin/ruby

require 'csv'

Dir.open('.').grep(/.*\.csv$/).sort.each do |filename|
  counter = Hash.new(0)
  CSV.foreach(filename) do |row|
    counter[row[6]] += 1
  end
  puts filename
  counter.sort_by{|k, v| -v}.slice(1..10).each do |name,value|
    puts "#{name},#{value}"
  end
end
