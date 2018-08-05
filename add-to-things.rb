require 'uri'

"
Generates Things.app urls to add books as projects with chapters as tasks

USAGE:
ruby add-to-things.rb <name> <chapter-count>

# For somebook with 5 chapters
ruby add-to-things.rb SomeBook 5
"

project = ARGV[0]
titles = (1..11).map {|i| "Chapter-#{i}"}

puts "#{ARGV.inspect}"
unit_count = (ARGV[1] || 3).to_i

titles = (1..unit_count).map {|i| "Chapter-#{i}"}

titles = URI.encode titles.join("\n")
project = URI.encode project

things_url = "things:///add?titles=#{titles}&list=#{project}&reveal=true"
puts things_url

