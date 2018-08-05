require 'uri'

"
Generates Things.app urls to add books as projects with chapters as tasks

USAGE:
ruby add-to-things.rb <name> <chapter-count>

# For somebook with 5 chapters
ruby add-to-things.rb SomeBook 5
"

project = ARGV[0]
unit_count = (ARGV[1] || 3).to_i
todo_prefix = ARGV[2] || "Chapter"

titles = (1..unit_count).map {|i| "#{todo_prefix}-#{i}"}
titles = URI.encode titles.join("\n")
project = URI.encode project

things_url = "things:///add-project?to-dos=#{titles}&title=#{project}&reveal=true"
puts things_url

