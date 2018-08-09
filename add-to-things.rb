require 'uri'

"
Generates Things.app urls to add books as projects with chapters as tasks

USAGE:
ruby add-to-things.rb <name> <chapter-count>

# For somebook with 5 chapters
ruby add-to-things.rb SomeBook 5

LIST_ID=project-or-area-id ruby add-to-things.rb SomeBook 5
"

list_id = ENV['LIST_ID']
arg_index = if list_id
  {unit_count: 0, todo_prefix: 1, action_name: 'add'}
else
  {project: 0, unit_count: 1, todo_prefix: 2, action_name: 'add-project'}
end

project = if arg_index[:project]
  ARGV[arg_index[:project]]
end

unit_count = (ARGV[arg_index[:unit_count]] || 3).to_i
todo_prefix = ARGV[arg_index[:todo_prefix]] || "Chapter"

titles = (1..unit_count).map {|i| "#{todo_prefix}-#{i}"}
titles = URI.encode(titles.join("\n"))

things_url = if ENV['LIST_ID']
   list_id = URI.encode(list_id)
  "things:///#{arg_index[:action_name]}?titles=#{titles}&list-id=#{list_id}&reveal=true"
else
  project = URI.encode(project)
  "things:///#{arg_index[:action_name]}?to-dos=#{titles}&title=#{project}&reveal=true"
end

puts things_url

