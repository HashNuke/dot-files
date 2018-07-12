require 'uri'

project = "SomeProject"
titles = (1..11).map {|i| "Chapter-#{i}"}

units = [
  {name: "Foo", count: 4},
  {name: "Bar", count: 5}
]

# titles = []
# units.each do |unit|
#   titles += (1..unit[:count]).map {|i| "#{unit[:name]}-#{i}" }
# end


titles = URI.encode titles.join("\n")
project = URI.encode project

things_url = "things:///add?titles=#{titles}&list=#{project}&reveal=true"
puts things_url