require 'set'

class CircusNode
  attr :name, :weight

  def initialize(name, weight, children=nil)
    @name = name
    @weight = weight
    @children = children || []
  end

  def <<(c)
    @children << c
  end

  def total_weight
    @total_weight ||= @weight + @children.map {|c| c.total_weight}.sum
  end

  def balance_weight(target=nil)
    by_weight = Hash.new{|h, k| h[k] = []}
    @children.each{|c| by_weight[c.total_weight] << c}

    if by_weight.size == 1 then
      if target
        return @weight - (total_weight - target)
      else
        raise ArgumentError, 'This tree seems balanced!'
      end
    else
      odd_one_out = by_weight.select {|k, v| v.length == 1}.first[1][0]
      child_target = by_weight.select {|k, v| v.length > 1}.first[0]
      return odd_one_out.balance_weight child_target
    end
  end

  def to_s
    "#{@name} (#{@weight})"
  end

  def print_tree(n=0)
    puts "#{'    '*n}#{self} -> #{self.total_weight}"
    @children.each do |child|
      child.print_tree n+1
    end
  end

end

def build_circus(data)
  all_nodes = {}
  all_children = Set.new

  data.each do |name, weight, children|
    all_nodes[name] = CircusNode.new name, weight
  end

  data.each do |name, weight, children|
    children.each {|child| all_nodes[name] << all_nodes[child]}
    all_children.merge children
  end

  root_name = (all_nodes.keys.to_set - all_children).first
  return all_nodes[root_name]
end
    
data = readlines.map do |line|
  match = /(?<parent>\w+) \((?<weight>\d+)\)(?: -> (?<children>.*))?/.match line
  [match['parent'].to_sym,
   match['weight'].to_i,
   match['children'] ? match['children'].split(', ').map {|x| x.to_sym} : []]
end

root = build_circus data

puts "Root node: #{root}"

# puts root.print_tree
puts root.balance_weight
