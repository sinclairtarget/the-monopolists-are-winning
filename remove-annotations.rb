require 'fileutils'

files = Dir["concentration-data/*with_ann.csv"]

files.each do |filepath|
  f = File.open(filepath)
  f_no_ann = File.open(filepath.gsub('_with_ann', ''), 'w')
  f.each_with_index do |line, i|
    unless i == 1 # Skip second line
      f_no_ann.write(line)
    end
  end

  f.close
  f_no_ann.close
end
