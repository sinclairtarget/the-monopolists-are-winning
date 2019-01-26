require 'rake/clean'

OUTPUT_DIR = 'static-graphs/raw'.freeze

SOURCE_FILES = FileList['graph-*.r']
OUTPUT_FILES = SOURCE_FILES.pathmap("#{OUTPUT_DIR}/%p").ext('.png')

task :default => [:graphs]

desc 'Run graph R scripts and output graphs.'
task :graphs => OUTPUT_FILES
CLOBBER.include(OUTPUT_DIR)
CLEAN.include('Rplots.pdf')

directory OUTPUT_DIR

desc 'Generate PDF report from rmarkdown file.'
task :report => 'report.html'

file 'report.html' do
  sh %{ Rscript -e "rmarkdown::render('report.rmd')" }
end

rule '.png' => [-> (name) { source_for_png(name) }, OUTPUT_DIR] do |t|
  sh "Rscript '#{t.source}'"
end

def source_for_png(name)
  name.pathmap("%{^#{OUTPUT_DIR}/,}p").ext('.r')
end
