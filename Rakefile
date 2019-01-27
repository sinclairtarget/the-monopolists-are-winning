require 'rake/clean'

PLOT_SOURCE_DIR = 'plots'.freeze
GRAPH_OUTPUT_DIR = 'static-graphs/raw'.freeze

GRAPH_SOURCE_FILES = FileList["#{PLOT_SOURCE_DIR}/*.r"]
GRAPH_OUTPUT_FILES =
  GRAPH_SOURCE_FILES.pathmap("#{GRAPH_OUTPUT_DIR}/%n").ext('.png')

task :default => [:graphs]

desc 'Run graph R scripts and output graphs.'
task :graphs => GRAPH_OUTPUT_FILES
CLOBBER.include(GRAPH_OUTPUT_FILES)
CLEAN.include('Rplots.pdf')

desc 'Generate PDF report from rmarkdown file.'
task :report => 'report.html'

directory GRAPH_OUTPUT_DIR

file 'report.html' => ['report.rmd'] + GRAPH_SOURCE_FILES do
  sh %{ Rscript -e "rmarkdown::render('report.rmd')" }
end
CLOBBER.include('report.html')

rule '.png' => [-> (name) { source_for_png(name) }, GRAPH_OUTPUT_DIR] do |t|
  sh "Rscript graph.r '#{t.source}'"
end

def source_for_png(name)
  name.pathmap("#{PLOT_SOURCE_DIR}/%n").ext('.r')
end
