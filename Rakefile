require 'rake/clean'
 
INCLUDE = "include"
ERLC_FLAGS = "-I#{include} +warn_unused_vars +warn_unused_import"
 
SRC = FileList['src/*.erl']
OBJ = SRC.pathmap("%{src.ebin}X.beam")
CLEAN.include("ebin/*.beam")

directory 'ebin'
 
rule ".beam" => ["%{ebin.src}X.erl"] do |t|
  sh "erlc -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source}"
end

task :libs do
  cp Dir.glob('lib/**/*.beam'), 'ebin/'
end
 
task :compile => ['ebin'] + OBJ
task :default => :compile