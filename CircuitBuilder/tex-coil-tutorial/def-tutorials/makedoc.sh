#!/bin/sh 
# make the documentation ready to be included in directory "doc" in nic.

modules="latex-elmergui latex-tutorials-CL latex-tutorials-GUI latex-elmer-getstart latex-tex-start latex-tex-template"
for m in $modules; do
  cd $m && make manual && make install && make clean && cd ..
done

mkdir -p ElmerDocumentation
/bin/mv -f *.pdf ElmerDocumentation
tar cvf ElmerDocumentation.tar ElmerDocumentation
gzip ElmerDocumentation.tar
zip -r ElmerDocumentation.zip ElmerDocumentation

tar cvf tutorials-CL-files.tar tutorials-CL-files
gzip tutorials-CL-files.tar
tar cvf tutorials-GUI-files.tar tutorials-GUI-files
gzip tutorials-GUI-files.tar
zip -r tutorials-CL-files.zip tutorials-CL-files
zip -r tutorials-GUI-files.zip tutorials-GUI-files

mkdir -p doc
/bin/mv -f *.zip *.tar.gz doc
/bin/cp -r ElmerDocumentation/*.pdf doc
