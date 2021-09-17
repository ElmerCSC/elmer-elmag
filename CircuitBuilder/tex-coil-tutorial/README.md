Template for ElmerGUI Tutorials
=============================

- This document, Template for ElmerGUI Tutorials, is licensed under Creative Commons Attribution-NonCommercial.

Purpose
-------

- Instead of compiling all twenty plus tutorials that takes minutes, compiling a single tutorial only takes seconds.
- Let's reduce the size and set up a copy of the tutorial directory structure, but with only a single template tutorial.
- Use the single template to create and edit a single tutorial, and see what the results will look like in the document, including the table of contents and the index.
- Using the template should allow creation of new tutorials easier, since the details of setting up an outline in TexWorks has been taken care of by the template.

Summary
-------

- The directory, 'latex-tex-template', includes a LaTeX template for creating new ElmerGUI tutorials.
- All the files needed to use Texworks to compile a single tutorial are included.
- The bash script 'source makedoc' in the parent directory will also work.

How to use the template
-----------

- Pick a name for your new tutorial, we'll use 'MyTutorial' for these instructions.
- Get a fresh copy of 'elmerfem-manuals' using git:
  - git clone  https://github.com/ElmerCSC/elmerfem-manuals.git
- Create a new folder next to 'elmerfem-manuals' directory, such as 'MyNewTutorial'.
- Copy the folder 'latex-tex-template', and three files 'elmerdef.tex', 'elmerversion.tex', and 'makedoc' from the 'elmerfem-manuals' folder into the new folder 'MyNewTutorial'.
- Change to 'MyNewTutorial' and then to the 'latex-tex-template' directory.
- Edit the file 'TexTemplate.tex' and search and replace the entries for 'Template' to 'MyTutorial'.  Specifically, change these entries:
  - \graphicspath{{./}{Template/}}
  - \include{Template/Template}
- Rename the 'Template' directory to an appropriate tutorial name, like 'MyTutorial'.  Change to your newly renamed folder 'MyTutorial', and then rename the file 'Template.tex' to 'MyTutorial.tex'.
- Change back to the 'latex-tex-template' directory, and open 'TexTemplate.tex' in Texworks.  Press the green arrow, and it should compile the template.
- When you think you are done editing, be sure to search for both 'zzz' and '??'.  There shouldn't be any of either items left.  The 'zzz' is a place holder that needs to be replaced, and the '??' indicates that TeXworks couldn't find a figure number reference.  Also as a final step, verify that any needed keywords, nouns, technical terms, etc., have been added to the index using '\Idx{...}'.
- When you (finally) are done editing the new tutorial, copy the single folder 'MyTutorial' into the folder 'elmerfem-manuals\latex-tutorials-GUI'.  Then edit the file 'ElmerTutorials.tex', and add an entry for both '\graphicspath' and '\include' that points to the new tutorial folder.
  - \graphicspath{{./}{MyTutorial/}}
  - \include{MyTutorial/MyTutorial}
- You can locate the new pair of entries anywhere near the current list of tutorials, and if possible, locate the new tutorial into a section with other similar tutorials.
- One more item, as part of developing your new tutorial, you probably have made a folder with a working ElmerGUI project, with the mesh files,and a geometry input file.   You can create a new folder under 'elmerfem-manuals\tutorials-GUI-files', using the same folder name as 'MyTutorial', and copy only the egproject.xml file, the mesh files, and the (optional) geometry input file into the new folder.
- Finish up by creating a pull request for the new tutorial.
