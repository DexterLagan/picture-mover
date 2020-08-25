# picture-mover
  A small app that automatically organizes imported camera pictures.<br>

  Say your pictures folder is organized this way:<br>
/2020-08-21 -> pics<br>
/2020-08-22 -> pics<br>
/2020-08-23 -> pics<br>
<br>
  Picture-mover will create an 'Originals' sub-folder in each directory, and move all pictures in each. Folder dates are also automatically renamed to conform the following format: YYYYMMDD.<br>
  The resulting folder structure will be:<br>
/20200821<br>
  |_ Originals -> pics<br>
/20200821<br>
  |_ Originals -> pics<br>
  /20200821<br>
  |_ Originals -> pics<br>
<br>  
Detailed Process Overview:<br>
1) reads the source path from a config file of the same name (picture-mover.conf);
2) make a list of all folders in the source folder;
3) for each folder,
   a) if no Originals folder exists, create it;
   b) move all files to the ./Originals folder.

Cheers,<br>
<br>
Dexter<br>
