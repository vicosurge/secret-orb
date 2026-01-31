program apacea_builder;

{$mode objfpc}{$H+}

const
  C_FOLDERNAME = 'docs//';

var
  MyFile: TextFile;
  c_filename: string;
begin
  // Assign file name by user input
  writeln('Assign file name: ');
  readln(c_filename);

  // Open file for writing or creates a brand new one
  AssignFile(MyFile, C_FOLDERNAME+c_filename);
  writeln('Write a text file');
  Rewrite(MyFile);
  writeln(MyFile, '=========');
  writeln(MyFile, 'Name = Main Room');
  writeln(MyFile, 'North = 0');
  writeln(MyFile, 'South = 0');
  writeln(MyFile, 'West = 0');
  writeln(MyFile, 'East = 0');

  // Close file
  CloseFile(MyFile);
  writeln('Finished writing file ', c_filename);
end.
