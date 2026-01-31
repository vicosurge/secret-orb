program TestEOF;
var
  f: file;
  buffer: array[0..255] of byte;
  bytesRead: word;
  i: integer;
  
begin
  { Create a file with only 5 bytes }
  Assign(f, 'small.dat');
  Rewrite(f, 1);
  for i := 0 to 4 do
    buffer[i] := i + 65;  { A, B, C, D, E }
  BlockWrite(f, buffer, 5);  { Write only 5 bytes }
  Close(f);
  
  { Now try to read 10 bytes from a 5-byte file }
  Assign(f, 'small.dat');
  Reset(f, 1);
  
  BlockRead(f, buffer, 10, bytesRead);  { Ask for 10 }
  
  writeln('Asked for 10 bytes');
  writeln('Actually got: ', bytesRead, ' bytes');  { Will show 5 }
  
  { Print what we got }
  for i := 0 to bytesRead - 1 do
    write(chr(buffer[i]), ' ');  { Will print: A B C D E }
  writeln;
  
  Close(f);
end.
