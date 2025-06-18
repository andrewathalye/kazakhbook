with Ada.Text_IO; use Ada.Text_IO;
with Input_Sources.File; use Input_Sources.File;

procedure Analyse is
   Input : File_Input;
begin
   Put_Line ("Test Message");
   Open ("pref.xml", Input);
end Analyse;
