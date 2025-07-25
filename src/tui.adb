pragma Ada_2012;

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.IO_Exceptions;

with Toolkit.Contexts;

procedure TUI
  (FDB : Toolkit.Features.Feature_Database;
   PDB : Toolkit.Phonemes.Phoneme_Database;
   SDB : Toolkit.Symbols.Symbol_Database)
is
   use Toolkit;

   type Commands is (C_Help, C_Lookup, C_Transcribe, C_Quit);

   procedure Help;
   procedure Help is
   begin
      Put_Line ("| Available Commands:");
      Put ("| ");
      for CT in Commands'Range loop
         Put (CT'Image (3 .. CT'Image'Last) & " ");
      end loop;
      New_Line;
   end Help;

   Quit    : Boolean := False;
   Command : Commands;
begin
   while not Quit loop
      Put ("> ");

      begin
         Command := Commands'Value ("C_" & Get_Line);
      exception
         when Constraint_Error =>
            Command := C_Help;
      end;

      case Command is
         when C_Help =>
            Help;
         when C_Lookup =>
            begin
               Put_Line
                 (Phonemes.To_XML
                    (Phonemes.Resolve
                       (PDB, Phonemes.To_Ada (FDB, PDB, Get_Line),
                        Contexts.Null_Context)));
            exception
               when X : Phonemes.Unknown_Phoneme =>
                  Put_Line
                    (Ada.Exceptions.Exception_Name (X) & ": " &
                     Ada.Exceptions.Exception_Message (X));
            end;
         when C_Transcribe =>
            Put_Line
              (Phonemes.Transcribe
                 (Symbols.To_Phonemes
                    (Symbols.Resolve
                       (PDB => PDB, ASL => Symbols.To_Ada (SDB, Get_Line),
                        External_Symbol_Context  => Contexts.Null_Context,
                        External_Phoneme_Context => Contexts.Null_Context))));
         when C_Quit =>
            Quit := True;
      end case;
   end loop;
exception
   when Ada.IO_Exceptions.End_Error =>
      return;
end TUI;
