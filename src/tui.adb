pragma Warnings (Off, "-gnatwu"); --  TODO
pragma Ada_2012;

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Containers.Indefinite_Vectors;
with Ada.IO_Exceptions;

with Toolkit.Contexts;

procedure TUI
  (FDB : Toolkit.Features.Feature_Database;
   PDB : Toolkit.Phonemes.Phoneme_Database)
is
   use Toolkit;

   type Commands is (C_Help, C_Values, C_Phones, C_Lookup, C_Quit);

   package Argument_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, String);
   subtype Argument_List is Argument_Lists.Vector;

   function Split (Input : String) return Argument_List;
   function Split (Input : String) return Argument_List is
      use Ada.Strings.Fixed;

      Start_Index : Positive := Input'First;
      Space_Index : Natural;
      Result      : Argument_List;
   begin
      Space_Index := Index (Input, " ", Start_Index);

      if Space_Index = 0 then
         Space_Index := Input'Last + 1;
      end if;

      while Start_Index < Input'Last loop
         Result.Append (Input (Start_Index .. Space_Index - 1));

         if Space_Index = 0 then
            exit;
         end if;

         Space_Index := Index (Input, " ", Start_Index);
         Start_Index := Space_Index + 1;
      end loop;

      return Result;
   end Split;

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
   Blank   : Contexts.Context;
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
         when C_Values =>
            null;
         when C_Phones =>
            null;
         when C_Lookup =>
            begin
               Put_Line
                 (Phonemes.To_XML
                    (Phonemes.Resolve_Text (FDB, PDB, Get_Line, Blank)));
            exception
               when X : Phonemes.Unknown_Phoneme =>
                  Put_Line
                    (Ada.Exceptions.Exception_Name (X) & ": " &
                     Ada.Exceptions.Exception_Message (X));
            end;
         when C_Quit =>
            Quit := True;
      end case;
   end loop;
exception
   when Ada.IO_Exceptions.End_Error =>
      return;
end TUI;
