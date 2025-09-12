pragma Ada_2012;

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;

with Toolkit.Grammars;

procedure Analyse is
   use Toolkit;
   use type Ada.Containers.Count_Type;

   package SL is new Ada.Containers.Indefinite_Vectors (Positive, String);

   function Split (X : String) return SL.Vector;
   function Split (X : String) return SL.Vector is
      use Ada.Strings.Fixed;
      First  : Positive := X'First;
      Last   : Natural;
      Result : SL.Vector;
   begin
      while First <= X'Last loop
         Last := Index (X, " ", First);
         if Last = 0 then
            Last := X'Last;
         elsif Last = First then
            Last := Last - 1;
            goto Next;
         else
            Last := Last - 1;
         end if;

         Result.Append (X (First .. Last));
         <<Next>>
         First := Last + 2;
      end loop;
      return Result;
   end Split;

   type Commands is (C_Help, C_Load, C_Quit);

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

   Quit      : Boolean := False;
   Arguments : SL.Vector;
   Command   : Commands;
   Grammar   : Toolkit.Grammars.Grammar;
begin
   while not Quit loop
      Put ("> ");

      Arguments := Split (Get_Line);
      begin
         Command := Commands'Value ("C_" & Arguments (1));
      exception
         when Constraint_Error =>
            Command := C_Help;
      end;

      case Command is
         when C_Help =>
            Help;
         when C_Load =>
            if Arguments.Length /= 2 then
               Put_Line ("| Requires one argument: 'Name'");
               goto Next;
            end if;
            begin
               Grammars.Load (Grammar, "languages/" & Arguments (2));
            exception
               when X : Toolkit.Grammars.Invalid_Grammar =>
                  Put_Line (Ada.Exceptions.Exception_Information (X));
                  goto Next;
            end;
         when C_Quit =>
            Quit := True;
      end case;
      <<Next>>
   end loop;
exception
   when Ada.IO_Exceptions.End_Error =>
      return;
end Analyse;
