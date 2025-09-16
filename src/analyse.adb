pragma Ada_2012;

with Ada.Containers;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.IO_Exceptions;

with Toolkit.Grammars;
with Toolkit.Features;
with Toolkit.Phonemes;
with Toolkit.Strings;

procedure Analyse is
   use Toolkit;
   use type Ada.Containers.Count_Type;

   type Commands is (C_Help, C_Load, C_Resolve, C_Quit);

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
   Arguments : Toolkit.Strings.Argument_List;
   Command   : Commands;
   Grammar   : Toolkit.Grammars.Grammar;
begin
   while not Quit loop
      Put ("> ");

      Arguments := Toolkit.Strings.Split (Get_Line);
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
         when C_Resolve =>
            if Arguments.Length < 2 then
               Put_Line ("| Requires a feature list after command name");
               goto Next;
            end if;
            declare
               Feature_List : constant String :=
                 Strings.Join (Arguments, 2, Arguments.Last_Index);
               APL          : Phonemes.Abstract_Phoneme_List;
               PI           : Phonemes.Phoneme_Instance;
            begin
               APL.Append
                 (Phonemes.To_Ada
                    (Grammar.Features, Grammar.Phonemes, Feature_List));
               PI :=
                 Phonemes.Resolve
                   (Grammar.Phonemes, APL.First_Element,
                    Phonemes.To_Cursor (APL.First));
               Put_Line (Phonemes.To_XML (PI));
            exception
               when X : Phonemes.Indeterminate_Phoneme =>
                  Put_Line (Ada.Exceptions.Exception_Information (X));
                  Put_Line ("| Indeterminate phoneme");
               when X : Toolkit.Features.Unknown_Feature =>
                  Put_Line (Ada.Exceptions.Exception_Information (X));
                  Put_Line ("| Unknown feature");
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
