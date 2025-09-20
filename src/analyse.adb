pragma Ada_2012;

with Ada.Containers;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.IO_Exceptions;

with Toolkit.Grammars;
with Toolkit.Features;
with Toolkit.Contexts;
with Toolkit.Phonemes;
with Toolkit.Strings;
with Toolkit.Syllables;
with Toolkit.Log;

procedure Analyse is
   use Toolkit;
   use all type Contexts.Context_Scope;
   use type Ada.Containers.Count_Type;

   type Commands is
     (C_Help, C_Load, C_Add, C_Resolve, C_Syllabify, C_Debug, C_Quit, C_Q);

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
   APL       : Phonemes.Abstract_Phoneme_List;
begin
   while not Quit loop
      Put ("> ");

      Arguments := Toolkit.Strings.Split (Get_Line);

      --  Comments
      if not Arguments.Is_Empty and then Arguments.First_Element = "#" then
         goto Next;
      end if;

      --  Parse Command
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
         when C_Add =>
            if Arguments.Length < 2 then
               Put_Line ("| Requires a feature list after command name");
               goto Next;
            end if;
            declare
               Feature_List : constant String :=
                 Strings.Join (Arguments, 2, Arguments.Last_Index);
            begin
               APL.Append
                 (Phonemes.To_Ada
                    (Grammar.Features, Grammar.Phonemes, Feature_List));
            exception
               when X : Phonemes.Unknown_Phoneme         =>
                  Put_Line (Ada.Exceptions.Exception_Information (X));
                  Put_Line ("| Unknown phoneme");
               when X : Toolkit.Features.Unknown_Feature =>
                  Put_Line (Ada.Exceptions.Exception_Information (X));
                  Put_Line ("| Unknown feature");
            end;
         when C_Resolve =>
            if Arguments.Length /= 1 then
               Put_Line ("| Does not take arguments");
               goto Next;
            end if;
            if APL.Length = 0 then
               Put_Line ("| Call Add to add phonemes");
               goto Next;
            end if;

            declare
               PL : Phonemes.Phoneme_List;
            begin
               PL :=
                 Phonemes.Resolve
                   (Grammar.Phonemes, APL, Contexts.Isolated (Text));
               Put_Line ("Result: " & Phonemes.Transcribe (PL));
            exception
               when X : Phonemes.Indeterminate_Phoneme =>
                  Put_Line (Ada.Exceptions.Exception_Information (X));
                  Put_Line ("| Indeterminate phoneme");

            end;

            APL.Clear;
         when C_Syllabify =>
            if Arguments.Length /= 2 then
               Put_Line ("| Takes one argument");
               goto Next;
            end if;

            APL := Phonemes.Transcribe (Grammar.Phonemes, Arguments (2));
            Put_Line
              ("Result: " &
               Syllables.Transcribe
                 (Syllables.Syllabify
                    (Grammar.Syllables,
                     Phonemes.Resolve
                       (Grammar.Phonemes, APL, Contexts.Isolated (Text)))));
         when C_Debug =>
            if Arguments.Length /= 2 then
               Put_Line ("| Takes one argument");
               goto Next;
            end if;
            Toolkit.Log.Enable
              (Toolkit.Log.Log_Category'Value (Arguments (2)));
         when C_Quit | C_Q =>
            Quit := True;
      end case;
      <<Next>>
   end loop;
exception
   when Ada.IO_Exceptions.End_Error =>
      return;
end Analyse;
