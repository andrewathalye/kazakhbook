pragma Ada_2012;

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Input_Sources.File; use Input_Sources.File;
with Sax.Readers;
with Schema.Dom_Readers; use Schema.Dom_Readers;

with Toolkit.Log; use Toolkit.Log;

package body Toolkit.Grammars is
   ----------
   -- Load --
   ----------
   procedure Load (G : out Grammar; Path : String) is
      Input  : File_Input;
      Reader : Tree_Reader;
   begin
      Put_Log (Log.Grammars, "Load " & Path);
      if not Exists (Path) or else Kind (Path) /= Directory then
         raise Invalid_Grammar with "Grammar directory does not exist.";
      end if;

      Set_Feature (Reader, Sax.Readers.Schema_Validation_Feature, True);

      Put_Log (Log.Grammars, ">> Features");
      Open (Path & "/features.xml", Input);
      Parse (Reader, Input);
      Close (Input);
      Toolkit.Features.Read (Get_Tree (Reader), G.Features);

      Put_Log (Log.Grammars, ">> Contexts");
      Open (Path & "/contexts.xml", Input);
      Parse (Reader, Input);
      Close (Input);
      Toolkit.Contexts.Read (Get_Tree (Reader), G.Features, G.Contexts);

      Put_Log (Log.Grammars, ">> Phonemes");
      Open (Path & "/phonemes.xml", Input);
      Parse (Reader, Input);
      Close (Input);
      Toolkit.Phonemes.Read
        (Get_Tree (Reader), G.Features, G.Contexts, G.Phonemes);

      Put_Log (Log.Grammars, ">> Syllables");
      Open (Path & "/syllables.xml", Input);
      Parse (Reader, Input);
      Close (Input);
      Toolkit.Syllables.Read
        (Get_Tree (Reader), G.Features, G.Contexts, G.Phonemes, G.Syllables);

      Free (Reader);
      Put_Log (Log.Grammars, "Done.");
   exception
      when Schema.XML_Not_Implemented =>
         Put_Line (Standard_Error, "ERROR: " & Get_Error_Message (Reader));
         raise Invalid_Grammar;
   end Load;

end Toolkit.Grammars;
