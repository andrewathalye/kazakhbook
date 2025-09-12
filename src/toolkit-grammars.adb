pragma Ada_2012;

with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;

with Input_Sources.File; use Input_Sources.File;
with Sax.Readers;
with Schema.Dom_Readers; use Schema.Dom_Readers;

package body Toolkit.Grammars is
   ----------
   -- Load --
   ----------
   procedure Load (G : out Grammar; Path : String) is
      Input  : File_Input;
      Reader : Tree_Reader;
   begin
      Put_Line ("Load " & Path);
      if not Exists (Path) or else Kind (Path) /= Directory then
         raise Invalid_Grammar with "Grammar directory does not exist.";
      end if;

      Set_Feature (Reader, Sax.Readers.Schema_Validation_Feature, True);

      Put_Line (">> Features");
      Open (Path & "/features.xml", Input);
      Parse (Reader, Input);
      Close (Input);
      Toolkit.Features.Read (Get_Tree (Reader), G.Features);

      Put_Line (">> Contexts");
      Open (Path & "/contexts.xml", Input);
      Parse (Reader, Input);
      Close (Input);
      Toolkit.Contexts.Read (Get_Tree (Reader), G.Features, G.Contexts);

      Put_Line (">> Phonemes");
      Put_Line ("unimplemented");
      --  Open (Path & "/phonemes.xml", Input);
      --  Parse (Reader, Input);
      --  Close (Input);
      --  Toolkit.Phonemes.Read (Get_Tree (Reader), G.Features, G.Phonemes);

      Put_Line (">> Syllables");
      Put_Line ("unimplemented");

      Put_Line (">> Symbols");
      Put_Line ("unimplemented");
      --  Open (Path & "/symbols.xml", Input);
      --  Parse (Reader, Input);
      --  Close (Input);
      --  Toolkit.Symbols.Read
      --    (Get_Tree (Reader), G.Features, G.Phonemes, G.Symbols);

      Put_Line (">> Rules");
      Put_Line ("unimplemented");

      Put_Line (">> Morphemes");
      Put_Line ("unimplemented");

      Free (Reader);
      Put_Line ("Done.");
   exception
      when Schema.XML_Not_Implemented =>
         Put_Line ("ERROR: " & Get_Error_Message (Reader));
         raise Invalid_Grammar;
   end Load;

end Toolkit.Grammars;
