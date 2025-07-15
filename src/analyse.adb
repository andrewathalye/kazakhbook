pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

with Toolkit.Features;
with Toolkit.Phonemes;
--  with Toolkit.Symbols;

with DOM.Core;              use DOM.Core;
with Input_Sources.File;    use Input_Sources.File;
with Sax.Readers;
with Schema.Dom_Readers;    use Schema.Dom_Readers;
with Schema.Schema_Readers; use Schema.Schema_Readers;

procedure Analyse is
   Schema : Schema_Reader;
   Input  : File_Input;
   Reader : Tree_Reader;
   Doc    : DOM.Core.Document;

   Features : Toolkit.Features.Feature_Database;
   Phonemes : Toolkit.Phonemes.Phoneme_Database;
   --   Symbols  : Toolkit.Symbols.Symbol_List;
begin
   Put_Line ("Load Schema");
   Open ("xml/grammar.xsd", Input);
   Parse (Schema, Input);
   Close (Input);

   Set_Grammar (Reader, Get_Grammar (Schema));
   Set_Feature (Reader, Sax.Readers.Schema_Validation_Feature, True);

   Put_Line ("Load Data");
   Open ("xml/grammar.xml", Input);
   Parse (Reader, Input);
   Close (Input);
   Doc := Get_Tree (Reader);

   Put_Line ("Parse Data");
   Put_Line (">> Features");
   Toolkit.Features.Read (Doc, Features);
   Put_Line (">> Phonemes");
   Toolkit.Phonemes.Read (Doc, Features, Phonemes);
   --   Toolkit.Symbols.Read (Doc, Symbols);

   Put_Line ("Done");
   Free (Reader);
end Analyse;
