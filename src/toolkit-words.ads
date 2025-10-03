pragma Ada_2012;

with Toolkit.Phonemes;
with Toolkit.Symbols;
with Toolkit.Features;

package Toolkit.Words is
   type Word_Database is private;
   --  A list of words and associated features

   type Word is private;
   --  The basic unit of grammar

   function Transcribe (W : Word) return String;
   --  Transcribe a word

   function To_Ada (WDB : Word_Database; Text : String) return 
private
   type Word is record
      Symbols  : Toolkit.Symbols.Symbol_List;
      Morphemes : Toolkit.Morphemes.Morph_List;
      Features : Toolkit.Features.Feature_Set;
   end record;
end Toolkit.Words;
