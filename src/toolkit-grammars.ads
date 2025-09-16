pragma Ada_2012;

with Toolkit.Features;
with Toolkit.Contexts;
with Toolkit.Phonemes;
--  with Toolkit.Symbols;

package Toolkit.Grammars is
   type Grammar is record
      Features : Toolkit.Features.Feature_Database;
      Contexts : Toolkit.Contexts.Context_Database;
      Phonemes : Toolkit.Phonemes.Phoneme_Database;
      --      Syllables : Toolkit.Syllables.Syllable_Database;
      --      Symbols   : Toolkit.Symbols.Symbol_Database;
      --      Rules     : Toolkit.Rules.Rule_Database;
      --      Morphemes : Toolkit.Morphemes.Morpheme_Database;
   end record;
   --  A database containing the information needed to parse sentences
   --  in an arbitrary language and segment words into phonemes, syllables,
   --  and morphemes.

   Invalid_Grammar : exception;
   procedure Load (G : out Grammar; Path : String);
   --  Raise Invalid_Grammar if the grammar could not be loaded.
   --  Debug information will be provided if available.
   --  `Path` should point to a directory containing grammar information
end Toolkit.Grammars;
