pragma Ada_2012;

with DOM.Core;
with Toolkit.Contexts;
with Toolkit.Symbols;
with Toolkit.Features;

with Ada.Containers.Vectors;

package Toolkit.Morphemes is
   type Morpheme_Database is private;
   --  Stores data about known forms
   --  Linked to a Symbol_Database and a Feature_Database

   type Morpheme_Instance is private;
   --  Stores the data required to determine the
   --  pronunciation and features of a morph in
   --  a given context

   package Morpheme_Lists is new Ada.Containers.Vectors (Positive, Morpheme_Instance);
   subtype Morpheme_List is Morpheme_Lists.Vector;
   --  A list of morpheme instances. This is the basis for a word.

   type Abstract_Morpheme is private;
   --  Stores data about meaning and features outside
   --  of a morpheme context.

   package Abstract_Morpheme_Lists is new Ada.Containers.Vectors (Positive, Abstract_Morpheme);
   subtype Abstract_Morpheme is Abstract_Morpheme_Lists.Vector;

   -----------------
   -- Application --
   -----------------
   function Apply
     (FODB     : Morpheme_Database; Input : Abstarct_Morpheme;
      Features : Toolkit.Features.Feature_Set) return Abstract_Morpheme;
   --  Return the first Morpheme that can apply 'Features' to 'Input'

   function Apply
     (FODB    : Morpheme_Database; Input : Abstract_Morpheme;
      Symbols : Toolkit.Symbols.Abstract_Symbol_List) return Abstract_Morpheme;
   --  Return the first Morpheme matching 'Text' that can apply to 'Input'

   function Resolve (ML : Abstract_Morpheme_List) return Morpheme_List;
   --  Determine the necessary morphs, given a list of abstract morphemes
   --  and context.

   ----------------
   -- Conversion --
   ----------------
   function To_Symbols (M : Morpheme_Instance) return Symbols.Abstract_Symbol_List;
   --  Return the symbols associated with a morph as a list
   --  Discontinous Morphemes are represented using Null_Abstract_Symbol

   function To_Morphemes
     (MDB : Morpheme_Database; ASL : Toolkit.Symbols.Symbol_List)
      return Morpheme_List;
   --  Return a best attempt at parsing a symbol list into
   --  morphemes

   function To_XML (M : Morpheme) return String;
   --  Return the XML representation of a morpheme

   --------------------
   -- Initialisation --
   --------------------
   procedure Read
     (Doc : DOM.Core.Document; FDB : Features.Feature_Database;
      SDB : Symbols.Symbol_Database; MDB : out Morpheme_Database);
   --  Read all Morpheme definitions from an XML document
private
   type Morpheme_Kind is
     (Prefix, Stem, Suffix, Postfix, Circumfix, Interfix, Triliteral, Transfix);
   --  TODO handle duplifixes, and infixes

   type Morph is record
      Contexts : Toolkit.Contexts.Context_List;
      Symbols : Toolkit.Symbols.Symbol_List;
   end record;

   package Morph_Lists is new Ada.Containers.Vectors (Natural, Morph);
   subtype Morph_List is Morph_Lists.Vector;

   type Morpheme_Definition is record
      Contexts : Toolkit.Contexts.Context_List;
      Morphs   : Morph_List;
      Kind     : Morpheme_Kind;
      Features : Toolkit.Features.Feature_Set;
   end record;

   package MDL is new Ada.Containers.Vectors (Natural, Morpheme_Definition);
   type Morpheme_Database is new MDL.Vector with null record;

   type Abstract_Morpheme is record
      C : MDL.Cursor;
   end record;

   type Morpheme_Instance is record
      Morpheme : MDL.Cursor;
      Instance : Morph_Lists.Cursor;
   end record;
end Toolkit.Morphemes;
