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

   type Morpheme is private;
   --  A semantics-bearing information structure
   --  This is equivalent to a morpheme

   package Morpheme_Lists is new Ada.Containers.Vectors (Positive, Morpheme);
   subtype Morpheme_List is Morpheme_Lists.Vector;

   -----------------
   -- Application --
   -----------------
   function Apply
     (FODB     : Morpheme_Database; Input : Morpheme;
      Features : Toolkit.Features.Feature_Set) return Morpheme;
   --  Return the first Morpheme that can apply 'Features' to 'Input'

   function Apply
     (FODB    : Morpheme_Database; Input : Morpheme;
      Symbols : Toolkit.Symbols.Abstract_Symbol_List) return Abstract_Morpheme;
   --  Return the first Morpheme matching 'Text' that can apply to 'Input'

   function Assemble (ML : Morpheme_List) return Symbols.Abstract_Symbol_List;
   --  Return the symbols associated with a list of Morphemes,
   --  in writing / pronunciation order
   --  TODO do better and return an annotated data set showing
   --  where the Morphemes are?

   ----------------
   -- Conversion --
   ----------------
   function To_Symbols (M : Morpheme) return Symbols.Abstract_Symbol_List;
   --  Return the symbols associated with a Morpheme as a list
   --  Discontinous Morphemes are represented using Null_Abstract_Symbol

   function To_Morphemes
     (MDB : Morpheme_Database; ASL : Toolkit.Symbols.Abstract_Symbol_List)
      return Morpheme_List;
   --  Return a best attempt at parsing an abstract symbol list into
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
     (Prefix, Stem, Suffix, Postfix, Circumfix, Triliteral, Transfix);
   --  TODO handle duplifixes, interfixes, and infixes

   type Morpheme_Definition is record
      Contexts : Toolkit.Contexts.Context_List;
      Symbols  : Toolkit.Symbols.Abstract_Symbol_List;
      Features : Toolkit.Features.Feature_Set;
      Kind     : Morpheme_Kind;
   end record;

   package MDL is new Ada.Containers.Vectors (Natural, Morpheme_Definition);
   type Morpheme_Database is new MDL.Vector with null record;

   type Morpheme is record
      C : FDL.Cursor;
   end record;
end Toolkit.Morphemes;
