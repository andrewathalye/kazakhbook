pragma Ada_2012;

with DOM.Core;

with Toolkit.Phonemes;
with Toolkit.Features;
with Toolkit.Contexts;
with Toolkit.Symbols_Impl;

with Ada.Containers.Vectors;

package Toolkit.Symbols is
   subtype Symbol_Database is Symbols_Impl.Symbol_Database;
   --  Linked to a Feature Database and Phoneme Database

   subtype Symbol_Instance is Symbols_Impl.Symbol_Instance;
   use type Symbol_Instance;
   --  A concrete occurrence of a symbol (with form and pronunciation)

   package Symbol_Lists is new Ada.Containers.Vectors
     (Positive, Symbol_Instance);
   subtype Symbol_List is Symbol_Lists.Vector;
   --  A list of symbols, sufficient to describe a word?
   --  TODO

   ----------------
   -- CONVERSION --
   ----------------
   function To_Ada
     (SDB : Symbol_Database; Symbol : String; Context : Contexts.Context)
      return Symbol_Instance renames
     Symbols_Impl.To_Ada;
   --  Return the Ada representation of a symbol

   function To_Ada (SDB : Symbol_Database; Text : String) return Symbol_List;
   --  Parse text as a list of symbols (each symbol is composed of
   --  the longest possible continuous span of characters that map
   --  to a symbol in the database)
   --  TODO what about context?

   function To_Unicode (S : Symbol_Instance) return String renames
     Symbols_Impl.To_Unicode;
   --  Print a symbol as a Unicode character

   function To_Unicode (S : Symbol_List) return String;
   --  Print a symbol list as a Unicode character

   ----------------
   -- INTERFACES --
   ----------------
   procedure Read
     (Doc : DOM.Core.Document; FDB : Features.Feature_Database;
      PDB : Phonemes.Phoneme_Database; SDB : out Symbol_Database) renames
     Symbols_Impl.Read;
   --  Read symbols from `Doc` and populate their properties
end Toolkit.Symbols;
