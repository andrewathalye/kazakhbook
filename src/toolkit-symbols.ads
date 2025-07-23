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
   --  A list of symbols
   --  Sufficient to describe the pronunciation and graphical representation
   --  of a word

   subtype Abstract_Symbol is Symbols_Impl.Abstract_Symbol;
   use type Abstract_Symbol;
   --  An occurrence of a symbol with no fixed pronunciation

   package Abstract_Symbol_Lists is new Ada.Containers.Vectors
     (Positive, Abstract_Symbol);
   subtype Abstract_Symbol_List is Abstract_Symbol_Lists.Vector;

   ----------------
   -- RESOLUTION --
   ----------------
   Indeterminate_Symbol : exception renames Symbols_Impl.Indeterminate_Symbol;
   function Resolve
     (PDB            : Phonemes.Phoneme_Database; AS : Abstract_Symbol;
      Symbol_Context : Contexts.Context; Phoneme_Context : Contexts.Context)
      return Symbol_Instance renames
     Symbols_Impl.Resolve;
   --  Determine the pronunciation of a symbol based upon the surrounding
   --  symbols and phonemes

   function Resolve
     (PDB : Phonemes.Phoneme_Database; ASL : Abstract_Symbol_List;
      External_Symbol_Context  : Contexts.Context;
      External_Phoneme_Context : Contexts.Context) return Symbol_List;
   --  Resolve a list of abstract symbols based upon external context

   ----------------
   -- CONVERSION --
   ----------------
   Unknown_Symbol : exception renames Symbols_Impl.Unknown_Symbol;
   function To_Ada
     (SDB : Symbol_Database; Text : String) return Abstract_Symbol renames
     Symbols_Impl.To_Ada;
   --  Parse text as a single symbol. The result must be resolved.

   function To_Ada
     (SDB : Symbol_Database; Text : String) return Abstract_Symbol_List;
   --  Parse text as a list of symbols (each symbol is composed of
   --  the longest possible continuous span of characters that map
   --  to a symbol in the database)
   --  The result must be resloved

   function To_Unicode (S : Symbol_Instance) return String renames
     Symbols_Impl.To_Unicode;
   --  Print a symbol as a Unicode character

   function To_Unicode (S : Symbol_List) return String;
   --  Print a symbol list as a Unicode string

   function To_Phonemes
     (S : Symbol_Instance) return Phonemes.Phoneme_List renames
     Symbols_Impl.To_Phonemes;
   function To_Phonemes (SL : Symbol_List) return Phonemes.Phoneme_List;
   --  Print the phonemes associated with a symbol instance or list

   ----------------
   -- INTERFACES --
   ----------------
   procedure Read
     (Doc : DOM.Core.Document; FDB : Features.Feature_Database;
      PDB : Phonemes.Phoneme_Database; SDB : out Symbol_Database) renames
     Symbols_Impl.Read;
   --  Read symbols from `Doc` and populate their properties
end Toolkit.Symbols;
