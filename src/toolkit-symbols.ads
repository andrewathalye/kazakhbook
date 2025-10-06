pragma Ada_2012;

with DOM.Core;

with Toolkit.Phonemes;
with Toolkit.Features;
with Toolkit.Contexts;
with Toolkit.Symbols_Impl;

with Ada.Containers.Vectors;

package Toolkit.Symbols is
   subtype Symbol_Database is Symbols_Impl.Symbol_Database;
   --  Used to resolve text into `Symbols` based upon
   --  Context, Features, and Phonemes

   subtype Symbol_Instance is Symbols_Impl.Symbol_Instance;
   use type Symbol_Instance;
   --  A concrete occurrence of a symbol:
   --  - with textual representation
   --  - with a concrete phonemic representation

   package Symbol_Lists is new Ada.Containers.Vectors
     (Positive, Symbol_Instance);
   subtype Symbol_List is Symbol_Lists.Vector;
   --  A list of symbols, sufficient to describe the orthography of a word
   subtype Abstract_Symbol is Symbols_Impl.Abstract_Symbol;
   --  An occurrence of a symbol with no fixed pronunciation

   Null_Abstract_Symbol :
     Abstract_Symbol renames Symbols_Impl.Null_Abstract_Symbol;
   --  A specifically-invalid abstract symbol representing discontinuity.
   --  Attempts to resolve this will result in an exception.

   use type Abstract_Symbol;

   package Abstract_Symbol_Lists is new Ada.Containers.Vectors
     (Positive, Abstract_Symbol);
   subtype Abstract_Symbol_List is Abstract_Symbol_Lists.Vector;

   -------------
   -- CURSORS --
   -------------
   function To_Cursor (Pos : Symbol_Lists.Cursor) return Contexts.Cursor'Class;
   function To_Cursor
     (Pos : Abstract_Symbol_Lists.Cursor) return Contexts.Cursor'Class;
   --  Each position of the cursor within the list can be used to query
   --  features associated with the symbol

   ----------------
   -- RESOLUTION --
   ----------------
   Indeterminate_Symbol : exception renames Symbols_Impl.Indeterminate_Symbol;
   function Resolve
     (PDB : Phonemes.Phoneme_Database; AS : Abstract_Symbol;
      Cur : Contexts.Cursor'Class) return Symbol_Instance renames
     Symbols_Impl.Resolve;
   --  Determine the pronunciation of a symbol based upon the surrounding
   --  symbols and phonemes.
   --
   --  @param Cur must have scope SYMBOL and point to to AS
   --  @exception Indeterminate_Symbol if a symbol isn’t allowed to occur
   --  in a given position because of contextual rules

   function Resolve
     (PDB : Phonemes.Phoneme_Database; ASL : Abstract_Symbol_List;
      Cur : Contexts.Cursor'Class) return Symbol_List;
   --  Resolve a list of abstract symbols based upon external context
   --
   --  @param Cur must have scope SYMBOL and point to ASL'First
   --  @exception Indeterminate_Symbol if a symbol isn’t allowed to occur
   --  in the given context and no alternative can be decided

   ----------------
   -- CONVERSION --
   ----------------
   Unknown_Symbol : exception renames Symbols_Impl.Unknown_Symbol;
   function To_Ada
     (SDB : Symbol_Database; Text : String) return Abstract_Symbol renames
     Symbols_Impl.To_Ada;
   --  Parse text as a single symbol. The result must be resolved.
   --  @exception Unknown_Symbol the symbol is not recognised

   function To_Ada
     (SDB : Symbol_Database; Text : String) return Abstract_Symbol_List;
   --  Parse text as a list of symbols (each symbol is composed of
   --  the longest possible continuous span of characters that map
   --  to a symbol in the database)
   --  The result must be resolved
   --  @exception Unknown_Symbol a symbol was not recognised

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
   --  In the case of a word, this should then be syllabified

   ----------------
   -- INTERFACES --
   ----------------
   procedure Read
     (Doc :     DOM.Core.Document; FDB : Features.Feature_Database;
      CDB :     Contexts.Context_Database; PDB : Phonemes.Phoneme_Database;
      SDB : out Symbol_Database) renames
     Symbols_Impl.Read;
   --  Read symbols from `Doc` and populate their properties
end Toolkit.Symbols;
