--------------------------------
-- NOT INTEDED FOR DIRECT USE --
--------------------------------

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with DOM.Core;

with Toolkit.Features;
with Toolkit.Contexts;
with Toolkit.Phonemes;

package Toolkit.Symbols_Impl is
   type Symbol_Database is private;
   type Symbol_Instance is private;
   type Abstract_Symbol is private;
   Null_Abstract_Symbol : constant Abstract_Symbol;

   Unknown_Symbol : exception;
   function To_Ada
     (SDB : Symbol_Database; Text : String) return Abstract_Symbol;

   function To_Unicode (S : Symbol_Instance) return String;

   procedure Read
     (Doc : DOM.Core.Document; FDB : Features.Feature_Database;
      PDB : Phonemes.Phoneme_Database; SDB : out Symbol_Database);

   function To_Phonemes (X : Symbol_Instance) return Phonemes.Phoneme_List;

   Indeterminate_Symbol : exception;
   function Resolve
     (PDB            : Phonemes.Phoneme_Database; AS : Abstract_Symbol;
      Symbol_Context : Contexts.Context; Phoneme_Context : Contexts.Context)
      return Symbol_Instance;

   --------------
   -- INTERNAL --
   --------------
   Null_Symbol : constant Symbol_Instance;
   --  Must not be returned to user code

   function Dump_Features (X : Abstract_Symbol) return Features.Feature_Set;
   function Dump_Features (X : Symbol_Instance) return Features.Feature_Set;
   --  Return all features associated with the symbol
private
   type Form is record
      Contexts : Toolkit.Contexts.Context_List;
      Text     : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Form_Lists is new Ada.Containers.Vectors (Natural, Form);
   subtype Form_List is Form_Lists.Vector;

   type Pronunciation is record
      Contexts          : Toolkit.Contexts.Context_List;
      Abstract_Phonemes : Phonemes.Abstract_Phoneme_List;
   end record;

   package Pronunciation_Lists is new Ada.Containers.Vectors
     (Natural, Pronunciation);
   subtype Pronunciation_List is Pronunciation_Lists.Vector;

   type Symbol_Definition is record
      Forms          : Form_List;
      Pronunciations : Pronunciation_List;
      Features       : Toolkit.Features.Feature_Set;
   end record;

   package Symbol_Databases is new Ada.Containers.Vectors
     (Natural, Symbol_Definition);
   type Symbol_Database is new Symbol_Databases.Vector with null record;

   type Symbol_Instance is record
      Symbol   : Symbol_Databases.Cursor;
      Form     : Form_Lists.Cursor;
      Phonemes : Toolkit.Phonemes.Phoneme_List;
   end record;

   Null_Symbol : constant Symbol_Instance := (others => <>);

   type Abstract_Symbol is record
      Symbol : Symbol_Databases.Cursor;
      Form   : Form_Lists.Cursor;
   end record;

   Null_Abstract_Symbol : constant Abstract_Symbol := (others => <>);
end Toolkit.Symbols_Impl;
