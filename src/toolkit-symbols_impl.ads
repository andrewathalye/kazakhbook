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

   function To_Ada
     (SDB : Symbol_Database; Symbol : String; Context : Contexts.Context)
      return Symbol_Instance;

   function To_Unicode (S : Symbol_Instance) return String;

   procedure Read
     (Doc : DOM.Core.Document; FDB : Features.Feature_Database;
      PDB : Phonemes.Phoneme_Database; SDB : out Symbol_Database);

private
   type Form is record
      Contexts : Toolkit.Contexts.Context_List;
      Text     : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Form_Lists is new Ada.Containers.Vectors (Natural, Form);
   subtype Form_List is Form_Lists.Vector;

   type Symbol is record
      Forms          : Form_List;
      Pronunciations : Phonemes.Phoneme_Set;
   end record;

   package Symbol_Databases is new Ada.Containers.Vectors (Natural, Symbol);
   type Symbol_Database is new Symbol_Databases.Vector with null record;

   type Symbol_Instance is record
      Form          : Form_Lists.Cursor;
      Pronunciation : Phonemes.Phoneme_Sets.Cursor;
   end record;
end Toolkit.Symbols_Impl;
