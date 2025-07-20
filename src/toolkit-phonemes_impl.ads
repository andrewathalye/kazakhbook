------------------------
-- NOT FOR DIRECT USE --
------------------------

pragma Ada_2012;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Toolkit.Features;
with Toolkit.Contexts;

with DOM.Core;

package Toolkit.Phonemes_Impl is
   type Phoneme_Database is private;
   type Phoneme_Instance is private;

   Unknown_Phoneme : exception;
   function Resolve_Set
     (DB      : Phoneme_Database; Required_Set : Features.Feature_Set;
      Context : Contexts.Context) return Phoneme_Instance;

   function Resolve_Text
     (FDB         : Features.Feature_Database; PDB : Phoneme_Database;
      Description : String; Context : Contexts.Context)
      return Phoneme_Instance;

   function To_XML (Instance : Phoneme_Instance) return String;

   Duplicate_Phoneme : exception;
   procedure Read
     (Doc :     DOM.Core.Document; FDB : Toolkit.Features.Feature_Database;
      PDB : out Phoneme_Database);
private
   type Phone is record
      Contexts : Toolkit.Contexts.Context_List;
      Sounds   : Features.Feature_Set_List;
      IPA      : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Phone_Lists is new Ada.Containers.Vectors (Natural, Phone);
   subtype Phone_List is Phone_Lists.Vector;
   use type Phone_List;

   type Phoneme_Name is new String;
   function Hash (L : Phoneme_Name) return Ada.Containers.Hash_Type;

   package Phoneme_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => Phoneme_Name, Element_Type => Phone_List, Hash => Hash,
      Equivalent_Keys => "=");
   type Phoneme_Database is new Phoneme_Maps.Map with null record;

   type Phoneme_Instance is record
      Phoneme  : Phoneme_Maps.Cursor;
      Instance : Phone_Lists.Cursor;
   end record;
end Toolkit.Phonemes_Impl;
