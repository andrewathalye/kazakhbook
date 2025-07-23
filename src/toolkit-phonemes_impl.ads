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
   type Abstract_Phoneme is private;

   function Resolve
     (PDB     : Phoneme_Database; AP : Abstract_Phoneme;
      Context : Contexts.Context) return Phoneme_Instance;

   Unknown_Phoneme : exception;
   function To_XML (Instance : Phoneme_Instance) return String;
   function To_Ada
     (FDB : Features.Feature_Database; PDB : Phoneme_Database; Text : String)
      return Abstract_Phoneme;

   Duplicate_Phoneme : exception;
   procedure Read
     (Doc :     DOM.Core.Document; FDB : Toolkit.Features.Feature_Database;
      PDB : out Phoneme_Database);

   function Transcribe (P : Phoneme_Instance) return String;

   --------------
   -- INTERNAL --
   --------------
   function Dump_Features
     (AP : Abstract_Phoneme) return Toolkit.Features.Feature_Set;
   function Dump_Features
     (PI : Phoneme_Instance) return Toolkit.Features.Feature_Set;
   --  Return all features associated with an AP or PI

   Null_Phoneme : constant Phoneme_Instance;
   --  No phoneme, an invalid phoneme. Must not be returned to user code.
private
   type Phone is record
      Contexts : Toolkit.Contexts.Context_List;
      Sounds   : Toolkit.Features.Feature_Set_List;
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

   Null_Phoneme : constant Phoneme_Instance :=
     (Phoneme_Maps.No_Element, Phone_Lists.No_Element);

   type Abstract_Phoneme is record
      Phoneme  : Phoneme_Maps.Cursor;
      Features : Toolkit.Features.Feature_Set;
   end record;
end Toolkit.Phonemes_Impl;
