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

   function Get_Features (PI : Phoneme_Instance) return Features.Feature_Set;
   function Get_Features (AP : Abstract_Phoneme) return Features.Feature_Set;

   Indeterminate_Phoneme : exception;
   use all type Toolkit.Contexts.Context_Scope;
   function Resolve
     (PDB : Phoneme_Database; AP : Abstract_Phoneme;
      Cur : Contexts.Cursor'Class) return Phoneme_Instance with
     Pre => Cur.Scope = Phoneme;

   function Abstractise (Instance : Phoneme_Instance) return Abstract_Phoneme;
   procedure Add (AP : in out Abstract_Phoneme; FS : Features.Feature_Set);
   procedure Subtract
     (AP : in out Abstract_Phoneme; FS : Features.Feature_Set);

   Unknown_Phoneme : exception;
   function To_XML (Instance : Phoneme_Instance) return String;

   function To_Ada
     (FDB : Features.Feature_Database; PDB : Phoneme_Database; Text : String)
      return Abstract_Phoneme;

   Duplicate_Phoneme : exception;
   procedure Read
     (Doc : DOM.Core.Document; FDB : Features.Feature_Database;
      CDB : Contexts.Context_Database; PDB : out Phoneme_Database);

   function Transcribe (P : Phoneme_Instance) return String;

   function Transcribe
     (PDB : Phoneme_Database; IPA : String; C : in out Natural)
      return Abstract_Phoneme;
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
