pragma Ada_2012;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Toolkit.Features;
with Toolkit.Contexts;

with DOM.Core;

package Toolkit.Phonemes is
   type Phoneme_Database is private;
   --  Database of all known phonemes.
   --  Linked to a Feature Database

   type Phoneme_Instance is private;
   --  One concrete phoneme realisation

   ----------------
   -- RESOLUTION --
   ----------------
   Unknown_Phoneme : exception;
   function Resolve_Set
     (DB      : Phoneme_Database; Required_Set : Features.Feature_Set;
      Context : Contexts.Context) return Phoneme_Instance;
   --  Resolve a set of sounds to a specific phoneme in a given context
   --  Optionally provide a phoneme for resolution

   function Resolve_Text
     (FDB         : Features.Feature_Database; PDB : Phoneme_Database;
      Description : String; Context : Contexts.Context)
      return Phoneme_Instance;
   --  Resolve a textual feature list to a specific phoneme in a given context

   ----------------
   -- CONVERSION --
   ----------------
   function To_XML (Instance : Phoneme_Instance) return String;
   --  Convert a phoneme instance to an XML feature description
   --  This omits any features inherent to a phoneme

   --------------------
   -- INITIALISATION --
   --------------------
   Duplicate_Phoneme : exception;
   procedure Read
     (Doc : DOM.Core.Document; FDB : Toolkit.Features.Feature_Database;
      PDB : out Phoneme_Database);
   --  Read a phoneme database from an XML file
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
      Extra    : Features.Feature_Set_List;
   end record;
end Toolkit.Phonemes;
