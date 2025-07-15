pragma Ada_2012;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Toolkit.Features;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Toolkit.Phonemes is
   use type Toolkit.Features.Feature_Instance;
   package Feature_Sets is new Ada.Containers.Vectors (Natural, Toolkit.Features.Feature_Instance);
   subtype Feature_Set is Feature_Sets.Vector;
   use type Feature_Set;

   type Context is record
      Before : Feature_Set;
      After  : Feature_Set;
      Global : Feature_Set;
   end record;

   package Context_Lists is new Ada.Containers.Vectors (Natural, Context);
   subtype Context_List is Context_Lists.Vector;

   package FRL_Lists is new Ada.Containers.Vectors
     (Natural, Feature_Set);
   subtype Feature_Set_List is FRL_Lists.Vector;

   type Phone is record
      Contexts : Context_List;
      Sounds   : Feature_Set_List;
      IPA      : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Phone_Lists is new Ada.Containers.Vectors (Natural, Phone);
   subtype Phone_List is Phone_Lists.Vector;
   use type Phone_List;

   type Phoneme_Name is new String;
   function Hash (L : Phoneme_Name) return Ada.Containers.Hash_Type;

   package Phoneme_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Phoneme_Name,
      Element_Type    => Phone_List,
      Hash            => Hash,
      Equivalent_Keys => "=");
   subtype Phoneme_Map is Phoneme_Maps.Map;
end Toolkit.Phonemes;
