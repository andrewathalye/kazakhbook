pragma Ada_2012;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;

with DOM.Core;

package Toolkit.Features is
   type Feature_Database is tagged private;

   type Feature_Instance is private;
   --  Identifies the properties of a specific feature instance.
   --  This is valid only so long as the feature map is visible.

   function To_XML (Instance : Feature_Instance) return String;
   --  Return the XML identifier of a feature instance

   Unknown_Feature, Unknown_Value : exception;
   function To_Ada
     (DB : Feature_Database; XML : String) return Feature_Instance;
   --  Convert an XML identifier to a feature instance
   --  Raise Unknown_Feature if the feature can’t be resolved.
   --  Raise Unknown_Value if the value can’t be resolved.

   Duplicate_Feature : exception;
   procedure Read (Doc : DOM.Core.Document; DB : out Feature_Database);
   --  Read all features from `Doc` and parse them
   --  Raise Duplicate_Feature if a feature is repeated
private
   type Feature_Name is new String;
   type Feature_Value is new String;

   package Value_Lists is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => Feature_Value);
   subtype Value_List is Value_Lists.Vector;
   use type Value_List;

   function Hash (L : Feature_Name) return Ada.Containers.Hash_Type;

   package Feature_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => Feature_Name, Element_Type => Value_List, Hash => Hash,
      Equivalent_Keys => "=");
   type Feature_Database is new Feature_Maps.Map with null record;

   type Feature_Instance is record
      Feature : Feature_Maps.Cursor;
      Value   : Value_Lists.Cursor;
   end record;
end Toolkit.Features;
