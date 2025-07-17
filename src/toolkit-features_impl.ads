------------------------
-- NOT FOR DIRECT USE --
------------------------

pragma Ada_2012;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;

with DOM.Core;

package Toolkit.Features_Impl is
   type Feature_Database is private;
   type Feature_Instance is private;

   Unknown_Feature, Unknown_Value, Indeterminate_Feature : exception;
   Duplicate_Feature : exception;

   function To_String (Instance : Feature_Instance) return String;
   function To_Ada
     (DB : Feature_Database; Text : String) return Feature_Instance;
   procedure Read (Doc : DOM.Core.Document; DB : out Feature_Database);
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

end Toolkit.Features_Impl;
