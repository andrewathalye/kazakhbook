pragma Ada_2012;

with Ada.Containers.Vectors;

with DOM.Core;

with Toolkit.Features_Impl;

package Toolkit.Features is
   subtype Feature_Database is Features_Impl.Feature_Database;
   --  Stores opaque data about known features

   subtype Feature_Instance is Features_Impl.Feature_Instance;
   --  Identifies the properties of a specific feature instance.
   --  This is valid only so long as the feature map is visible.

   use type Feature_Instance;
   package Feature_Sets is new Ada.Containers.Vectors
     (Natural, Feature_Instance);
   subtype Feature_Set is Feature_Sets.Vector;
   --  A set of features sufficient to describe a single monophthong
   --  or morpheme

   use type Feature_Set;
   package Feature_Set_Lists is new Ada.Containers.Vectors
     (Natural, Feature_Set);
   subtype Feature_Set_List is Feature_Set_Lists.Vector;
   --  A list of arbitrary feature sets. This could be used to
   --  describe a word, a sound stream, or other tertiary object.

   ----------------
   -- OPERATIONS --
   ----------------
   function Subtract (L, R : Feature_Set) return Feature_Set;
   --  Returns the list of elements only in L, but not R

   function Superset (L, R : Feature_Set) return Boolean;
   --  Returns whether L contains all elements in R

   function Flatten (L : Feature_Set_List) return Feature_Set;
   --  Return the set of all features in all sets in L
   --  (this removes any duplicate features)

   ----------------
   -- CONVERSION --
   ----------------
   function To_String (Instance : Feature_Instance) return String renames
     Features_Impl.To_String;
   function To_String (Set : Feature_Set) return String;
   --  Return a string representation of a feature or feature set

   function To_XML (Set : Feature_Set) return String;
   function To_XML (List : Feature_Set_List) return String;
   --  Return the XML identifiers of a feature set (list)
   --  <provide>...</provide>
   --  <provide>...</provide>

   Unknown_Feature       : exception renames Features_Impl.Unknown_Feature;
   Unknown_Value         : exception renames Features_Impl.Unknown_Value;
   Indeterminate_Feature : exception renames
     Features_Impl.Indeterminate_Feature;
   function To_Ada
     (DB : Feature_Database; Text : String) return Feature_Instance renames
     Features_Impl.To_Ada;
   --  Convert a textual identifier to a feature instance
   --  Raise Unknown_Feature if the feature can’t be resolved.
   --  Raise Unknown_Value if the value can’t be resolved.
   --  Raise Indeterminate_Feature if the value requires external data
   --  (in this toolkit that means it is based on a phoneme)

   function To_Ada
     (DB : Feature_Database; XML : DOM.Core.Node) return Feature_Set;
   function To_Ada
     (DB : Feature_Database; XML : DOM.Core.Node_List) return Feature_Set_List;
   --  Convert an XML feature set (list) to Ada
   --  Accepts either <provide> or <require> lists

   --------------------
   -- INITIALISATION --
   --------------------
   Duplicate_Feature : exception renames Features_Impl.Duplicate_Feature;
   procedure Read (Doc : DOM.Core.Document; DB : out Feature_Database) renames
     Features_Impl.Read;
   --  Read all features from `Doc` and parse them
   --  Raise Duplicate_Feature if a feature is repeated
end Toolkit.Features;
