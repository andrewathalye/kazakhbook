pragma Ada_2012;

with Ada.Containers.Vectors;

with Toolkit.Features;

package Toolkit.Contexts is
   use type Toolkit.Features.Feature_Instance;
   package Feature_Sets is new Ada.Containers.Vectors
     (Natural, Toolkit.Features.Feature_Instance);
   subtype Feature_Set is Feature_Sets.Vector;
   --  A set of features (accurately describes a monophthong or morpheme)
   use type Feature_Set;

   type Context is record
      Before : Feature_Set;
      After  : Feature_Set;
      Global : Feature_Set;
   end record;
   --  Describes the immediate feature context around an element

   package Context_Lists is new Ada.Containers.Vectors (Natural, Context);
   subtype Context_List is Context_Lists.Vector;

   package Feature_Set_Lists is new Ada.Containers.Vectors
     (Natural, Feature_Set);
   subtype Feature_Set_List is Feature_Set_Lists.Vector;

   -----------------
   -- SUBPROGRAMS --
   -----------------
   function Subtract (L, R : Feature_Set) return Feature_Set;
   --  Returns the list of elements only in L, but not R

   function Superset (L, R : Feature_Set) return Boolean;
   --  Returns whether L contains all elements in R

   function Superset (L, R : Context) return Boolean;
   --  Return whether L_x > R_x for each compoment of L and R

   use type Ada.Containers.Count_Type;
   function Subtract
     (L, R : Feature_Set_List) return Feature_Set_List with
     Pre => L.Length = R.Length;
   --  Return the list where each element equals L_x - R_x

   function Superset (L, R : Feature_Set_List) return Boolean;
   --  Return whether L and R have the same number of sets
   --  and whether L_x > R_x for each of these sets.
end Toolkit.Contexts;
