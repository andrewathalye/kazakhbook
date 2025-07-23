pragma Ada_2012;

with Ada.Containers.Vectors;

with DOM.Core;

with Toolkit.Features;

package Toolkit.Contexts is
   type Context is record
      Before : Features.Feature_Set;
      After  : Features.Feature_Set;
      Global : Features.Feature_Set;
   end record;
   --  Describes the immediate feature context around an element

   Null_Context : constant Context := (others => <>);

   package Context_Lists is new Ada.Containers.Vectors (Natural, Context);
   subtype Context_List is Context_Lists.Vector;

   --------------
   -- GENERICS --
   --------------
   generic
      type Index is range <>;
      type Element is private;
      with package Lists is new Ada.Containers.Vectors (Index, Element);
      with function Features (X : Element) return Features.Feature_Set;
   function Derive_Context
     (Cursor : Lists.Cursor; External_Context : Contexts.Context)
      return Contexts.Context;
   --  Return the context of an element based upon its adjacent elements in a
   --  list and an external context.

   -----------------
   -- SUBPROGRAMS --
   -----------------
   function Superset (L, R : Context) return Boolean;
   --  Return whether all parts of L contain all features in R

   function Has_Superset (L : Context; R : Context_List) return Boolean;
   --  Return whether R contains a Context that fulfills Superset (L, R_x)

   ----------------
   -- CONVERSION --
   ----------------
   function To_XML (L : Context) return String;
   --  Return an XML representation of L
   --  <context>
   --    <before>...</before>
   --    <after>...</after>
   --    <global>...</global>
   --  </context>

   function To_Ada
     (DB : Features.Feature_Database; XML : DOM.Core.Node) return Context;
   function To_Ada
     (DB : Features.Feature_Database; XML : DOM.Core.Node_List)
      return Context_List;
   --  Return a Context (list) from an XML description
end Toolkit.Contexts;
