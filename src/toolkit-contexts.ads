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

   package Context_Lists is new Ada.Containers.Vectors (Natural, Context);
   subtype Context_List is Context_Lists.Vector;

   -----------------
   -- SUBPROGRAMS --
   -----------------
   function Superset (L, R : Context) return Boolean;
   --  Return whether L_x > R_x for each compoment of L and R

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
