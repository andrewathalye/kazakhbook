pragma Ada_2012;

with DOM.Core.Nodes;

package body Toolkit.Contexts is

   ------------
   -- To_Ada --
   ------------
   function To_Ada (DB : Context_Database; XML : DOM.Core.Node) return Context
   is
      use type DOM.Core.Node_Types;
      use type DOM.Core.Node;
      Attr : DOM.Core.Attr;
   begin
      if DOM.Core.Nodes.Node_Type (XML) /= DOM.Core.Element_Node then
         raise Invalid_Context;
      end if;

      Attr :=
        DOM.Core.Nodes.Get_Named_Item (DOM.Core.Nodes.Attributes (XML), "ref");

      if Attr = null then
         raise Program_Error;
      end if;

      return
        Toolkit.Contexts_Impl.Lookup (DB, DOM.Core.Nodes.Node_Value (Attr));
   end To_Ada;

   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (DB : Context_Database; XML : DOM.Core.Node_List) return Context_List
   is
      Result : Context_List;
   begin
      for I in 1 .. DOM.Core.Nodes.Length (XML) loop
         Result.Append (To_Ada (DB, DOM.Core.Nodes.Item (XML, I - 1)));
      end loop;

      return Result;
   end To_Ada;

end Toolkit.Contexts;
