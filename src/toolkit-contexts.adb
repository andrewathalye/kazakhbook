pragma Ada_2012;

with DOM.Core.Nodes;

package body Toolkit.Contexts is
   ---------------------
   -- Generic_Cursors --
   ---------------------
   package body Generic_Cursors is
      function Create (LC : List.Cursor) return Generic_Cursor is
         Result : Generic_Cursor;
      begin
         Result.L_Cursor := LC;
         return Result;
      end Create;

      procedure Set_Super (C : in out Generic_Cursor; Super : Cursor'Class) is
      begin
         C.L_Super.Replace_Element (Super);
      end Set_Super;

      overriding function Scope (C : Generic_Cursor) return Context_Scope is
        (Cursor_Scope);

      function Features
        (C : Generic_Cursor) return Toolkit.Features.Feature_Set
      is
      begin
         if not List.Has_Element (C.L_Cursor) then
            return Toolkit.Features.Null_Feature_Set;
         end if;

         return Get_Features (List.Element (C.L_Cursor));
      end Features;

      function Previous (C : Generic_Cursor) return Generic_Cursor is
         Result : Generic_Cursor;
      begin
         Result := Create (List.Previous (C.L_Cursor));
         Result.Set_Super (C.L_Super.Element);
         return Result;
      end Previous;

      function Next (C : Generic_Cursor) return Generic_Cursor is
         Result : Generic_Cursor;
      begin
         Result := Create (List.Next (C.L_Cursor));
         Result.Set_Super (C.L_Super.Element);
         return Result;
      end Next;

      function Sub
        (C : Generic_Cursor; Placement : Cursor_Placement) return Cursor'Class
      is
      begin
         return Get_Sub (List.Element (C.L_Cursor), Placement);
      end Sub;

      function Super (C : Generic_Cursor) return Cursor'Class is
      begin
         return C.L_Super.Element;
      end Super;
   end Generic_Cursors;

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
