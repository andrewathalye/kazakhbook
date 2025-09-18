pragma Ada_2012;

with DOM.Core.Nodes;

package body Toolkit.Contexts is
   ---------------------
   -- Generic_Cursors --
   ---------------------
   package body Generic_Cursors is
      function Find_First (C : List.Cursor) return List.Cursor;
      function Find_First (C : List.Cursor) return List.Cursor is
         LC : List.Cursor := C;
      begin
         while List.Has_Element (List.Previous (LC)) loop
            LC := List.Previous (LC);
         end loop;

         return LC;
      end Find_First;

      function Find_Last (C : List.Cursor) return List.Cursor;
      function Find_Last (C : List.Cursor) return List.Cursor is
         LC : List.Cursor := C;
      begin
         while List.Has_Element (List.Next (LC)) loop
            LC := List.Next (LC);
         end loop;

         return LC;
      end Find_Last;

      --------------------
      -- Generic Cursor --
      --------------------
      function Create (LC : List.Cursor) return Generic_Cursor is
         Result : Generic_Cursor;
      begin
         Result.L_Cursor := LC;
         return Result;
      end Create;

      procedure Set_Super (C : in out Generic_Cursor; Super : Cursor'Class) is
         use type Context_Scope;
      begin
         if C.Scope = Context_Scope'Last
           or else Super.Scope /= Context_Scope'Succ (C.Scope)
         then
            raise Invalid_Super;
         end if;
         C.L_Super.Replace_Element (Super);
      end Set_Super;

      function Scope (C : Generic_Cursor) return Context_Scope is
        (Cursor_Scope);

      function Prune
        (C : Generic_Cursor; Target : Context_Scope) return Generic_Cursor
      is
         use type Context_Scope;
      begin
         if Target = C.Scope then
            return (C.L_Cursor, L_Super => <>);
         elsif Target < C.Scope then
            raise Invalid_Super;
         end if;

         return
           (C.L_Cursor,
            Contexts_Impl.Cursor_Holders.To_Holder (C.Super.Prune (Target)));
      end Prune;

      function Features
        (C : Generic_Cursor) return Toolkit.Features.Feature_Set
      is
      begin
         return Get_Features (List.Element (C.L_Cursor));
      end Features;

      function First (C : Generic_Cursor) return Generic_Cursor is
         Result : Generic_Cursor;
      begin
         if not C.L_Super.Is_Empty then
            return Generic_Cursor (C.Super.First.Sub (Contexts_Impl.First));
         end if;

         Result         := Create (Find_First (C.L_Cursor));
         Result.L_Super := C.L_Super;
         return Result;
      end First;

      function Previous (C : Generic_Cursor) return Generic_Cursor is
         Result : Generic_Cursor;
      begin
         if not List.Has_Element (List.Previous (C.L_Cursor)) then
            return Generic_Cursor (C.Super.Previous.Sub (Contexts_Impl.Last));
         end if;

         Result         := Create (List.Previous (C.L_Cursor));
         Result.L_Super := C.L_Super;
         return Result;
      end Previous;

      function Next (C : Generic_Cursor) return Generic_Cursor is
         Result : Generic_Cursor;
      begin
         if not List.Has_Element (List.Next (C.L_Cursor)) then
            return Generic_Cursor (C.Super.Next.Sub (Contexts_Impl.Last));
         end if;

         Result         := Create (List.Next (C.L_Cursor));
         Result.L_Super := C.L_Super;
         return Result;
      end Next;

      function Last (C : Generic_Cursor) return Generic_Cursor is
         Result : Generic_Cursor;
      begin
         if not C.L_Super.Is_Empty then
            return Generic_Cursor (C.Super.Last.Sub (Contexts_Impl.Last));
         end if;

         Result         := Create (Find_Last (C.L_Cursor));
         Result.L_Super := C.L_Super;
         return Result;
      end Last;

      function Sub
        (C : Generic_Cursor; Placement : Cursor_Placement) return Cursor'Class
      is
      begin
         return Get_Sub (List.Element (C.L_Cursor), Placement);
      end Sub;

      function Super (C : Generic_Cursor) return Cursor'Class is
      begin
         if C.L_Super.Is_Empty then
            raise Invalid_Cursor;
         end if;

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
      if DOM.Core.Nodes.Node_Type (XML) /= DOM.Core.Element_Node
        or else DOM.Core.Nodes.Node_Name (XML) /= "context"
      then
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
