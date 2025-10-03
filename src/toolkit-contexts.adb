pragma Ada_2012;

with DOM.Core.Nodes;

package body Toolkit.Contexts is

   ---------------------
   -- Isolated Cursor --
   ---------------------
   type Isolated_Cursor (CS : Context_Scope) is new Cursor with null record;

   overriding function Reparent
     (C : Isolated_Cursor; Parent : Cursor'Class) return Isolated_Cursor is
     (C);

   overriding function Prune
     (C : Isolated_Cursor; Scope : Context_Scope) return Isolated_Cursor is
     (C);

   overriding function Scope (C : Isolated_Cursor) return Context_Scope is
     (C.CS);

   overriding function Features
     (C : Isolated_Cursor) return Toolkit.Features.Feature_Set is
     (Toolkit.Features.Null_Feature_Set);

   overriding function First (C : Isolated_Cursor) return Isolated_Cursor is
     (C);
   overriding function Previous (C : Isolated_Cursor) return Isolated_Cursor is
     (raise Invalid_Cursor);
   overriding function Next (C : Isolated_Cursor) return Isolated_Cursor is
     (raise Invalid_Cursor);
   overriding function Last (C : Isolated_Cursor) return Isolated_Cursor is
     (C);

   overriding function Sub
     (C : Isolated_Cursor; Placement : Cursor_Placement) return Cursor'Class is
     (raise Invalid_Cursor);
   overriding function Super (C : Isolated_Cursor) return Cursor'Class is
     (raise Invalid_Cursor);

   function Isolated (Scope : Context_Scope) return Cursor'Class is
     (Isolated_Cursor'(CS => Scope));

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

      function Reparent
        (C : Generic_Cursor; Parent : Cursor'Class) return Generic_Cursor
      is
         use type Context_Scope;
      begin
         if Parent.Scope <= C.Scope then
            raise Invalid_Cursor with "Parent cannot have scope <= Child";
         end if;

         return
           (L_Parent => Cursor_Holders.To_Holder (Parent),
            L_Cursor => C.L_Cursor);
      end Reparent;

      function Prune
        (C : Generic_Cursor; Scope : Context_Scope) return Generic_Cursor
      is
         use type Context_Scope;
      begin
         if Scope > C.Scope then
            return C.Reparent (Isolated (Scope));
         elsif Scope = C.Scope then
            return (L_Cursor => C.L_Cursor, others => <>);
         else
            raise Invalid_Cursor with "Cannot prune lower than scope";
         end if;
      end Prune;

      function Scope (C : Generic_Cursor) return Context_Scope is
        (Cursor_Scope);

      function Features
        (C : Generic_Cursor) return Toolkit.Features.Feature_Set
      is
      begin
         return Get_Features (List.Element (C.L_Cursor));
      end Features;

      function First (C : Generic_Cursor) return Generic_Cursor is
      begin
         return (C.L_Parent, Find_First (C.L_Cursor));
      end First;

      function Previous (C : Generic_Cursor) return Generic_Cursor is
      begin
         if not List.Has_Element (List.Previous (C.L_Cursor)) then
            return Generic_Cursor (C.Super.Previous.Sub (Contexts_Impl.Last));
         end if;

         return (C.L_Parent, List.Previous (C.L_Cursor));
      end Previous;

      function Next (C : Generic_Cursor) return Generic_Cursor is
      begin
         if not List.Has_Element (List.Next (C.L_Cursor)) then
            return Generic_Cursor (C.Super.Next.Sub (Contexts_Impl.First));
         end if;

         return (C.L_Parent, List.Next (C.L_Cursor));
      end Next;

      function Last (C : Generic_Cursor) return Generic_Cursor is
      begin
         return (C.L_Parent, Find_Last (C.L_Cursor));
      end Last;

      function Sub
        (C : Generic_Cursor; Placement : Cursor_Placement) return Cursor'Class
      is
      begin
         case Placement is
            when Contexts_Impl.First =>
               return Get_Child (List.Element (C.L_Cursor)).Reparent (C);
            when Contexts_Impl.Last =>
               return Get_Child (List.Element (C.L_Cursor)).Last.Reparent (C);
         end case;
      end Sub;

      function Super (C : Generic_Cursor) return Cursor'Class is
      begin
         if C.L_Parent.Is_Empty then
            raise Invalid_Cursor with "No parent element";
         end if;

         return C.L_Parent.Element;
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
        or else
        (DOM.Core.Nodes.Node_Name (XML) /= "context" and
         DOM.Core.Nodes.Node_Name (XML) /= "initial_context" and
         DOM.Core.Nodes.Node_Name (XML) /= "final_context")
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
