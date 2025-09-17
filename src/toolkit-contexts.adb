pragma Ada_2012;

with DOM.Core.Nodes;

package body Toolkit.Contexts is
   ----------------------
   -- Surrogate Cursor --
   ----------------------
   package Surrogate_Cursors is
      type Surrogate_Cursor (<>) is new Cursor with private;
      --  Used to represent the end of a list or of a hierarchy

      Null_Cursor : constant Surrogate_Cursor;

      function Create
        (Previous, Next, Super, Sub : Cursor'Class := Null_Cursor)
         return Surrogate_Cursor;
   private

      type Surrogate_Cursor is new Cursor with record
         L_Scope                            : Context_Scope;
         L_Previous, L_Next, L_Sub, L_Super : Contexts_Impl.Cursor_Holder;
      end record;

      overriding function Scope (C : Surrogate_Cursor) return Context_Scope;

      overriding function Features
        (C : Surrogate_Cursor) return Toolkit.Features.Feature_Set is
        (Toolkit.Features.Null_Feature_Set);

      overriding function First (C : Surrogate_Cursor) return Cursor'Class;
      overriding function Previous (C : Surrogate_Cursor) return Cursor'Class;
      overriding function Next (C : Surrogate_Cursor) return Cursor'Class;
      overriding function Last (C : Surrogate_Cursor) return Cursor'Class;

      overriding function Sub
        (C : Surrogate_Cursor; Placement : Cursor_Placement)
         return Cursor'Class;

      overriding function Super (C : Surrogate_Cursor) return Cursor'Class;

      Null_Cursor : constant Surrogate_Cursor :=
        (L_Scope => Contexts_Impl.None, L_Previous => <>, L_Next => <>,
         L_Sub   => <>, L_Super => <>);
   end Surrogate_Cursors;

   package body Surrogate_Cursors is
      function Not_Null (C : Cursor'Class) return Boolean;
      function Not_Null (C : Cursor'Class) return Boolean is
      begin
         pragma Warnings (Off, "redundant conversion");
         if C in Surrogate_Cursor'Class
           and then Surrogate_Cursor (C) = Null_Cursor
         then
            return False;
         end if;
         pragma Warnings (On, "redundant conversion");

         return True;
      end Not_Null;

      function Create
        (Previous, Next, Super, Sub : Cursor'Class := Null_Cursor)
         return Surrogate_Cursor
      is
         Valid  : Boolean;
         Result : Surrogate_Cursor;
      begin
         if Not_Null (Previous) then
            Result.L_Scope := Previous.Scope;
            Result.L_Previous.Replace_Element (Previous);
            Valid := True;
         end if;

         if Not_Null (Next) then
            Result.L_Scope := Next.Scope;
            Result.L_Next.Replace_Element (Next);
            Valid := True;
         end if;

         if Not_Null (Sub) then
            Result.L_Scope := Context_Scope'Succ (Sub.Scope);
            Result.L_Sub.Replace_Element (Sub);
            Valid := True;
         end if;

         if Not_Null (Super) then
            Result.L_Scope := Context_Scope'Pred (Super.Scope);
            Result.L_Super.Replace_Element (Super);
            Valid := True;
         end if;

         if not Valid then
            raise Constraint_Error
              with "At least one valid cursor must be specified.";
         end if;

         return Result;
      end Create;

      function Scope (C : Surrogate_Cursor) return Context_Scope is
        (C.L_Scope);

      function First (C : Surrogate_Cursor) return Cursor'Class is
      begin
         if C.L_Previous.Is_Empty then
            return C;
         end if;

         return C.L_Previous.Element.First;
      end First;

      function Previous (C : Surrogate_Cursor) return Cursor'Class is
      begin
         if C.L_Previous.Is_Empty then
            raise Invalid_Cursor;
         end if;

         return C.L_Previous.Element;
      end Previous;

      function Next (C : Surrogate_Cursor) return Cursor'Class is
      begin
         if C.L_Next.Is_Empty then
            raise Invalid_Cursor;
         end if;

         return C.L_Next.Element;
      end Next;

      function Last (C : Surrogate_Cursor) return Cursor'Class is
      begin
         if C.L_Next.Is_Empty then
            return C;
         end if;

         return C.L_Next.Element.Last;
      end Last;

      function Sub
        (C : Surrogate_Cursor; Placement : Cursor_Placement)
         return Cursor'Class
      is
      begin
         if C.L_Sub.Is_Empty then
            return Create (Super => C);
         end if;

         case Placement is
            when Contexts_Impl.First =>
               return C.L_Sub.Element.First;
            when Contexts_Impl.Last =>
               return C.L_Sub.Element.Last;
         end case;
      end Sub;

      function Super (C : Surrogate_Cursor) return Cursor'Class is
      begin
         if C.L_Super.Is_Empty then
            return Create (Sub => C);
         end if;

         return C.L_Super.Element;
      end Super;

   end Surrogate_Cursors;

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
      begin
         C.L_Super.Replace_Element (Super);
      end Set_Super;

      overriding function Scope (C : Generic_Cursor) return Context_Scope is
        (Cursor_Scope);

      function Features
        (C : Generic_Cursor) return Toolkit.Features.Feature_Set
      is
      begin
         return Get_Features (List.Element (C.L_Cursor));
      end Features;

      function First (C : Generic_Cursor) return Cursor'Class is
         Result : Generic_Cursor;
      begin
         Result := Create (Find_First (C.L_Cursor));
         Result.Set_Super (C.Super);
         return Result;
      end First;

      function Previous (C : Generic_Cursor) return Cursor'Class is
         Result : Generic_Cursor;
      begin
         if not List.Has_Element (List.Previous (C.L_Cursor)) then
            return Surrogate_Cursors.Create (Super => C.Super, Next => C);
         end if;

         Result := Create (List.Previous (C.L_Cursor));
         Result.Set_Super (C.Super);
         return Result;
      end Previous;

      function Next (C : Generic_Cursor) return Cursor'Class is
         Result : Generic_Cursor;
      begin
         if not List.Has_Element (List.Next (C.L_Cursor)) then
            return Surrogate_Cursors.Create (Super => C.Super, Previous => C);
         end if;

         Result := Create (List.Next (C.L_Cursor));
         Result.Set_Super (C.Super);
         return Result;
      end Next;

      function Last (C : Generic_Cursor) return Cursor'Class is
         Result : Generic_Cursor;
      begin
         Result := Create (Find_Last (C.L_Cursor));
         Result.Set_Super (C.Super);
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
            return Surrogate_Cursors.Create (Sub => C);
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
