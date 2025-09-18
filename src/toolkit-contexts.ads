pragma Ada_2012;
pragma Extensions_Allowed (all);

with Ada.Containers.Vectors;

with DOM.Core;

with Toolkit.Features;
with Toolkit.Contexts_Impl;

package Toolkit.Contexts is
   subtype Context_Database is Toolkit.Contexts_Impl.Context_Database;
   --  A database of contexts under which features can occur

   subtype Context is Toolkit.Contexts_Impl.Context;
   --  A reference to a context defined in a database.
   --  Each context defines a set of conditions which must
   --  be met for a feature to occur.

   use type Context;
   package Context_Lists is new Ada.Containers.Vectors (Natural, Context);
   subtype Context_List is Context_Lists.Vector;
   --  A list of references to contexts.

   ----------------------
   -- CURSOR INTERFACE --
   ----------------------
   subtype Context_Scope is Toolkit.Contexts_Impl.Context_Scope;
   subtype Cursor_Placement is Toolkit.Contexts_Impl.Cursor_Placement;
   subtype Cursor is Toolkit.Contexts_Impl.Cursor;
   --  Used to systematically navigate feature sets and elements

   Invalid_Cursor : exception renames Contexts_Impl.Invalid_Cursor;
   function Rescope
     (C : Cursor'Class; Target : Context_Scope; Placement : Cursor_Placement)
     return Cursor'Class renames Contexts_Impl.Rescope;
   --  Set the scope of cursor `C` to target
   --  @param Placement
   --     Set where the cursor should be placed if the level must be reduced
   --  @exception Invalid_Cursor

   generic
      Cursor_Scope : Context_Scope;
      with package List is new Ada.Containers.Vectors (<>);
      with function Get_Features
        (LE : List.Element_Type) return Features.Feature_Set;
      with function Get_Sub
        (LE : List.Element_Type; Placement : Cursor_Placement)
         return Cursor'Class is (raise Invalid_Cursor);
   package Generic_Cursors is
      --  Cursor based on vectors. If a super-cursor is requested
      --  but no super has been set, a 'surrogate' cursor will be
      --  returned that has only a Sub
      type Generic_Cursor is new Cursor with private;

      function Create (LC : List.Cursor) return Generic_Cursor;

      Invalid_Super : exception;
      procedure Set_Super (C : in out Generic_Cursor; Super : Cursor'Class);
      --  @exception Invalid_Super
      --    Raised when `Super` does not have the correct scope

      overriding function Scope (C : Generic_Cursor) return Context_Scope;
      overriding function Prune (C : Generic_Cursor; Target : Context_Scope)
         return Generic_Cursor;
      --  Return a tree going up only to Target
      --  @exception Invalid_Super
      --    Raised when Target is a lower scope than C.Scope
      --  @exception Invalid_Cursor
      --    Raised when C does not have a Super scope, but one is requested

      overriding function Features
        (C : Generic_Cursor) return Toolkit.Features.Feature_Set;

      overriding function First (C : Generic_Cursor) return Generic_Cursor;
      overriding function Previous (C : Generic_Cursor) return Generic_Cursor;
      overriding function Next (C : Generic_Cursor) return Generic_Cursor;
      overriding function Last (C : Generic_Cursor) return Generic_Cursor;

      overriding function Sub
        (C : Generic_Cursor; Placement : Cursor_Placement) return Cursor'Class;
      overriding function Super (C : Generic_Cursor) return Cursor'Class;
   private
      type Generic_Cursor is new Cursor with record
         L_Cursor : List.Cursor;
         L_Super  : Contexts_Impl.Cursor_Holder;
      end record;
   end Generic_Cursors;

   -----------------
   -- SUBPROGRAMS --
   -----------------
   State_Incomplete : exception renames Toolkit.Contexts_Impl.State_Incomplete;
   function Applicable
     (Cur : Cursor'Class; Ctx : Context) return Boolean renames
     Toolkit.Contexts_Impl.Applicable;
   --  Return whether the given context applies to the current state.
   --  Raise State_Incomplete if not all data required is available

   ----------------
   -- CONVERSION --
   ----------------
   function To_XML (L : Context) return String renames
     Toolkit.Contexts_Impl.To_XML;
   --  Return an XML representation of L

   Invalid_Context : exception renames Contexts_Impl.Invalid_Context;
   function To_Ada (DB : Context_Database; XML : DOM.Core.Node) return Context;
   --  Return a Context given the XML reference

   function To_Ada
     (DB : Context_Database; XML : DOM.Core.Node_List) return Context_List;
   --  Return a Context List given nodes referencing contexts

   --------------------
   -- INITIALISATION --
   --------------------
   Duplicate_Context : exception renames Contexts_Impl.Duplicate_Context;
   procedure Read
     (Doc :     DOM.Core.Document; FDB : Features.Feature_Database;
      CDB : out Context_Database) renames
     Contexts_Impl.Read;
   --  Read all contexts from `Doc` and parse them
   --  Raise Duplicate_Context if a context name is repeated
end Toolkit.Contexts;
