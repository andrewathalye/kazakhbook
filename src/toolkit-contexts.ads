pragma Ada_2012;

with Ada.Containers.Indefinite_Holders;
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

   Empty_Context : Context renames Toolkit.Contexts_Impl.Empty_Context;
   --  An empty context that always applies

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
   --  If no cursor exists in that position

   function Isolated (Scope : Context_Scope) return Cursor'Class;
   --  An entirely isolated cursor, used for debugging and isolation
   --  It will return Null_Feature_Set for any query and Invalid_Cursor
   --  for any request to Sub or Super

   generic
      Cursor_Scope : Context_Scope;
      with package List is new Ada.Containers.Vectors (<>);
      with function Get_Features
        (LE : List.Element_Type) return Toolkit.Features.Feature_Set;
      with function Get_Child (LE : List.Element_Type) return Cursor'Class;
   package Generic_Cursors is
      type Generic_Cursor is new Cursor with private;

      function Create (LC : List.Cursor) return Generic_Cursor;

      overriding function Reparent
        (C : Generic_Cursor; Parent : Cursor'Class) return Generic_Cursor;

      overriding function Prune
        (C : Generic_Cursor; Scope : Context_Scope) return Generic_Cursor;

      overriding function Scope (C : Generic_Cursor) return Context_Scope;

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
      pragma Warnings (Off, "is not referenced");
      function Never_Equal (L, R : Cursor'Class) return Boolean is (False);
      pragma Warnings (On, "is not referenced");

      package Cursor_Holders is new Ada.Containers.Indefinite_Holders
        (Cursor'Class, Never_Equal);
      subtype Cursor_Holder is Cursor_Holders.Holder;

      type Generic_Cursor is new Cursor with record
         L_Parent : Cursor_Holder;
         L_Cursor : List.Cursor;
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
     (Doc :     DOM.Core.Document; FDB : Toolkit.Features.Feature_Database;
      CDB : out Context_Database) renames
     Contexts_Impl.Read;
   --  Read all contexts from `Doc` and parse them
   --  Raise Duplicate_Context if a context name is repeated

end Toolkit.Contexts;
