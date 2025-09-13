pragma Ada_2012;

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
   No_Cursor : exception renames Toolkit.Contexts_Impl.No_Cursor;
   subtype Cursor is Toolkit.Contexts_Impl.Cursor;
   --  Used to systematically navigate feature sets and elements

   --  function Scope (C : Cursor) return Context_Scope is abstract;
   --    Return the scope of the current cursor.
   --  function Before
   --   (C : Cursor) return Features.Feature_Set_List is abstract;
   --    Return previous elements’ features in an ordered list.
   --  function This (C : Cursor) return Features.Feature_Set is abstract;
   --    Return the features of the current element
   --  function After
   --   (C : Cursor) return Features.Feature_Set_List is abstract;
   --    Return subsequent elements’ features in an ordered list
   --  function Sub (C : Cursor) return Cursor'Class is abstract;
   --    Return the cursor corresponding to the current element’s child
   --    Or raise No_Cursor
   --  function Super (C : Cursor) return Cursor'Class is abstract;
   --    Return the cursor corresponding to the current element’s parent
   --    Or raise No_Cursor

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
