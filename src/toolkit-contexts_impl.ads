------------------------
-- NOT FOR DIRECT USE --
------------------------
pragma Ada_2012;

with DOM.Core;

with Toolkit.Features;

private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;

package Toolkit.Contexts_Impl is
   type Context_Scope is
     (Phoneme, Syllable, Morpheme, Symbol, Word, Clause, Sentence, Text);

   type Context_Database is private;
   type Context is private;

   Empty_Context : constant Context;

   Invalid_Context : exception;
   function Lookup (DB : Context_Database; Name : String) return Context;

   ------------
   -- CURSOR --
   ------------
   type Cursor is interface;
   Invalid_Cursor : exception;
   type Cursor_Placement is (First, Last);

   function Rescope
     (C : Cursor'Class; Target : Context_Scope; Placement : Cursor_Placement)
      return Cursor'Class;

   function Reparent
     (C : Cursor; Parent : Cursor'Class) return Cursor is abstract;
   --  Set a new parent for `C`

   function Prune
     (C : Cursor; Scope : Context_Scope) return Cursor is abstract;
   --  Cut off the inheritance tree at SCOPE

   function Scope (C : Cursor) return Context_Scope is abstract;
   --  Return the scope of this cursor

   function First (C : Cursor) return Cursor is abstract;
   --  The first cursor available at this level
   --  Result.Previous will raise Invalid_Cursor

   function Previous (C : Cursor) return Cursor is abstract;
   --  Cursor for the previous unit
   --  @exception Invalid_Cursor

   function Features
     (C : Cursor) return Toolkit.Features.Feature_Set is abstract;
   --  All features of the current unit

   function Next (C : Cursor) return Cursor is abstract;
   --  Cursor for the next unit
   --  @exception Invalid_Cursor

   function Last (C : Cursor) return Cursor is abstract;
   --  The last cursor available at this level
   --  Result.Next will raise Invalid_Cursor

   function Super (C : Cursor) return Cursor'Class is abstract;
   --  Cursor for the parent unit of `C`
   --  @exception Invalid_Cursor

   function Sub
     (C : Cursor; Placement : Cursor_Placement)
      return Cursor'Class is abstract;
   --  Cursor for the first or last unit of the child unit of C
   --  @exception Invalid_Cursor

   -----------
   -- STATE --
   -----------
   State_Incomplete : exception;
   function Applicable (Cur : Cursor'Class; Ctx : Context) return Boolean;

   function To_XML (L : Context) return String;

   Duplicate_Context : exception;
   procedure Read
     (Doc :     DOM.Core.Document; FDB : Toolkit.Features.Feature_Database;
      CDB : out Context_Database);
private
   type Context_Kind is (Anyprev, Anynext, Prev, Next, Unique, Super);

   type Context_Single (K : Context_Kind := Context_Kind'First) is record
      FS : Toolkit.Features.Feature_Set;
   end record;

   package CM is new Ada.Containers.Vectors (Natural, Context_Single);
   subtype Context_Multiple is CM.Vector;

   type Scoped_Context (Level, Within : Context_Scope := Context_Scope'First)
   is
   record
      C_Not : Context_Multiple;
      C_Any : Context_Multiple;
      C_All : Context_Multiple;
   end record;

   package Scoped_Context_Lists is new Ada.Containers.Vectors
     (Natural, Scoped_Context);
   subtype Scoped_Context_List is Scoped_Context_Lists.Vector;

   use type Scoped_Context_List;
   package Context_Databases is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Scoped_Context_List, Ada.Strings.Hash, "=");

   type Context_Database is new Context_Databases.Map with null record;
   type Context is new Context_Databases.Cursor;

   Empty_Context : constant Context := Context (Context_Databases.No_Element);
end Toolkit.Contexts_Impl;
