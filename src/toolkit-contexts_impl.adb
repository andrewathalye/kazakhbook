pragma Ada_2012;

pragma Warnings (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Holders;

with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;

package body Toolkit.Contexts_Impl is

   ------------
   -- Lookup --
   ------------
   function Lookup (DB : Context_Database; Name : String) return Context is
   begin
      if not DB.Contains (Name) then
         raise Invalid_Context with Name;
      end if;

      return Context (DB.Find (Name));
   end Lookup;

   ----------------
   -- Applicable --
   ----------------
   function Applicable (Cur : Cursor'Class; Ctx : Context) return Boolean is
      function Get_Level_Cur
        (Cur : Cursor'Class; Level : Context_Scope) return Cursor'Class;
      --  Get `Cur` at the correct level for the scope

      function Get_Level_Cur
        (Cur : Cursor'Class; Level : Context_Scope) return Cursor'Class
      is
      begin
         if Level = Cur.Scope then
            return Cur;
         elsif Level > Cur.Scope then
            return Get_Level_Cur (Cur.Super, Level);
         else
            return Get_Level_Cur (Cur.Sub, Level);
         end if;
      end Get_Level_Cur;
   begin
      ------------------------
      -- Process Each Scope --
      ------------------------
      for SC of Context_Databases.Element (Context_Databases.Cursor (Ctx)) loop
         --  Check breadth of Within and Level
         if SC.Within <= SC.Level then
            raise Invalid_Context with "Within must be broader than Level";
         end if;

         Level :
         declare
            Level_Cur : Cursor'Class := Get_Level_Cur (Cur, SC.Level);
            Before    : Features.Feature_Set_List;
            After     : Features.Feature_Set_List;

            function Apply (CS : Context_Single) return Boolean;
            function Apply (CS : Context_Single) return Boolean
            is
            begin
               case CS.K is
                  when None =>
                     raise Invalid_Context;
                  when Anyprev =>
                     for FS of Before loop
                        if Features.Superset (FS, CS.FS) then
                           return True;
                        end if;
                     end loop;
                     return False;
                  when Anynext =>
                     for FS of After loop
                        if Features.Superset (FS, CS.FS) then
                           return True;
                        end if;
                     end loop;
                     return False;
                  when Prev =>
                     return Features.Superset (Before.Last_Element, CS.FS);
                  when Next =>
                     return Features.Superset (After.First_Element, CS.FS);
                  when Unique =>
                     for FS of Before loop
                        if Features.Superset (FS, CS.FS) then
                           return False;
                        end if;
                     end loop;
                     for FS of After loop
                        if Features.Superset (FS, CS.FS) then
                           return False;
                        end if;
                     end loop;
                     return True;
                  when Super =>
                     return Features.Superset (Level_Cur.Super.This, CS.FS);
               end case;
            end Apply;

         begin
            --------------------------
            -- Collect all Features --
            --------------------------
            Before := Collect_Before (SC.Level, SC.Within);
            After  := Collect_After (SC.Level, SC.Within);

            ------------------------
            -- Check all Features --
            ------------------------
            for CS of SC.C_Not loop
               if Apply (CS) then
                  return False;
               end if;
            end loop;

            for CS of SC.C_Any loop
               if Apply (CS) then
                  goto Any;
               end if;
            end loop;
            return False;
            <<Any>>

            for CS of SC.C_All loop
               if not Apply (CS) then
                  return False;
               end if;
            end loop;
         end Level;
      end loop;

      ---------------------------------
      --  True iff all Scopes Passed --
      ---------------------------------
      return True;
   end Applicable;

   ------------
   -- To_XML --
   ------------
   function To_XML (L : Context) return String is
      use Ada.Strings.Unbounded;
      use Ada.Characters.Handling;

      Buffer : Unbounded_String;
      SCL    : Scoped_Context_List;

      procedure Append_CM (CM : Context_Multiple);
      procedure Append_CM (CM : Context_Multiple) is
      begin
         for CS of CM loop
            if CS.K = None then
               raise Invalid_Context;
            end if;
            Append (Buffer, "<" & To_Lower (CS.K'Image) & ">");
            Append (Buffer, Features.To_String (CS.FS));
            Append (Buffer, "</" & To_Lower (CS.K'Image) & ">");
         end loop;
      end Append_CM;
   begin
      SCL := Context_Databases.Element (Context_Databases.Cursor (L));

      --  ID
      Append (Buffer, "<context id=" & ASCII.Quotation);
      Append (Buffer, Context_Databases.Key (Context_Databases.Cursor (L)));
      Append (Buffer, ">");

      --  Per scope (level, within)
      for SC of SCL loop
         Append (Buffer, "<scope level=" & ASCII.Quotation);
         Append (Buffer, To_Lower (SC.Level'Image) & ASCII.Quotation);
         Append (Buffer, " within=" & ASCII.Quotation);
         Append (Buffer, To_Lower (SC.Within'Image) & ASCII.Quotation & ">");

         --  Not, Any, All
         if not SC.C_Not.Is_Empty then
            Append (Buffer, "<not>");
            Append_CM (SC.C_Not);
            Append (Buffer, "</not>");
         end if;
         if not SC.C_Any.Is_Empty then
            Append (Buffer, "<any>");
            Append_CM (SC.C_Any);
            Append (Buffer, "</any>");
         end if;

         Append_CM (SC.C_All);
         Append (Buffer, "</scope>");
      end loop;
      Append (Buffer, "</context");

      return To_String (Buffer);
   end To_XML;

   ----------
   -- Read --
   ----------

   procedure Read
     (Doc :     DOM.Core.Document; FDB : Features.Feature_Database;
      CDB : out Context_Database)
   is
      use type DOM.Core.Node_Types;

      function Read_CS (X : DOM.Core.Element) return Context_Single;
      function Read_CS (X : DOM.Core.Element) return Context_Single is
         Result : Context_Single;
      begin
         Result :=
           (K  => Context_Kind'Value (DOM.Core.Nodes.Node_Name (X)),
            FS => Features.To_Ada (FDB, X));
         return Result;
      end Read_CS;

      function Read_CM (X : DOM.Core.Element) return Context_Multiple;
      function Read_CM (X : DOM.Core.Element) return Context_Multiple is
         Result    : Context_Multiple;
         X_CS_List : DOM.Core.Node_List;
         X_CS      : DOM.Core.Node;
      begin
         X_CS_List := DOM.Core.Nodes.Child_Nodes (X);
         for I in 1 .. DOM.Core.Nodes.Length (X_CS_List) loop
            X_CS := DOM.Core.Nodes.Item (X_CS_List, I - 1);
            if DOM.Core.Nodes.Node_Type (X_CS) = DOM.Core.Element_Node then
               Result.Append (Read_CS (X_CS));
            end if;
         end loop;
         DOM.Core.Free (X_CS_List);
         return Result;
      end Read_CM;

      SCL : Scoped_Context_List;
      SC  : Scoped_Context;

      X_Contexts       : DOM.Core.Node_List;
      X_Context        : DOM.Core.Element;
      X_Context_Id     : DOM.Core.Attr;
      X_Scopes         : DOM.Core.Node_List;
      X_Scope          : DOM.Core.Element;
      X_Scope_Attrs    : DOM.Core.Named_Node_Map;
      X_Scope_Level    : DOM.Core.Attr;
      X_Scope_Within   : DOM.Core.Attr;
      X_Scope_Children : DOM.Core.Node_List;
      X_Scope_Child    : DOM.Core.Node;
   begin
      -----------------------
      -- Read all Contexts --
      -----------------------
      CDB.Clear;
      X_Contexts :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name (Doc, "context");
      for X_Context_Index in 1 .. DOM.Core.Nodes.Length (X_Contexts) loop
         X_Context    := DOM.Core.Nodes.Item (X_Contexts, X_Context_Index - 1);
         X_Context_Id :=
           DOM.Core.Nodes.Get_Named_Item
             (DOM.Core.Nodes.Attributes (X_Context), "id");

         ---------------------
         -- Read all Scopes --
         ---------------------
         SCL.Clear;
         X_Scopes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name (X_Context, "scope");
         for X_Scope_Index in 1 .. DOM.Core.Nodes.Length (X_Scopes) loop
            X_Scope := DOM.Core.Nodes.Item (X_Scopes, X_Scope_Index - 1);
            X_Scope_Attrs  := DOM.Core.Nodes.Attributes (X_Scope);
            X_Scope_Level  :=
              DOM.Core.Nodes.Get_Named_Item (X_Scope_Attrs, "level");
            X_Scope_Within :=
              DOM.Core.Nodes.Get_Named_Item (X_Scope_Attrs, "within");

            SC :=
              (Level  =>
                 Context_Scope'Value
                   (DOM.Core.Nodes.Node_Value (X_Scope_Level)),
               Within =>
                 Context_Scope'Value
                   (DOM.Core.Nodes.Node_Value (X_Scope_Within)),
               others => <>);

            ------------------------
            -- Read Not, Any, All --
            ------------------------
            SC.C_Not.Clear;
            SC.C_Any.Clear;
            SC.C_All.Clear;
            X_Scope_Children := DOM.Core.Nodes.Child_Nodes (X_Scope);
            for X_Scope_Child_Index in
              1 .. DOM.Core.Nodes.Length (X_Scope_Children)
            loop
               X_Scope_Child :=
                 DOM.Core.Nodes.Item
                   (X_Scope_Children, X_Scope_Child_Index - 1);
               if DOM.Core.Nodes.Node_Name (X_Scope_Child) = "not" then
                  SC.C_Not := Read_CM (X_Scope_Child);
               elsif DOM.Core.Nodes.Node_Name (X_Scope_Child) = "any" then
                  SC.C_Any := Read_CM (X_Scope_Child);
               elsif DOM.Core.Nodes.Node_Type (X_Scope_Child) =
                 DOM.Core.Element_Node
               then
                  SC.C_All.Append (Read_CS (X_Scope_Child));
               end if;
            end loop;
            DOM.Core.Free (X_Scope_Children);
         end loop;
         DOM.Core.Free (X_Scopes);
         CDB.Insert (DOM.Core.Nodes.Node_Value (X_Context_Id), SCL);
      end loop;

      DOM.Core.Free (X_Contexts);
   end Read;

end Toolkit.Contexts_Impl;
