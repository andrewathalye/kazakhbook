pragma Ada_2022;

with Ada.Strings.Unbounded;
with Ada.Characters.Handling;

with Ada.Text_IO; use Ada.Text_IO;

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

   -------------
   -- Rescope --
   -------------
   function Rescope
     (C : Cursor'Class; Target : Context_Scope; Placement : Cursor_Placement)
      return Cursor'Class
   is
   begin
      if Target = C.Scope then
         return C;
      elsif Target > C.Scope then
         return Rescope (C.Super, Target, Placement);
      else
         return Rescope (C.Sub (Placement), Target, Placement);
      end if;
   end Rescope;

   ----------------
   -- Applicable --
   ----------------
   function Applicable (Cur : Cursor'Class; Ctx : Context) return Boolean is
   begin
      Put_Line (To_XML (Ctx));

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
            Level_Cur     : constant Cursor'Class :=
              Rescope (Cur, SC.Level, First);
            Before, After : Toolkit.Features.Feature_Set_List;

            procedure Collect_Before (WC : Cursor'Class);
            --  Collect all preceding elements specified in
            --  the context from the cursor

            procedure Collect_Before (WC : Cursor'Class) is
               use type Toolkit.Features.Feature_Set;
               LWC : Cursor_Holder := Cursor_Holders.To_Holder (WC);
            begin
               if LWC.Element.Scope = SC.Level then
                  while LWC.Element.Features /=
                    Toolkit.Features.Null_Feature_Set
                  loop
                     LWC.Replace_Element (LWC.Element.Previous);
                     Before.Prepend (LWC.Element.Features);
                  end loop;
               else
                  loop
                     Collect_Before (LWC.Element.Sub (Last));
                     begin
                        LWC.Replace_Element (LWC.Element.Previous);
                     exception
                        when Invalid_Cursor =>
                           return;
                     end;
                  end loop;
               end if;
            end Collect_Before;

            procedure Collect_After (WC : Cursor'Class);
            --  Collect all successive elements specified
            --  in the context from the cursor

            procedure Collect_After (WC : Cursor'Class) is
               use type Toolkit.Features.Feature_Set;
               LWC : Cursor_Holder := Cursor_Holders.To_Holder (WC);
            begin
               if LWC.Element.Scope = SC.Level then
                  while LWC.Element.Features /=
                    Toolkit.Features.Null_Feature_Set
                  loop
                     LWC.Replace_Element (LWC.Element.Next);
                     After.Append (LWC.Element.Features);
                  end loop;
               else
                  loop
                     Collect_After (LWC.Element.Sub (First));
                     begin
                        LWC.Replace_Element (LWC.Element.Next);
                     exception
                        when Invalid_Cursor =>
                           return;
                     end;
                  end loop;
               end if;
            end Collect_After;

            function Apply (CS : Context_Single) return Boolean;
            --  Check whether a single context clause applies to
            --  the given cursor

            function Apply (CS : Context_Single) return Boolean is
            begin
               case CS.K is
                  when None =>
                     raise Invalid_Context;
                  when Anyprev =>
                     for FS of Before loop
                        if Toolkit.Features.Superset (FS, CS.FS) then
                           return True;
                        end if;
                     end loop;
                     return False;
                  when Anynext =>
                     for FS of After loop
                        if Toolkit.Features.Superset (FS, CS.FS) then
                           return True;
                        end if;
                     end loop;
                     return False;
                  when Prev =>
                     return
                       Toolkit.Features.Superset (Before.Last_Element, CS.FS);
                  when Next =>
                     return
                       Toolkit.Features.Superset (After.First_Element, CS.FS);
                  when Unique =>
                     for FS of Before loop
                        if Toolkit.Features.Superset (FS, CS.FS) then
                           return False;
                        end if;
                     end loop;
                     for FS of After loop
                        if Toolkit.Features.Superset (FS, CS.FS) then
                           return False;
                        end if;
                     end loop;
                     return True;
                  when Super =>
                     return
                       Toolkit.Features.Superset
                         (Level_Cur.Rescope (SC.Within, First).Features,
                          CS.FS);
               end case;
            end Apply;

         begin
            --------------------------
            -- Collect all Features --
            --------------------------
            Collect_Before (Level_Cur.Rescope (SC.Within, First));
            Collect_After (Level_Cur.Rescope (SC.Within, First));

            Put_Line (Toolkit.Features.To_XML (Before));
            Put_Line (Toolkit.Features.To_XML (After));

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
            Append (Buffer, Toolkit.Features.To_String (CS.FS));
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
      Append (Buffer, "</context>");

      return To_String (Buffer);
   end To_XML;

   ----------
   -- Read --
   ----------

   procedure Read
     (Doc :     DOM.Core.Document; FDB : Toolkit.Features.Feature_Database;
      CDB : out Context_Database)
   is
      use type DOM.Core.Node_Types;

      function Read_CS (X : DOM.Core.Element) return Context_Single;
      function Read_CS (X : DOM.Core.Element) return Context_Single is
         Result : Context_Single;
      begin
         Result :=
           (K  => Context_Kind'Value (DOM.Core.Nodes.Node_Name (X)),
            FS => Toolkit.Features.To_Ada (FDB, X));
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
            SCL.Append (SC);
         end loop;
         DOM.Core.Free (X_Scopes);
         CDB.Insert (DOM.Core.Nodes.Node_Value (X_Context_Id), SCL);
      end loop;

      DOM.Core.Free (X_Contexts);
   end Read;

end Toolkit.Contexts_Impl;
