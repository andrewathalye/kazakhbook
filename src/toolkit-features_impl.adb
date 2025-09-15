pragma Ada_2012;

with Ada.Strings.Fixed;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Attrs;
with DOM.Core.Nodes;

with Toolkit.Strings;

package body Toolkit.Features_Impl is
   ----------
   -- Hash --
   ----------
   function Hash (L : Feature_Name) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (L));
   end Hash;

   ---------------
   -- To_String --
   ---------------
   function To_String (Instance : Feature_Instance) return String is
      use type Value_Lists.Cursor;
   begin
      if Instance.Value = Value_Lists.No_Element then
         return String (Feature_Maps.Key (Instance.Feature));
      end if;

      return
        String (Feature_Maps.Key (Instance.Feature)) & "/" &
        String (Value_Lists.Element (Instance.Value));
   end To_String;

   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (DB : Feature_Database; Text : String) return Feature_Instance
   is
      use type Ada.Containers.Count_Type;

      Strings : constant Toolkit.Strings.Argument_List :=
        Toolkit.Strings.Split (Text, "/");
   begin
      --  Invalid specifications
      if Text'Length = 0 or Strings.Length > 2 then
         raise Constraint_Error;
      end if;

      --  [] must occur alone
      if Ada.Strings.Fixed.Index (Text, "[]") /= 0 then
         if Text = "[]" then
            return Null_Feature;
         else
            raise Constraint_Error with "Null feature occurring in a list";
         end if;
      end if;

      --  Symbolic reference to a phoneme
      --  These shouldn’t be decomposed
      if Text (Text'First) = '@' then
         raise Indeterminate_Feature with Text;
      end if;

      --  No feature value specified
      if Strings.Length = 1 then
         declare
            Name : constant Feature_Name := Feature_Name (Strings.Element (1));
         begin
            if not DB.Contains (Name) then
               raise Unknown_Feature with String (Name);
            end if;
            return (DB.Find (Name), Value_Lists.No_Element);
         end;
      end if;

      --  Otherwise
      declare
         Name : constant Feature_Name := Feature_Name (Strings.Element (1));
         Value : constant Feature_Value := Feature_Value (Strings.Element (2));
      begin
         if not DB.Contains (Name) then
            raise Unknown_Feature with String (Name);
         end if;

         if not DB (Name).Contains (Value) then
            raise Unknown_Value with Text;
         end if;

         return (DB.Find (Name), DB (Name).Find (Value));
      end;
   end To_Ada;

   ----------
   -- Read --
   ----------
   procedure Read (Doc : DOM.Core.Document; DB : out Feature_Database) is
      N : Node;
      X_Features : Node_List :=
        Documents.Get_Elements_By_Tag_Name (Doc, "feature");
      X_Feature_ID : Attr;
      X_Values : Node_List;
      X_Value_ID : Attr;

      Values : Value_List;
   begin
      DB.Clear;

      Add_Feature :
      for Index in 1 .. Nodes.Length (X_Features) loop
         N := Nodes.Item (X_Features, Index - 1);
         X_Feature_ID := Nodes.Get_Named_Item (Nodes.Attributes (N), "id");

         Values.Clear;
         X_Values := Elements.Get_Elements_By_Tag_Name (N, "value");
         Add_Value :
         for Index in 1 .. Nodes.Length (X_Values) loop
            N := Nodes.Item (X_Values, Index - 1);
            X_Value_ID := Nodes.Get_Named_Item (Nodes.Attributes (N), "id");
            Values.Append (Feature_Value (Attrs.Value (X_Value_ID)));
         end loop Add_Value;

         Free (X_Values);

         declare
            Feature_ID : constant Feature_Name :=
              Feature_Name (Attrs.Value (X_Feature_ID));
         begin
            if DB.Contains (Feature_ID) then
               raise Duplicate_Feature with String (Feature_ID);
            end if;
            DB.Insert (Feature_ID, Values);
         end;
      end loop Add_Feature;

      Free (X_Features);
   end Read;

end Toolkit.Features_Impl;
