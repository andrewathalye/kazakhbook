pragma Ada_2012;

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with DOM.Core.Nodes;
with Toolkit.XML;

package body Toolkit.Features is

   --------------
   -- Subtract --
   --------------
   function Subtract (L, R : Feature_Set) return Feature_Set is
      Result : Feature_Set;
   begin
      for Feature of L loop
         if not R.Contains (Feature) then
            Result.Append (Feature);
         end if;
      end loop;

      return Result;
   end Subtract;

   --------------
   -- Superset --
   --------------
   function Superset (L, R : Feature_Set) return Boolean is
   begin
      for Feature of R loop
         if not L.Contains (Feature) then
            return False;
         end if;
      end loop;

      return True;
   end Superset;

   -------------
   -- Flatten --
   -------------
   function Flatten (L : Feature_Set_List) return Feature_Set is
      Result : Feature_Set;
   begin
      for Set of L loop
         for Feature of Set loop
            if not Result.Contains (Feature) then
               Result.Append (Feature);
            end if;
         end loop;
      end loop;

      return Result;
   end Flatten;

   ---------------
   -- To_String --
   ---------------
   function To_String (Set : Feature_Set) return String is
      use type Feature_Sets.Cursor;

      Buffer : Unbounded_String;
   begin
      for C in Set.Iterate loop
         Append (Buffer, To_String (Feature_Sets.Element (C)));

         if C /= Set.Last then
            Append (Buffer, ' ');
         end if;
      end loop;

      return To_String (Buffer);
   end To_String;

   ------------
   -- To_XML --
   ------------
   function To_XML (Set : Feature_Set) return String is
     ("<provide>" & To_String (Set) & "</provide>");

   function To_XML (List : Feature_Set_List) return String is
      Buffer : Unbounded_String;
   begin
      for Set of List loop
         Append (Buffer, To_XML (Set));
      end loop;

      return To_String (Buffer);
   end To_XML;

   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (DB : Feature_Database; XML : DOM.Core.Node) return Feature_Set
   is
      use DOM.Core;
      use Ada.Strings.Fixed;
      use Ada.Text_IO;

      Name : constant String := Nodes.Node_Name (XML);
      Text : constant String := Toolkit.XML.Get_Text (XML);
      Start_Index : Positive := Text'First;
      Space_Index : Natural;

      Result : Feature_Set;
   begin
      Put_Line (Text);

      if Name /= "provide" and
        Name /= "require" and
        Name /= "before" and
        Name /= "after" and
        Name /= "global"
      then
         raise Constraint_Error
           with "Unsupported node type " & Name;
      end if;

      Space_Index := Index (Text, " ", Start_Index);
      while Space_Index > Text'First loop
         Result.Append (To_Ada (DB, Text (Start_Index .. Space_Index - 1)));
         Start_Index := Space_Index + 1;
         Space_Index := Index (Text, " ", Start_Index);
      end loop;

      return Result;
   end To_Ada;

   function To_Ada
     (DB : Feature_Database; XML : DOM.Core.Node_List) return Feature_Set_List
   is
      use DOM.Core;

      Result : Feature_Set_List;
   begin
      for I in 1 .. Nodes.Length (XML) loop
         Result.Append (To_Ada (DB, Nodes.Item (XML, I - 1)));
      end loop;

      return Result;
   end To_Ada;

end Toolkit.Features;
