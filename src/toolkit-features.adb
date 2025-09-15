pragma Ada_2012;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with Toolkit.Strings;
with Toolkit.XML;

package body Toolkit.Features is

   ---------
   -- Add --
   ---------
   function Add (L, R : Feature_Set) return Feature_Set is
      Result : Feature_Set := L;
   begin
      for Feature of R loop
         if not Result.Contains (Feature) then
            Result.Append (Feature);
         end if;
      end loop;

      return Result;
   end Add;

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
         Result := Add (Result, Set);
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
      Text    : constant String := Toolkit.XML.Get_Text (XML);
      Strings : constant Toolkit.Strings.Argument_List :=
        Toolkit.Strings.Split (Text);

      Result : Feature_Set;
   begin
      for S of Strings loop
         Result.Append (To_Ada (DB, S));
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
