pragma Ada_2012;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;

package body Toolkit.Contexts is
   --------------------
   -- Derive_Context --
   --------------------
   function Derive_Context
     (Cursor           : Lists.Cursor;
      External_Context : Contexts.Context) return Contexts.Context
   is
      use type Lists.Cursor;

      Before, After : Toolkit.Features.Feature_Set;
   begin
      if Lists.Previous (Cursor) =
        Lists.No_Element
      then
         Before := External_Context.Before;
      else
         Before :=
           Features
             (Lists.Element
                (Lists.Previous (Cursor)));
      end if;

      if Lists.Next (Cursor) =
        Lists.No_Element
      then
         After := External_Context.After;
      else
         After :=
           Features
             (Lists.Element
                (Lists.Next (Cursor)));
      end if;

      return (Before, After, External_Context.Global);
   end Derive_Context;

   --------------
   -- Superset --
   --------------
   function Superset (L, R : Context) return Boolean is
   begin
      if Features.Superset (L.After, R.After) and
        Features.Superset (L.Before, R.Before) and
        Features.Superset (L.Global, R.Global)
      then
         return True;
      end if;

      return False;
   end Superset;

   ------------------
   -- Has_Superset --
   ------------------
   function Has_Superset (L : Context; R : Context_List) return Boolean is
   begin
      for R_x of R loop
         if Superset (L, R_x) then
            return True;
         end if;
      end loop;

      return False;
   end Has_Superset;

   ------------
   -- To_XML --
   ------------
   function To_XML (L : Context) return String is
      Buffer : Unbounded_String;
   begin
      Append (Buffer, "<context>");

      if not L.Before.Is_Empty then
         Append (Buffer, "<before>");
         Append (Buffer, Features.To_String (L.Before));
         Append (Buffer, "</before>");
      end if;

      if not L.After.Is_Empty then
         Append (Buffer, "<after>");
         Append (Buffer, Features.To_String (L.After));
         Append (Buffer, "</after>");
      end if;

      if not L.Global.Is_Empty then
         Append (Buffer, "<global>");
         Append (Buffer, Features.To_String (L.Global));
         Append (Buffer, "</global>");
      end if;

      Append (Buffer, "</context>");

      return To_String (Buffer);
   end To_XML;

   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (DB : Features.Feature_Database; XML : DOM.Core.Node) return Context
   is
      use DOM.Core;

      Result : Context;

      X_Before, X_After, X_Global : Node_List;
   begin
      if Nodes.Node_Name (XML) /= "context" then
         raise Constraint_Error;
      end if;

      X_Before := Elements.Get_Elements_By_Tag_Name (XML, "before");
      X_After  := Elements.Get_Elements_By_Tag_Name (XML, "after");
      X_Global := Elements.Get_Elements_By_Tag_Name (XML, "global");

      if Nodes.Length (X_Before) = 1 then
         Result.Before := Features.To_Ada (DB, Nodes.Item (X_Before, 0));
      end if;

      if Nodes.Length (X_After) = 1 then
         Result.After  := Features.To_Ada (DB, Nodes.Item (X_After, 0));
      end if;

      if Nodes.Length (X_Global) = 1 then
         Result.Global := Features.To_Ada (DB, Nodes.Item (X_Global, 0));
      end if;

      Free (X_Before);
      Free (X_After);
      Free (X_Global);

      return Result;
   end To_Ada;

   function To_Ada
     (DB : Features.Feature_Database; XML : DOM.Core.Node_List)
      return Context_List
   is
      use DOM.Core;

      Result : Context_List;
   begin
      for I in 1 .. Nodes.Length (XML) loop
         Result.Append (To_Ada (DB, Nodes.Item (XML, I - 1)));
      end loop;

      return Result;
   end To_Ada;

end Toolkit.Contexts;
