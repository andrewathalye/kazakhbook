pragma Ada_2012;

with Ada.Streams;
with Ada.Strings.Unbounded;

with DOM.Core.Nodes;

package body Toolkit.XML is

   ---------------
   -- To_String --
   ---------------
   function To_String (XML : DOM.Core.Node) return String is
      use Ada.Streams;
      use Ada.Strings.Unbounded;
      use DOM.Core;

      type SST is new Root_Stream_Type with record
         Str : Unbounded_String;
         Idx : Natural := 1;
      end record;

      procedure Read
        (Stream : in out SST; Item : out Stream_Element_Array;
         Last   :    out Stream_Element_Offset) is null;

      procedure Write (Stream : in out SST; Item : Stream_Element_Array);
      procedure Write (Stream : in out SST; Item : Stream_Element_Array) is
         Str : String (1 .. Integer (Item'Length));
         S   : Integer := Str'First;
      begin
         for J in Item'Range loop
            Str (S) := Character'Val (Item (J));
            S       := S + 1;
         end loop;
         Append (Stream.Str, Str);
      end Write;

      S : aliased SST;
   begin
      Nodes.Write (S'Access, XML);
      return To_String (S.Str);
   end To_String;

   --------------
   -- Get_Text --
   --------------
   function Get_Text (N : DOM.Core.Node) return String is
      use DOM.Core;
      use Ada.Strings.Unbounded;

      Children : constant Node_List := Nodes.Child_Nodes (N);
      Buffer   : Unbounded_String;
   begin
      for I in 1 .. Nodes.Length (Children) loop
         if Nodes.Node_Type (Nodes.Item (Children, I - 1)) = Text_Node then
            Append (Buffer, Nodes.Node_Value (Nodes.Item (Children, I - 1)));
         end if;
      end loop;

      return To_String (Buffer);
   end Get_Text;

end Toolkit.XML;
