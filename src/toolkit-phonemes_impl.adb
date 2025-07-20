pragma Ada_2012;

with Ada.Strings.Hash;

with DOM.Core.Attrs;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with Toolkit.Strings;
with Toolkit.XML;

package body Toolkit.Phonemes_Impl is
   -----------------
   -- Resolve_Set --
   -----------------
   function Resolve_Set
     (Required_Set : Features.Feature_Set; Context : Contexts.Context;
      Within       : Phoneme_Maps.Cursor) return Phoneme_Instance;
   --  Private variant that allows specifying a phoneme

   function Resolve_Set
     (Required_Set : Features.Feature_Set; Context : Contexts.Context;
      Within       : Phoneme_Maps.Cursor) return Phoneme_Instance
   is
      use Toolkit.Contexts;
      use Toolkit.Features;

      --  Retrieve a reference to the actual map
      type Opaque is null record;
      type Cursor_Mirror is record
         Container : access constant Phoneme_Maps.Map'Class;
         Node      : access constant Opaque;
         Position  : Natural;
      end record;

      Mirror_Within : Cursor_Mirror with
        Address => Within'Address, Import => True;

      L_Phones :
        Phone_List renames
        Phoneme_Maps.Constant_Reference (Mirror_Within.Container.all, Within)
          .Element.all;
   begin
      for C in L_Phones.Iterate loop
         declare
            L_Phone : Phone renames Phone_Lists.Element (C);
         begin
            --  If the phone has no contexts (is default), go ahead
            if L_Phone.Contexts.Is_Empty then
               goto Found_Context;
            end if;

            --  Otherwise check contexts
            --  The required context must be broader or equal to
            --  one of the permissible contexts
            for L_Context of L_Phone.Contexts loop
               if Superset (Context, L_Context) then
                  goto Found_Context;
               end if;
            end loop;
            goto Next;

            <<Found_Context>>

            --  Check sounds
            --  The flattened set of features must be broader or equal to
            --  the required features
            if Superset (Flatten (L_Phone.Sounds), Required_Set) then
               return (Phoneme => Within, Instance => C);
            end if;
         end;
         <<Next>>
      end loop;

      raise Unknown_Phoneme
        with Features.To_XML (Required_Set) & " in " &
        Contexts.To_XML (Context) & " within " &
        String (Phoneme_Maps.Key (Within));
   end Resolve_Set;

   function Resolve_Set
     (DB      : Phoneme_Database; Required_Set : Features.Feature_Set;
      Context : Contexts.Context) return Phoneme_Instance
   is
   begin
      for C in DB.Iterate loop
         begin
            return Resolve_Set (Required_Set, Context, C);
         exception
            when Unknown_Phoneme =>
               null;
         end;
      end loop;

      raise Unknown_Phoneme
        with Features.To_XML (Required_Set) & " in " &
        Contexts.To_XML (Context);
   end Resolve_Set;

   ------------------
   -- Resolve_Text --
   ------------------
   function Resolve_Text
     (FDB         : Features.Feature_Database; PDB : Phoneme_Database;
      Description : String; Context : Toolkit.Contexts.Context)
      return Phoneme_Instance
   is
      use type Phoneme_Maps.Cursor;

      Required_Set     : Features.Feature_Set;
      Required_Phoneme : Phoneme_Maps.Cursor;

      Strings : constant Toolkit.Strings.Argument_List :=
        Toolkit.Strings.Split (Description);
   begin
      --  Process each requested feature or phoneme
      for S of Strings loop
         --  Handle phoneme references
         if S (S'First) = '@' then
            if S'Length = 1 or Required_Phoneme /= Phoneme_Maps.No_Element then
               raise Constraint_Error;
            end if;

            declare
               PN : constant Phoneme_Name :=
                 Phoneme_Name (S (S'First + 1 .. S'Last));
            begin
               if not PDB.Contains (PN) then
                  raise Unknown_Phoneme with String (PN);
               end if;

               Required_Phoneme := PDB.Find (PN);
            end;
         else
            Required_Set.Append (Features.To_Ada (FDB, S));
         end if;
      end loop;

      --  Resolve the set into a phoneme
      if Required_Phoneme = Phoneme_Maps.No_Element then
         return Resolve_Set (PDB, Required_Set, Context);
      else
         return Resolve_Set (Required_Set, Context, Required_Phoneme);
      end if;
   end Resolve_Text;

   ------------
   -- To_XML --
   ------------
   function To_XML (Instance : Phoneme_Instance) return String is
      use Ada.Strings.Unbounded;

      Buffer : Unbounded_String;
   begin
      Append (Buffer, "<phone>");

      --  TODO what about context?
      Append
        (Buffer,
         Features.To_XML (Phone_Lists.Element (Instance.Instance).Sounds));

      Append (Buffer, "<ipa>");
      Append (Buffer, Phone_Lists.Element (Instance.Instance).IPA);
      Append (Buffer, "</ipa>");

      Append (Buffer, "</phone>");

      return To_String (Buffer);
   end To_XML;

   ----------------
   -- Read_Phone --
   ----------------
   function Read_Phone
     (DB : Features.Feature_Database; XML : DOM.Core.Node) return Phone;

   function Read_Phone
     (DB : Features.Feature_Database; XML : DOM.Core.Node) return Phone
   is
      use DOM.Core;
      use Ada.Strings.Unbounded;

      L_Phone                       : Phone;
      X_Contexts, X_Features, X_IPA : Node_List;
   begin
      --  Note that this requires a <phoneme> containing context/provide/ipa
      if Nodes.Node_Name (XML) /= "phone" and
        Nodes.Node_Name (XML) /= "phoneme"
      then
         raise Constraint_Error;
      end if;

      X_Contexts := Elements.Get_Elements_By_Tag_Name (XML, "context");
      X_Features := Elements.Get_Elements_By_Tag_Name (XML, "provide");
      X_IPA      := Elements.Get_Elements_By_Tag_Name (XML, "ipa");

      L_Phone.Contexts := Contexts.To_Ada (DB, X_Contexts);
      L_Phone.Sounds   := Features.To_Ada (DB, X_Features);
      L_Phone.IPA      :=
        To_Unbounded_String (Toolkit.XML.Get_Text (Nodes.Item (X_IPA, 0)));

      Free (X_Contexts);
      Free (X_Features);
      Free (X_IPA);

      return L_Phone;
   end Read_Phone;

   function Read_Phones
     (FDB : Features.Feature_Database; XML : DOM.Core.Node) return Phone_List;
   function Read_Phones
     (FDB : Features.Feature_Database; XML : DOM.Core.Node) return Phone_List
   is
      use DOM.Core;

      L_Phones : Phone_List;
      X_Phone  : Node;
      X_Phones : Node_List := Elements.Get_Elements_By_Tag_Name (XML, "phone");
   begin
      --  Collect all data for a single phone and add to list
      for I in 1 .. Nodes.Length (X_Phones) loop
         X_Phone := Nodes.Item (X_Phones, I - 1);
         L_Phones.Append (Read_Phone (FDB, X_Phone));
      end loop;

      --  Use the implied <phone> if not are specified
      if Nodes.Length (X_Phones) = 0 then
         L_Phones.Append (Read_Phone (FDB, XML));
      end if;

      Free (X_Phones);

      return L_Phones;
   end Read_Phones;

   ----------
   -- Read --
   ----------
   procedure Read
     (Doc :     DOM.Core.Document; FDB : Toolkit.Features.Feature_Database;
      PDB : out Phoneme_Database)
   is
      use DOM.Core;

      X_Phoneme    : Node;
      X_Phonemes   : Node_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name (Doc, "phoneme");
      X_Phoneme_ID : Attr;
   begin
      PDB.Clear;

      Read_Phonemes :
      for I in 1 .. Nodes.Length (X_Phonemes) loop
         X_Phoneme    := Nodes.Item (X_Phonemes, I - 1);
         X_Phoneme_ID :=
           Nodes.Get_Named_Item (Nodes.Attributes (X_Phoneme), "id");

         PDB.Insert
           (Phoneme_Name (Attrs.Value (X_Phoneme_ID)),
            Read_Phones (FDB, X_Phoneme));
      end loop Read_Phonemes;

      Free (X_Phonemes);
   end Read;

   ----------
   -- Hash --
   ----------
   function Hash (L : Phoneme_Name) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (L));
   end Hash;
end Toolkit.Phonemes_Impl;
