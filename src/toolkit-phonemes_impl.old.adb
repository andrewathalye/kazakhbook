pragma Ada_2012;

with Ada.Strings.Hash;

with DOM.Core.Attrs;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with Toolkit.Strings;
with Toolkit.XML;

package body Toolkit.Phonemes_Impl is
   -------------
   -- Resolve --
   -------------
   function Resolve
     (AP : Abstract_Phoneme; Context : Contexts.Context)
      return Phoneme_Instance;
   --  Internal version that has a specific phoneme attached

   function Resolve
     (AP : Abstract_Phoneme; Context : Contexts.Context)
      return Phoneme_Instance
   is
      use Toolkit.Features;

      --  Retrieve a reference to the actual map
      type Opaque is null record;
      type Cursor_Mirror is record
         Container : access constant Phoneme_Maps.Map'Class;
         Node      : access constant Opaque;
         Position  : Natural;
      end record;

      Mirror_Within : Cursor_Mirror with
        Address => AP.Phoneme'Address, Import => True;

      L_Phones :
        Phone_List renames
        Phoneme_Maps.Constant_Reference
          (Mirror_Within.Container.all, AP.Phoneme)
          .Element.all;
   begin
      for C in L_Phones.Iterate loop
         declare
            L_Phone : Phone renames Phone_Lists.Element (C);
         begin
            --  If the phone has no contexts, it is good by default
            --  Otherwise check if any contexts match
            if not L_Phone.Contexts.Is_Empty and
              not Contexts.Has_Superset (Context, L_Phone.Contexts)
            then
               goto Next;
            end if;

            --  Check sounds
            --  The flattened set of features must be broader or equal to
            --  the required features
            --  TODO this might not be good enough for diphthongs
            if Superset (Flatten (L_Phone.Sounds), AP.Features) then
               return (Phoneme => AP.Phoneme, Instance => C);
            end if;
         end;
         <<Next>>
      end loop;

      raise Unknown_Phoneme
        with Toolkit.Features.To_XML (AP.Features) & " in " &
        Contexts.To_XML (Context) & " within " &
        String (Phoneme_Maps.Key (AP.Phoneme));
   end Resolve;

   function Resolve
     (PDB     : Phoneme_Database; AP : Abstract_Phoneme;
      Context : Contexts.Context) return Phoneme_Instance
   is
      use type Phoneme_Maps.Cursor;
   begin
      if AP.Phoneme /= Phoneme_Maps.No_Element then
         return Resolve (AP, Context);
      end if;

      --  Otherwise go through all phonemes and search
      for C in PDB.Iterate loop
         begin
            return Resolve (Abstract_Phoneme'(C, AP.Features), Context);
         exception
            when Unknown_Phoneme =>
               null;
         end;
      end loop;

      raise Unknown_Phoneme
        with Features.To_XML (AP.Features) & " in " &
        Contexts.To_XML (Context);
   end Resolve;

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

   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (FDB : Features.Feature_Database; PDB : Phoneme_Database; Text : String)
      return Abstract_Phoneme
   is
      use type Phoneme_Maps.Cursor;

      Required_Set     : Features.Feature_Set;
      Required_Phoneme : Phoneme_Maps.Cursor;

      Strings : constant Toolkit.Strings.Argument_List :=
        Toolkit.Strings.Split (Text);
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

      return (Required_Phoneme, Required_Set);
   end To_Ada;

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

   --------------
   -- Features --
   --------------
   function Dump_Features
     (AP : Abstract_Phoneme) return Toolkit.Features.Feature_Set is
     (AP.Features);
   function Dump_Features
     (PI : Phoneme_Instance) return Toolkit.Features.Feature_Set is
     (Features.Flatten (Phone_Lists.Element (PI.Instance).Sounds));

   ----------------
   -- Transcribe --
   ----------------
   function Transcribe (P : Phoneme_Instance) return String is
   begin
      return
        Ada.Strings.Unbounded.To_String (Phone_Lists.Element (P.Instance).IPA);
   end Transcribe;
end Toolkit.Phonemes_Impl;
