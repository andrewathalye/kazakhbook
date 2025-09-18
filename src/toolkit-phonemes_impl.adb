pragma Ada_2012;

with Ada.Strings.Hash;

with DOM.Core.Attrs;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;

with Toolkit.Strings;
with Toolkit.XML;

package body Toolkit.Phonemes_Impl is
   ------------------
   -- Get_Features --
   ------------------
   function Get_Features (PI : Phoneme_Instance) return Features.Feature_Set is
   begin
      return Features.Flatten (Phone_Lists.Element (PI.Instance).Sounds);
   end Get_Features;

   ------------------
   -- Get_Features --
   ------------------
   function Get_Features (AP : Abstract_Phoneme) return Features.Feature_Set is
   begin
      return AP.Features;
   end Get_Features;

   -------------
   -- Resolve --
   -------------
   function Resolve_Within
     (Cur : Contexts.Cursor'Class; FS : Features.Feature_Set;
      PMC : Phoneme_Maps.Cursor) return Phoneme_Instance;
   function Resolve_Within
     (Cur : Contexts.Cursor'Class; FS : Features.Feature_Set;
      PMC : Phoneme_Maps.Cursor) return Phoneme_Instance
   is
      -----------------------------------------------------------
      --                        Note:                          --
      -- Workaround for cursor-related bug due to bad aliasing --
      -----------------------------------------------------------
      type Opaque is null record;
      type Cursor_Mirror is record
         Container : access constant Phoneme_Maps.Map'Class;
         Node      : access constant Opaque;
         Position  : Natural;
      end record;

      Mirror_Within : Cursor_Mirror with
        Address => PMC'Address, Import => True;

      L_Phones :
        Phone_List renames
        Phoneme_Maps.Constant_Reference
          (Mirror_Within.Container.all, Phoneme_Maps.Key (PMC))
          .Element.all;
   begin
      for Phone_C in L_Phones.Iterate loop
         declare
            L_Phone : Phone renames Phone_Lists.Element (Phone_C);
         begin
            --  Check Contexts
            if L_Phone.Contexts.Is_Empty then
               goto Context_Success;
            end if;
            for Ctx of L_Phone.Contexts loop
               if Contexts.Applicable (Cur, Ctx) then
                  goto Context_Success;
               end if;
            end loop;
            goto Next;
            <<Context_Success>>
            --  Check Features
            if Features.Superset (Features.Flatten (L_Phone.Sounds), FS) then
               return (PMC, Phone_C);
            end if;
            <<Next>>
         end;
      end loop;

      raise Indeterminate_Phoneme;
   end Resolve_Within;

   function Resolve
     (PDB : Phoneme_Database; AP : Abstract_Phoneme;
      Cur : Contexts.Cursor'Class) return Phoneme_Instance
   is
   begin
      if Phoneme_Maps.Has_Element (AP.Phoneme) then
         return Resolve_Within (Cur, AP.Features, AP.Phoneme);
      else
         for PMC in PDB.Iterate loop
            begin
               return Resolve_Within (Cur, AP.Features, PMC);
            exception
               when Indeterminate_Phoneme =>
                  null;
            end;
         end loop;
      end if;

      raise Indeterminate_Phoneme;
   end Resolve;

   -----------------
   -- Abstractise --
   -----------------
   function Abstractise (Instance : Phoneme_Instance) return Abstract_Phoneme
   is
   begin
      return
        (Phoneme => Instance.Phoneme, Features => Get_Features (Instance));
   end Abstractise;

   ---------
   -- Add --
   ---------
   procedure Add (AP : in out Abstract_Phoneme; FS : Features.Feature_Set) is
   begin
      AP.Features := Features.Add (AP.Features, FS);
   end Add;

   --------------
   -- Subtract --
   --------------
   procedure Subtract (AP : in out Abstract_Phoneme; FS : Features.Feature_Set)
   is
   begin
      AP.Features := Features.Subtract (AP.Features, FS);
   end Subtract;

   ------------
   -- To_XML --
   ------------
   function To_XML (Instance : Phoneme_Instance) return String is
      use Ada.Strings.Unbounded;

      Buffer : Unbounded_String;
   begin
      Append
        (Buffer,
         "<phoneme id=" & ASCII.Quotation &
         String (Phoneme_Maps.Key (Instance.Phoneme)) & ASCII.Quotation & ">");

      Append (Buffer, "<phone>");

      Append
        (Buffer,
         Features.To_XML (Phone_Lists.Element (Instance.Instance).Sounds));

      Append (Buffer, "<ipa>");
      Append (Buffer, Phone_Lists.Element (Instance.Instance).IPA);
      Append (Buffer, "</ipa>");

      Append (Buffer, "</phone>");

      Append (Buffer, "</phoneme>");

      return To_String (Buffer);
   end To_XML;

   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (FDB : Features.Feature_Database; PDB : Phoneme_Database; Text : String)
      return Abstract_Phoneme
   is
      Required_Set     : Features.Feature_Set;
      Required_Phoneme : Phoneme_Maps.Cursor;

      Strings : constant Toolkit.Strings.Argument_List :=
        Toolkit.Strings.Split (Text);
   begin
      --  Process each requested feature or phoneme
      for S of Strings loop
         --  Handle phoneme references
         if S (S'First) = '@' then
            if S'Length = 1 then
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

      --  Determine a set of features common to all phones of the given phoneme
      if Phoneme_Maps.Has_Element (Required_Phoneme) then
         declare
            Declared_Features : Features.Feature_Set_List;
         begin
            for Phone_I of Phoneme_Maps.Element (Required_Phoneme) loop
               Declared_Features.Append (Features.Flatten (Phone_I.Sounds));
            end loop;

            Required_Set :=
              Features.Add (Required_Set, Features.Shared (Declared_Features));
         end;
      end if;

      return (Required_Phoneme, Required_Set);
   end To_Ada;

   ----------
   -- Read --
   ----------
   function Read_Phone
     (FDB     : Features.Feature_Database; CDB : Contexts.Context_Database;
      X_Phone : DOM.Core.Element) return Phone;
   function Read_Phone
     (FDB     : Features.Feature_Database; CDB : Contexts.Context_Database;
      X_Phone : DOM.Core.Element) return Phone
   is
      X_Contexts, X_Provides, X_IPA : DOM.Core.Node_List;
      Result                        : Phone;
   begin
      X_Contexts      :=
        DOM.Core.Elements.Get_Elements_By_Tag_Name (X_Phone, "context");
      Result.Contexts := Toolkit.Contexts.To_Ada (CDB, X_Contexts);
      DOM.Core.Free (X_Contexts);

      X_Provides    :=
        DOM.Core.Elements.Get_Elements_By_Tag_Name (X_Phone, "provide");
      Result.Sounds := Toolkit.Features.To_Ada (FDB, X_Provides);
      DOM.Core.Free (X_Provides);

      X_IPA := DOM.Core.Elements.Get_Elements_By_Tag_Name (X_Phone, "ipa");
      Result.IPA :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Toolkit.XML.Get_Text (DOM.Core.Nodes.Item (X_IPA, 0)));
      DOM.Core.Free (X_IPA);

      return Result;
   end Read_Phone;

   function Read_Phones
     (FDB       : Features.Feature_Database; CDB : Contexts.Context_Database;
      X_Phoneme : DOM.Core.Element) return Phone_List;
   function Read_Phones
     (FDB       : Features.Feature_Database; CDB : Contexts.Context_Database;
      X_Phoneme : DOM.Core.Element) return Phone_List
   is
      Result   : Phone_List;
      X_Phones : DOM.Core.Node_List;
      X_Phone  : DOM.Core.Element;
   begin
      X_Phones :=
        DOM.Core.Elements.Get_Elements_By_Tag_Name (X_Phoneme, "phone");

      --  If no phones are explicitly defined (there is only one realisation)
      if DOM.Core.Nodes.Length (X_Phones) = 0 then
         Result.Append (Read_Phone (FDB, CDB, X_Phoneme));
      else
         for I in 1 .. DOM.Core.Nodes.Length (X_Phones) loop
            X_Phone := DOM.Core.Nodes.Item (X_Phones, I - 1);
            Result.Append (Read_Phone (FDB, CDB, X_Phone));
         end loop;
      end if;

      DOM.Core.Free (X_Phones);
      return Result;
   end Read_Phones;

   procedure Read
     (Doc : DOM.Core.Document; FDB : Features.Feature_Database;
      CDB : Contexts.Context_Database; PDB : out Phoneme_Database)
   is
      X_Phonemes   : DOM.Core.Node_List;
      X_Phoneme    : DOM.Core.Element;
      X_Phoneme_ID : DOM.Core.Attr;
   begin
      PDB.Clear;

      X_Phonemes :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name (Doc, "phoneme");
      for X_Phoneme_Index in 1 .. DOM.Core.Nodes.Length (X_Phonemes) loop
         X_Phoneme    := DOM.Core.Nodes.Item (X_Phonemes, X_Phoneme_Index - 1);
         X_Phoneme_ID :=
           DOM.Core.Nodes.Get_Named_Item
             (DOM.Core.Nodes.Attributes (X_Phoneme), "id");

         PDB.Insert
           (Phoneme_Name (DOM.Core.Attrs.Value (X_Phoneme_ID)),
            Read_Phones (FDB, CDB, X_Phoneme));
      end loop;

      DOM.Core.Free (X_Phonemes);
   end Read;

   ----------------
   -- Transcribe --
   ----------------
   function Transcribe (P : Phoneme_Instance) return String is
   begin
      return
        Ada.Strings.Unbounded.To_String (Phone_Lists.Element (P.Instance).IPA);
   end Transcribe;

   ----------
   -- Hash --
   ----------
   function Hash (L : Phoneme_Name) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (L));
   end Hash;

end Toolkit.Phonemes_Impl;
