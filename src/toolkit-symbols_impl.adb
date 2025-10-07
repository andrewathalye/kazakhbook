pragma Ada_2022;

with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;

with Toolkit.Contexts_Impl;
with Toolkit.XML;
with Toolkit.Log; use Toolkit.Log;

package body Toolkit.Symbols_Impl is

   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (SDB : Symbol_Database; Text : String) return Abstract_Symbol
   is
      use Ada.Strings.Unbounded;
   begin
      for Symbol_C in SDB.Iterate loop
         declare
            Symbol_CR    : constant Symbol_Databases.Constant_Reference_Type :=
              Symbol_Databases.Constant_Reference
                (Symbol_Databases.Vector (SDB), Symbol_C);
            Symbol_Forms : Form_List renames Symbol_CR.Forms;
         begin
            for Form_C in Symbol_Forms.Iterate loop
               --  TODO unicode normalisation!
               if To_String (Form_Lists.Element (Form_C).Text) = Text then
                  return (Symbol_C, Form_C);
               end if;
            end loop;

         end;
      end loop;

      raise Unknown_Symbol with Text;
   end To_Ada;

   ----------------
   -- To_Unicode --
   ----------------
   function To_Unicode (S : Symbol_Instance) return String is
      use Ada.Strings.Unbounded;
   begin
      return To_String (Form_Lists.Element (S.Form).Text);
   end To_Unicode;

   function To_Unicode (S : Abstract_Symbol) return String is
      use Ada.Strings.Unbounded;
   begin
      return To_String (Form_Lists.Element (S.Form).Text);
   end To_Unicode;

   ----------
   -- Read --
   ----------
   procedure Read
     (Doc :     DOM.Core.Document; FDB : Features.Feature_Database;
      CDB :     Contexts.Context_Database; PDB : Phonemes.Phoneme_Database;
      SDB : out Symbol_Database)
   is
      use Ada.Strings.Unbounded;
      X_Symbols : DOM.Core.Node_List;
   begin
      X_Symbols := DOM.Core.Documents.Get_Elements_By_Tag_Name (Doc, "symbol");

      --  Handle each Symbol
      for X_Symbol_Index in 1 .. DOM.Core.Nodes.Length (X_Symbols) loop
         declare
            X_Symbol          : constant DOM.Core.Node      :=
              DOM.Core.Nodes.Item (X_Symbols, X_Symbol_Index - 1);
            X_Symbol_Children : constant DOM.Core.Node_List :=
              DOM.Core.Nodes.Child_Nodes (X_Symbol);
            SD                : Symbol_Definition;
         begin
            --  Iterate over children
            for X_Symbol_Child_Index in
              1 .. DOM.Core.Nodes.Length (X_Symbol_Children)
            loop
               declare
                  X_Symbol_Child : constant DOM.Core.Node :=
                    DOM.Core.Nodes.Item
                      (X_Symbol_Children, X_Symbol_Child_Index - 1);
               begin
                  --  Handle all the different types of children
                  if X_Symbol_Child in DOM.Core.Element then
                     --  TODO implement contexts for majuscule and minuscule
                     if DOM.Core.Nodes.Node_Name (X_Symbol_Child) =
                       "majuscule" or
                       DOM.Core.Nodes.Node_Name (X_Symbol_Child) =
                         "minuscule" or
                       DOM.Core.Nodes.Node_Name (X_Symbol_Child) = "unicase"
                     then
                        SD.Forms.Append
                          (Form'
                             (Contexts => <>,
                              Text     =>
                                To_Unbounded_String
                                  (Toolkit.XML.Get_Text (X_Symbol_Child))));
                     elsif DOM.Core.Nodes.Node_Name (X_Symbol_Child) =
                       "complex"
                     then
                        declare
                           X_Contexts : DOM.Core.Node_List;
                           New_Form   : Form;
                        begin
                           X_Contexts        :=
                             DOM.Core.Elements.Get_Elements_By_Tag_Name
                               (X_Symbol_Child, "context");
                           New_Form.Contexts :=
                             Contexts.To_Ada (CDB, X_Contexts);
                           New_Form.Text     :=
                             To_Unbounded_String
                               (Toolkit.XML.Get_Text
                                  (DOM.Core.Nodes.Last_Child
                                     (X_Symbol_Child)));
                           DOM.Core.Free (X_Contexts);
                           SD.Forms.Append (New_Form);
                        end;
                     elsif DOM.Core.Nodes.Node_Name (X_Symbol_Child) =
                       "pronunciation"
                     then
                        declare
                           X_Contexts        : DOM.Core.Node_List;
                           X_Requires        : DOM.Core.Node_List;
                           New_Pronunciation : Pronunciation;
                        begin
                           X_Contexts :=
                             DOM.Core.Elements.Get_Elements_By_Tag_Name
                               (X_Symbol_Child, "context");
                           X_Requires :=
                             DOM.Core.Elements.Get_Elements_By_Tag_Name
                               (X_Symbol_Child, "require");

                           New_Pronunciation.Contexts :=
                             Contexts.To_Ada (CDB, X_Contexts);

                           for X_Require_Index in
                             1 .. DOM.Core.Nodes.Length (X_Requires)
                           loop
                              New_Pronunciation.Abstract_Phonemes.Append
                                (Phonemes.To_Ada
                                   (FDB, PDB,
                                    DOM.Core.Nodes.Item
                                      (X_Requires, X_Require_Index - 1)));
                           end loop;

                           DOM.Core.Free (X_Contexts);
                           DOM.Core.Free (X_Requires);
                           SD.Pronunciations.Append (New_Pronunciation);
                        end;
                     elsif DOM.Core.Nodes.Node_Name (X_Symbol_Child) =
                       "provide"
                     then
                        SD.Features.Append
                          (Features.To_Ada (FDB, X_Symbol_Child));
                     else --  UNKNOWN CHILD (error)
                        raise Program_Error
                          with DOM.Core.Nodes.Node_Name (X_Symbol_Child);
                     end if;
                  end if;
               end;
            end loop;
            SDB.Append (SD);
         end;
      end loop;

      DOM.Core.Free (X_Symbols);
   end Read;

   -----------------
   -- To_Phonemes --
   -----------------
   function To_Phonemes (X : Symbol_Instance) return Phonemes.Phoneme_List is
   begin
      return X.Phonemes;
   end To_Phonemes;

   -------------
   -- Resolve --
   -------------
   function Image (P : Pronunciation) return String;
   function Image (P : Pronunciation) return String is
      use Ada.Strings.Unbounded;
      Buf : Unbounded_String;
   begin
      for Ctx of P.Contexts loop
         Append (Buf, Contexts.To_XML (Ctx));
         Append (Buf, ASCII.LF);
      end loop;

      for AP of P.Abstract_Phonemes loop
         Append (Buf, Features.To_String (Phonemes.Get_Features (AP)));
         Append (Buf, ASCII.LF);
      end loop;

      return To_String (Buf);
   end Image;

   function Resolve
     (PDB : Phonemes.Phoneme_Database; AS : Abstract_Symbol;
      Cur : Contexts.Cursor'Class) return Symbol_Instance
   is
      use Ada.Strings.Unbounded;

      Available_Pronunciations : constant Pronunciation_List :=
        Symbol_Databases.Element (AS.Symbol).Pronunciations;
   begin
      Put_Log
        (Log.Symbols,
         "RESOLVE_SINGLE " & To_String (Form_Lists.Element (AS.Form).Text));

      for P of Available_Pronunciations loop
         Put_Log (Log.Symbols, "Try Pronunciation " & Image (P));
         if P.Contexts.Is_Empty then
            goto Context_Found;
         else
            for Ctx of P.Contexts loop
               if Contexts.Applicable (Cur, Ctx) then
                  goto Context_Found;
               end if;
            end loop;
         end if;
         goto Next;

         <<Context_Found>>
         --  TODO ?? maybe this Cur isn’t right
         return SI : Symbol_Instance do
            SI.Symbol   := AS.Symbol;
            SI.Form     := AS.Form;
            SI.Phonemes :=
              Phonemes.Resolve
                (PDB, P.Abstract_Phonemes,
                 Contexts.Rescope
                   (Cur, Contexts_Impl.Phoneme, Contexts_Impl.First));
         end return;
         <<Next>>
      end loop;
      raise Indeterminate_Symbol with "No pronunciation found";
   end Resolve;

   --------------------------
   -- Interop for Generics --
   --------------------------
   function Dump_Features (AS : Abstract_Symbol) return Features.Feature_Set is
   begin
      return Symbol_Databases.Element (AS.Symbol).Features;
   end Dump_Features;

   function Dump_Features (SI : Symbol_Instance) return Features.Feature_Set is
   begin
      return Symbol_Databases.Element (SI.Symbol).Features;
   end Dump_Features;

   function Get_Child (AS : Abstract_Symbol) return Contexts.Cursor'Class is
      use type Ada.Containers.Count_Type;
   begin
      --  TODO can we do better?
      if Symbol_Databases.Element (AS.Symbol).Pronunciations.Length = 1 then
         declare
            Possible_Pronunciation :
              Pronunciation renames
              Symbol_Databases.Element (AS.Symbol).Pronunciations
                .First_Element;
         begin
            if Possible_Pronunciation.Contexts.Is_Empty then
               Put_Log (Log.Symbols, "Returning sole candidate for AS Child");
               return
                 Phonemes.To_Cursor
                   (Possible_Pronunciation.Abstract_Phonemes.First);
            end if;
         end;
      end if;

      Put_Log (Log.Symbols, "Return No Element");
      return Phonemes.To_Cursor (Phonemes.Abstract_Phoneme_Lists.No_Element);
   end Get_Child;

   function Get_Child (SI : Symbol_Instance) return Contexts.Cursor'Class is
   begin
      return Phonemes.To_Cursor (SI.Phonemes.First);
   end Get_Child;

end Toolkit.Symbols_Impl;
