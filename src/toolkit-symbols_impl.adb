pragma Ada_2012;

pragma Warnings (Off);

with Ada.Strings.Unbounded;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with Toolkit.XML;

package body Toolkit.Symbols_Impl is
   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (SDB : Symbol_Database; Text : String)
      return Abstract_Symbol
   is
      use Ada.Strings.Unbounded;
   begin
      Check_Symbols :
      for SC in SDB.Iterate loop
         Check_Forms :
         for FC in SDB (SC).Forms.Iterate loop
            declare
               F : Form renames SDB (FC).Forms (FC);
            begin
               if Text = To_String (F.Text) then
                  return (SC, FC);
               end if;
            end;
         end loop Check_Forms;
      end loop Check_Symbols;

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

   ----------
   -- Read --
   ----------
   procedure Read
     (Doc : DOM.Core.Document; FDB : Features.Feature_Database;
      PDB : Phonemes.Phoneme_Database; SDB : out Symbol_Database)
   is
      use DOM.Core;
      use Ada.Strings.Unbounded;

      X_Symbols : DOM.Core.Node_List;
      X_Symbol : DOM.Core.Element;
   begin
      SDB.Clear;

      X_Symbols := DOM.Core.Documents.Get_Elements_By_Tag_Name (Doc, "symbol");

      Read_Symbol :
      for I in 1 .. DOM.Core.Nodes.Length (X_Symbols) loop
         X_Symbol := DOM.Core.Nodes.Item (X_Symbols, I - 1);

         declare
            L_Symbol : Symbol_Definition;
            X_Complex, X_Majuscule, X_Minuscule,
            X_Unicase : DOM.Core.Node_List;
            X_Pronunciations : DOM.Core.Node_List;
            X_Provide : DOM.Core.Node_List;
         begin
            --  Read Forms
            X_Complex :=
              Elements.Get_Elements_By_Tag_Name (X_Symbol, "complex");
            X_Majuscule :=
              Elements.Get_Elements_By_Tag_Name (X_Symbol, "majuscule");
            X_Minuscule :=
              Elements.Get_Elements_By_Tag_Name (X_Symbol, "minuscule");

            for I in 1 .. Nodes.Length (X_Complex) loop
               declare
                  L_Form : Form;
                  X_Complex_Element : Node := Nodes.Item (X_Complex, I - 1);
                  X_Contexts : Node_List;
                  X_Unicode : Node_List;
               begin
                  X_Contexts :=
                    Elements.Get_Elements_By_Tag_Name
                      (X_Complex_Element, "context");
                  X_Unicode :=
                    Elements.Get_Elements_By_Tag_Name
                      (X_Complex_Element, "unicode");
                  L_Form :=
                    (Contexts.To_Ada (FDB, X_Contexts),
                     To_Unbounded_String
                       (Toolkit.XML.Get_Text (Nodes.Item (X_Unicode, 0))));
                  Free (X_Contexts);
                  Free (X_Unicode);

                  L_Symbol.Forms.Append (L_Form);
               end;
            end loop;

            --  TODO handle majuscule context
            for I in 1 .. Nodes.Length (X_Majuscule) loop
               L_Symbol.Forms.Append
                 (Form'
                    (Contexts => <>,
                     Text =>
                       To_Unbounded_String
                         (Toolkit.XML.Get_Text
                            (Nodes.Item (X_Majuscule, 0)))));
            end loop;

            --  TODO handle minuscule context
            for I in 1 .. Nodes.Length (X_Minuscule) loop
               L_Symbol.Forms.Append
                 (Form'
                    (Contexts => <>,
                     Text =>
                       To_Unbounded_String
                         (Toolkit.XML.Get_Text
                            (Nodes.Item (X_Minuscule, 0)))));
            end loop;

            for I in 1 .. Nodes.Length (X_Unicase) loop
               L_Symbol.Forms.Append
                 (Form'
                    (Contexts => <>,
                     Text =>
                       To_Unbounded_String
                         (Toolkit.XML.Get_Text (Nodes.Item (X_Unicase, 0)))));
            end loop;

            Free (X_Complex);
            Free (X_Majuscule);
            Free (X_Minuscule);
            Free (X_Unicase);

            --  Read pronunciations
            X_Pronunciations :=
              Elements.Get_Elements_By_Tag_Name (X_Symbol, "pronunciation");
            for I in 1 .. Nodes.Length (X_Pronunciations) loop
               declare
                  L_Pronunciation : Pronunciation;
                  X_Pronunciation : Node;
                  X_Contexts : Node_List;
                  X_Requires : Node_List;
               begin
                  X_Pronunciation := Nodes.Item (X_Pronunciations, I - 1);
                  X_Contexts :=
                    Elements.Get_Elements_By_Tag_Name
                      (X_Pronunciation, "context");
                  X_Requires :=
                    Elements.Get_Elements_By_Tag_Name
                      (X_Pronunciation, "require");
                  L_Pronunciation.Contexts :=
                    Contexts.To_Ada (FDB, X_Contexts);

                  for I in 1 .. Nodes.Length (X_Requires) loop
                     declare
                        L_AP : Phonemes.Abstract_Phoneme;
                        X_Require : Node;
                     begin
                        X_Require := Nodes.Item (X_Requires, I - 1);
                        L_Pronunciation.Abstract_Phonemes.Append
                          (Phonemes.To_Ada (FDB, PDB, X_Require));
                     end;
                  end loop;

                  Free (X_Contexts);
                  Free (X_Requires);

                  L_Symbol.Pronunciations.Append (L_Pronunciation);
               end;
            end loop;
            Free (X_Pronunciations);

            --  Provide
            X_Provide :=
              Elements.Get_Elements_By_Tag_Name (X_Symbol, "provide");
            for I in 1 .. Nodes.Length (X_Provide) loop
               L_Symbol.Features :=
                 Features.To_Ada (FDB, Nodes.Item (X_Provide, 0));
            end loop;
            Free (X_Provide);

            SDB.Append (L_Symbol);
         end;
      end loop Read_Symbol;

      Free (X_Symbols);
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
   function Resolve
     (PDB : Phonemes.Phoneme_Database; AS : Abstract_Symbol;
      Symbol_Context : Contexts.Context; Phoneme_Context : Contexts.Context)
      return Symbol_Instance
   is
      use Ada.Strings.Unbounded;

      SD : Symbol_Definition renames Symbol_Databases.Element (AS);
   begin
      for P of SD.Pronunciations loop
         if Contexts.Has_Superset (Symbol_Context, P.Contexts) then
            return
              (AS.Symbol, AS.Form,
               Phonemes.Resolve (PDB, P.Abstract_Phonemes, Phoneme_Context));
         end if;
      end loop;

      raise Indeterminate_Symbol
        with To_String (Form_Lists.Element (AS.Form).Text) & " in " &
        Contexts.To_XML (Symbol_Context);
   end Resolve;

end Toolkit.Symbols_Impl;
