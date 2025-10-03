pragma Ada_2012;

package Toolkit.Texts is
   type Abstract_Text is private;
   type Text is private;

   function To_Ada (Input_Text : String) return Text;
private
   type Text is record
      Sentences : Toolkit.Sentences.Sentence_List;
      Features  : Toolkit.Features.Feature_Set;
   end record;
end Toolkit.Texts;
