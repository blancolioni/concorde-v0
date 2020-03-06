with Nazar.Interfaces.Strings;

package Nazar.Models.Text is

   type Root_Text_Model is
     new Nazar_Model_Record
     and Nazar.Interfaces.Strings.String_Interface
   with private;

   type Nazar_Text_Model is access all Root_Text_Model'Class;

   function Text_Model
     (Default_Text  : String)
      return Nazar_Text_Model;

   function Current_Text
     (Model : Root_Text_Model)
      return String;

private

   type Root_Text_Model is
     new Nazar_Model_Record
     and Nazar.Interfaces.Strings.String_Interface with
      record
         Current : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Class_Name
     (Model : Root_Text_Model)
      return String
   is ("nazar-text-model");

   overriding function Get_String
     (Model  : Root_Text_Model)
      return String
   is (Root_Text_Model'Class (Model).Current_Text);

end Nazar.Models.Text;
