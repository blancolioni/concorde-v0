with Nazar.Models.Console;
with Nazar.Models.Text_Writer;
with Nazar.Views.Console;

package Nazar.Views.Text_Console is

   type Root_Text_Console_View is
     new Root_View_Type
     and Nazar.Views.Console.Console_View_Interface
   with private;

   type Nazar_Text_Console_View is access all Root_Text_Console_View'Class;

   function Text_Console_View
     (Model : not null access Nazar.Models.Console.Root_Console_Model'Class)
      return Nazar_Text_Console_View;

private

   type Root_Text_Console_View is
     new Root_View_Type
     and Nazar.Views.Console.Console_View_Interface with
      record
         Last_Line : Nazar.Models.Text_Writer.Line_Cursor;
      end record;

   overriding procedure Show
     (View : in out Root_Text_Console_View);

   overriding procedure Model_Changed
     (View : in out Root_Text_Console_View);

   type Model_Access is
     access all Nazar.Models.Console.Root_Console_Model'Class;

   function Console_Model
     (View : Root_Text_Console_View'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Text_Console;
