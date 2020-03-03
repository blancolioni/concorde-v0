with Nazar.Models.Console;
with Nazar.Models.Text_Writer;

with Nazar.Json;
with Nazar.Sessions;
with Nazar.Views.Console;

package Nazar.Web_UI.Views.Web_Console is

   type Root_Web_Console_View is
     new Root_AWS_View_Type
     and Nazar.Views.Console.Console_View_Interface
   with private;

   type Nazar_Web_Console_View is access all Root_Web_Console_View'Class;

   function Web_Console_View
     (Model : not null access Nazar.Models.Console.Root_Console_Model'Class)
      return Nazar_Web_Console_View;

private

   type Root_Web_Console_View is
     new Root_AWS_View_Type
     and Nazar.Views.Console.Console_View_Interface with
      record
         Last_Line : Nazar.Models.Text_Writer.Line_Cursor;
      end record;

   overriding procedure Show
     (View : in out Root_Web_Console_View);

   overriding procedure Model_Changed
     (View : in out Root_Web_Console_View);

   overriding function Handle_Get
     (Handler    : Root_Web_Console_View;
      Session    : Nazar.Sessions.Nazar_Session;
      Parameters : Nazar.Web_UI.Routes.Parameter_Container'Class)
      return Nazar.Json.Json_Value'Class;

   overriding function Handle_Post
     (Handler    : Root_Web_Console_View;
      Session    : Nazar.Sessions.Nazar_Session;
      Parameters : Nazar.Web_UI.Routes.Parameter_Container'Class)
      return Nazar.Json.Json_Value'Class;

   type Model_Access is
     access all Nazar.Models.Console.Root_Console_Model'Class;

   function Console_Model
     (View : Root_Web_Console_View'Class)
      return Model_Access
   is (Model_Access (View.Model));

end Nazar.Web_UI.Views.Web_Console;
